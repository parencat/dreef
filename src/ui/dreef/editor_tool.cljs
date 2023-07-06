(ns dreef.editor-tool
  (:require
   [shadow.cljs.modern :refer (defclass)]
   [applied-science.js-interop :as j]
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [dreef.state :refer [emit! subscribe]]
   [dreef.styles :refer [colors]]
   [dreef.script :as script]
   ["ui-box" :default box]
   ["codemirror" :refer [basicSetup]]
   ["@codemirror/state" :refer [EditorState]]
   ["@codemirror/view" :refer [EditorView ViewPlugin Decoration keymap lineNumbers highlightActiveLineGutter
                               highlightSpecialChars drawSelection dropCursor rectangularSelection
                               crosshairCursor]]
   ["@codemirror/language" :refer [foldKeymap foldGutter indentOnInput
                                   bracketMatching syntaxHighlighting
                                   HighlightStyle defaultHighlightStyle]]
   ["@codemirror/lang-sql" :refer [sql]]
   ["@codemirror/autocomplete" :refer [closeBracketsKeymap completionKeymap closeBrackets autocompletion]]
   ["@codemirror/commands" :refer [defaultKeymap historyKeymap history]]
   ["@codemirror/search" :refer [searchKeymap highlightSelectionMatches]]
   ["@codemirror/lint" :refer [lintKeymap]]
   ["@lezer/highlight" :refer [tags]]))


(def cm-keymap
  (let [keybindings #js []]
    (doseq [k [closeBracketsKeymap defaultKeymap searchKeymap
               historyKeymap foldKeymap completionKeymap lintKeymap]]
      (j/apply keybindings :push k))

    (j/call keymap :of keybindings)))


(def nord-theme
  (j/call EditorView :theme
          (clj->js
           {"&"
            {:color           (:snow0 colors)
             :backgroundColor (:polar1 colors)}

            ".cm-content"
            {:caretColor  (:snow0 colors)
             :font-family "'Fira Code', monospace"}

            "&.cm-focused .cm-cursor"
            {:borderLeftColor (:snow0 colors)}

            "&.cm-focused .cm-selectionBackground,
             &.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground,
             .cm-selectionBackground,
             .cm-content ::selection"
            {:backgroundColor (:polar3 colors)}

            ".cm-panels"
            {:backgroundColor (:polar1 colors)
             :color           (:snow0 colors)}

            ".cm-panels.cm-panels-top"
            {:borderBottom "2px solid black"}

            ".cm-panels.cm-panels-bottom"
            {:borderTop "2px solid black"}

            ".cm-searchMatch"
            {:backgroundColor (:frost4 colors)
             :outline         "1px solid #457DFF"}

            ".cm-searchMatch.cm-searchMatch-selected"
            {:backgroundColor (:frost5 colors)}

            ".cm-activeLine"
            {:backgroundColor (:polar0-0.4 colors)}

            ".cm-selectionMatch"
            {:backgroundColor (:polar3 colors)}

            ".cm-matchingBracket,
             .cm-nonmatchingBracket"
            {:backgroundColor (:polar4 colors)
             :outline         "1px solid #515a6b"}

            ".cm-gutters"
            {:backgroundColor (:polar1 colors)
             :color           (:polar4 colors)
             :border          "none"}

            ".cm-activeLineGutter"
            {:backgroundColor (:polar5 colors)}

            ".cm-foldPlaceholder"
            {:backgroundColor "transparent"
             :border          "none"
             :color           "#ddd"}

            ".cm-tooltip"
            {:border          "1px solid #181a1f"
             :backgroundColor (:polar2 colors)}

            ".cm-tooltip-autocomplete"
            {"& > ul > li[aria-selected]"
             {:backgroundColor (:polar6 colors)
              :color           (:snow0 colors)}}})
          #js {:dark true}))


(def nord-highlight-style
  (j/call HighlightStyle :define
          (clj->js
           [{:tag   [(j/get tags :keyword)]
             :color (:frost2 colors)}

            {:tag   [(j/get tags :operator)]
             :color (:frost1 colors)}

            {:tag   [(j/get tags :name)
                     (j/get tags :deleted)
                     (j/get tags :character)
                     (j/get tags :propertyName)
                     (j/get tags :macroName)]
             :color (:snow0 colors)}

            {:tag   [(j/call tags :function (j/get tags :variableName))
                     (j/get tags :labelName)]
             :color (:frost1 colors)}

            {:tag   [(j/get tags :color)
                     (j/call tags :constant (j/get tags :name))
                     (j/call tags :standard (j/get tags :name))]
             :color (:snow0 colors)}

            {:tag   [(j/call tags :definition (j/get tags :name))
                     (j/get tags :separator)]
             :color (:snow0 colors)}

            {:tag   [(j/get tags :typeName)
                     (j/get tags :className)
                     (j/get tags :number)
                     (j/get tags :changed)
                     (j/get tags :annotation)
                     (j/get tags :modifier)
                     (j/get tags :self)
                     (j/get tags :namespace)]
             :color (:aurora1 colors)}

            {:tag   [(j/get tags :operatorKeyword)
                     (j/get tags :url)
                     (j/get tags :escape)
                     (j/get tags :regexp)
                     (j/get tags :link)]
             :color (:aurora4 colors)}

            {:tag   [(j/get tags :meta)
                     (j/get tags :comment)]
             :color (:polar6 colors)}

            {:tag        (j/get tags :strong)
             :fontWeight "bold"}

            {:tag       (j/get tags :emphasis)
             :fontStyle "italic"}

            {:tag            (j/get tags :strikethrough)
             :textDecoration "line-through"}

            {:tag            (j/get tags :link)
             :color          (:aurora4 colors)
             :textDecoration "underline"}

            {:tag        (j/get tags :heading)
             :fontWeight "bold"
             :color      (:frost0 colors)}

            {:tag   [(j/get tags :atom)
                     (j/get tags :bool)
                     (j/call tags :special (j/get tags :variableName))]
             :color (:frost3 colors)}

            {:tag   [(j/get tags :processingInstruction)
                     (j/get tags :string)
                     (j/call tags :special (j/get tags :string))
                     (j/get tags :inserted)]
             :color (:aurora3 colors)}

            {:tag   (j/get tags :invalid)
             :color (:aurora0 colors)}])))


(def line-deco
  (j/call Decoration :line #js {:class "cm-activeLine"}))


(defn get-decorations [view]
  (->> (j/get-in view [:state :selection :ranges])
       (reduce (fn [{:keys [last-line-start decorations] :as acc} range]
                 (let [line      (j/call view :lineBlockAt (j/get range :head))
                       line-from (j/get line :from)]
                   (if (> line-from last-line-start)
                     {:decorations     (j/push! decorations (j/call line-deco :range line-from))
                      :last-line-start line-from}
                     acc)))
               {:last-line-start -1
                :decorations     #js []})
       :decorations))


(defn decorate? [view]
  (->> (j/get-in view [:state :selection :ranges])
       (not-any? (fn [range]
                   (not (j/get range :empty))))))


(defclass LineHighlighter
  (constructor [this view]
    (j/assoc! this :decorations (j/call this :getDeco view)))

  Object
  (update [this update]
    (when (or (j/get update :docChanged)
              (j/get update :selectionSet))
      (j/assoc! this :decorations (j/call this :getDeco (j/get update :view)))))

  (getDeco [this view]
    (if (decorate? view)
      (let [deco (get-decorations view)]
        (j/call Decoration :set deco))
      (j/get Decoration :none))))


(def highlight-active-line
  (j/call ViewPlugin :fromClass
          LineHighlighter
          #js {:decorations (fn [v] (j/get v :decorations))}))


(def extensions
  #js [(lineNumbers)
       (highlightActiveLineGutter)
       (highlightSpecialChars)
       (history)
       (foldGutter)
       (drawSelection)
       (dropCursor)
       (j/call-in EditorState [:allowMultipleSelections :of] true)
       (indentOnInput)
       (bracketMatching)
       (closeBrackets)
       (autocompletion)
       (rectangularSelection)
       (crosshairCursor)
       highlight-active-line
       (highlightSelectionMatches)
       cm-keymap
       nord-theme
       (syntaxHighlighting nord-highlight-style)
       (sql)])


(defn add-editor-evt [{:keys [id script-id]}]
  (ptk/reify ::add-editor
    ptk/UpdateEvent
    (update [_ state]
      (->> state
           (assoc-in [:editor id] {:id id :script script-id})
           (assoc :active-editor id)))))


(defn update-editor-evt [{:keys [id script]}]
  (ptk/reify ::update-editor
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:editor id] merge {:script script}))))


(defn save-script-state [script-id state]
  (emit! (script/save-script-state-evt
          {:script-id script-id
           :state     (j/call state :toJSON)})))


(mf/defc editor [{:keys [id]}]
  (let [container-ref (mf/use-ref)
        editor-state  (mf/use-state)

        {script-id :id :as script}
        (mf/deref (subscribe
                   #(->> (get-in % [:editor id :script])
                         (vector :script)
                         (get-in %))))]

    (mf/use-effect
     (mf/deps script-id)
     (fn []
       (let [container    (j/get container-ref :current)
             script-state (:state script)
             state        (if (some? script-state)
                            (j/call EditorState :fromJSON script-state)
                            (j/call EditorState :create
                                    #js {:doc        (or (:text script) "")
                                         :extensions extensions}))
             editor       (new EditorView #js {:state  state
                                               :parent container})]
         (reset! editor-state state)
         (fn editor-unmount []
           (save-script-state script-id state)
           (j/call editor :destroy)))))

    [:> box {:ref        container-ref
             :min-height "100%"}]))


