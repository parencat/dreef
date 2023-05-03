(ns dreef.layout
  (:require
   [rumext.v2 :as mf]
   [applied-science.js-interop :as j]
   [dreef.state :refer [state subscribe]]
   [dreef.styles :refer [colors]]
   [dreef.editor-tool :refer [editor]]
   ["ui-box" :default box]))


(defn vertical-pane? [pane-side]
  (or (= pane-side :left)
      (= pane-side :right)))


(mf/defc pane [{:keys [pane pane-side]}]
  (let [pane-props (mf/deref (subscribe [:layout pane]))
        vertical?  (vertical-pane? pane-side)]
    [:> box {:min-width  (when vertical? 200)
             :min-height (when-not vertical? 200)
             :overflow   "scroll"
             :flex       "0 0"
             :flex-basis "auto"
             :style      {:width  (when vertical? (get pane-props :width 300))
                          :height (when-not vertical? (get pane-props :height 300))}}]))


(defn mouse-track [{:keys [pane pane-side]} event]
  (j/call event :preventDefault)
  (let [body      (j/get js/document :body)
        vertical? (vertical-pane? pane-side)
        prop      (if vertical? :width :height)
        prop-val  (case pane-side
                    :left (j/get event :pageX)
                    :right (- (j/get body :clientWidth) (j/get event :pageX))
                    :bottom (- (j/get body :clientHeight) (j/get event :pageY)))]
    (swap! state update-in [:layout pane] assoc prop prop-val)))


(defn gutter-mouse-down [pane-props]
  (let [abort-controller (new js/AbortController)
        abort-signal     (j/get abort-controller :signal)
        body             (j/get js/document :body)]
    (j/call body :addEventListener "mousemove"
      #(mouse-track pane-props %) #js {:signal abort-signal})
    (j/call body :addEventListener "mouseup"
      #(j/call abort-controller :abort) #js {:signal abort-signal})))


(mf/defc gutter [{:keys [pane-side] :as pane-props}]
  (let [vertical?   (vertical-pane? pane-side)
        horizontal? (not vertical?)]
    [:> box {:on-mouse-down    #(gutter-mouse-down pane-props)
             :width            (when vertical? "2px")
             :height           (when horizontal? "2px")
             :background-color (:polar1 colors)
             :cursor           (if vertical? "col-resize" "row-resize")
             :position         "relative"
             :selectors        {:&:before
                                {:content   "''"
                                 :position  "absolute"
                                 :width     (if vertical? "10px" "100%")
                                 :height    (if vertical? "100%" "10px")
                                 :left      (when vertical? "50%")
                                 :top       (when horizontal? "50%")
                                 :transform (if vertical?
                                              "translateX(-50%)"
                                              "translateY(-50%)")}}}]))


(def layout-state
  (subscribe :layout))


(def layout-panes-state
  (subscribe
   {:left   [:left-pane :visible]
    :right  [:right-pane :visible]
    :bottom [:bottom-pane :visible]}
   layout-state))


(mf/defc layout-manager []
  (let [{:keys [left right bottom]} (mf/deref layout-panes-state)]
    [:> box {:height         "100%"
             :display        "flex"
             :flex-direction "column"}
     [:> box {:flex-grow "3"
              :display   "flex"}

      (when left
        [:*
         [:& pane {:pane-side :left
                   :pane      :left-pane}]
         [:& gutter {:pane-side :left
                     :pane      :left-pane}]])

      ;; main
      [:> box {:is         :main
               :flex-grow  "2"
               :min-height 200
               :min-width  200}
       [:& editor]]

      (when right
        [:*
         [:& gutter {:pane-side :right
                     :pane      :right-pane}]
         [:& pane {:pane-side :right
                   :pane      :right-pane}]])]

     (when bottom
       [:*
        [:& gutter {:pane-side :bottom
                    :pane      :bottom-pane}]
        [:& pane {:pane-side :bottom
                  :pane      :bottom-pane}]])]))
