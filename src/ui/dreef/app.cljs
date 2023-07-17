(ns dreef.app
  (:require
   [applied-science.js-interop :as j]
   [beicon.core :as rx]
   [clojure.string :as string]
   [potok.core :as ptk]
   [dreef.state :refer [emit! get-next-id]]
   [dreef.layout :as layout]
   [dreef.views :as views]
   [dreef.tabs :as tabs]
   [dreef.editor-tool :as editor]
   [dreef.script :as script]
   [dreef.utils :as utils]))


(defn initialize []
  (let [group-id       (get-next-id)
        pane-id        (get-next-id)
        editor-view-id (get-next-id)]
    ;; add default pane group and pane
    (emit!
     (layout/add-pane-group-evt
      {:type     :horizontal
       :group-id group-id
       :parent   :root})
     (layout/add-pane-evt
      {:pane-id pane-id
       :parent  group-id
       :view    editor-view-id})
     ;; add view for editor
     (views/add-view-evt
      {:view-id   editor-view-id
       :component :editor
       :tabs      true}))))


(defn get-active-editor-view [state]
  (utils/find-first #(= (:component %) (:active-editor state))
                    (-> state :view vals)))


(defn sync-tab-with-script-status-evt [{:keys [id tab-id script-id]}]
  (ptk/reify ::sync-with-script
    ptk/WatchEvent
    (watch [_ state _]
      (let [script-status (get-in state [:script script-id :status])]
        (rx/of (tabs/update-tab-evt
                {:id     id
                 :tab-id tab-id
                 :status script-status}))))))


(defn create-new-file-evt []
  (ptk/reify ::create-new-file
    ptk/WatchEvent
    (watch [_ state stream]
      (let [{editor-id :component tabs-id :tabs} (get-active-editor-view state)
            tab-id    (get-next-id)
            script-id (get-next-id)]
        (rx/merge
         (rx/from [(tabs/add-new-tab-evt {:id tabs-id :tab-id tab-id})
                   (tabs/select-tab-evt {:id tabs-id :tab-id tab-id})
                   (script/create-script-evt {:id script-id})
                   (editor/update-editor-evt {:id editor-id :script script-id})])
         ;; sync the opened file with related tab
         (->> (rx/merge
               (rx/filter (ptk/type? ::script/update-script) stream)
               (rx/filter (ptk/type? ::script/save-script) stream))
              (rx/map #(sync-tab-with-script-status-evt
                        {:id        tabs-id
                         :tab-id    tab-id
                         :script-id script-id}))))))))


(defn open-file-name-dialog-evt []
  (if-some [file-path (j/call js/window :prompt "Enter a full file path")]
    (emit! (ptk/data-event ::new-file-config
                           {:name (-> file-path (string/split #"/") last)
                            :path file-path}))
    (emit! (ptk/data-event ::save-file-cancel))))


(defn save-file-evt []
  (ptk/reify ::save-file
    ptk/WatchEvent
    (watch [_ state stream]
      (let [{editor-id :component tabs-id :tabs} (get-active-editor-view state)
            script-id     (get-in state [:editor editor-id :script])
            {:keys [doc status]} (get-in state [:script script-id])
            active-tab-id (get-in state [:tabs tabs-id :active])
            new-script?   (= status :new)]
        ;; check if new file -> ask for a name
        (if new-script?
          (let [cancel-saving   (rx/filter (ptk/type? ::save-file-cancel) stream)
                new-file-config (->> stream
                                     (rx/filter (ptk/type? ::new-file-config))
                                     (rx/take 1))]
            ;; emulate popup actions for now
            (js/setTimeout
             #(open-file-name-dialog-evt)
             10)
            ;; save new script if it's a new one
            (->> (rx/merge
                  ;; save the script
                  (rx/map #(let [{:keys [name path]} (deref %)]
                             (script/save-script-evt
                              {:id   script-id
                               :text doc
                               :name name
                               :path path}))
                          new-file-config)
                  ;; set tab status
                  (rx/map #(tabs/update-tab-evt
                            {:id     tabs-id
                             :tab-id active-tab-id
                             :title  (-> % deref :name)
                             :status :saved})
                          new-file-config))
                 (rx/take-until cancel-saving)))

          ;; just save script text and update tab
          (rx/of (script/save-script-evt
                  {:id   script-id
                   :text doc})
                 (tabs/update-tab-evt
                  {:id     tabs-id
                   :tab-id active-tab-id
                   :status :saved})))))))


(comment
 (emit! (create-new-file-evt))
 (emit! (save-file-evt))

 nil)
