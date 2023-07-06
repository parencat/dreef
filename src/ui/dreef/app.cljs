(ns dreef.app
  (:require
   [beicon.core :as rx]
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


(defn create-new-file-evt []
  (ptk/reify ::create-new-file
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [active-editor view]} state
            {editor-id :component tabs-id :tabs}
            (utils/find-first #(= (:component %) active-editor)
                              (vals view))
            tab-id    (get-next-id)
            script-id (get-next-id)
            tab       {:id tab-id}]
        (rx/from [(tabs/add-tab-evt {:id tabs-id :tab tab})
                  (tabs/select-tab-evt {:id tabs-id :tab tab-id})
                  (script/create-script-evt {:id script-id})
                  (editor/update-editor-evt {:id editor-id :script script-id})])))))
