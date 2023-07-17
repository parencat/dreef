(ns dreef.views
  (:require
   [beicon.core :as rx]
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [dreef.state :refer [subscribe emit! get-next-id]]
   [dreef.editor-tool :refer [editor add-editor-evt]]
   [dreef.tabs :refer [tabs tabs-height]]
   ["ui-box" :default box]))


(defn add-view-evt [{:keys [view-id component tabs]
                     :or   {view-id (get-next-id)}}]
  ;; view will initialize tabs and editor tool
  (let [tabs-id      (when tabs (get-next-id))
        component-id (get-next-id)]
    (ptk/reify ::add-view
      ptk/UpdateEvent
      (update [_ state]
        (-> state
            (assoc :active-view view-id)
            (assoc-in [:view view-id]
                      {:id             view-id
                       :component      component-id
                       :component-type component
                       :tabs           tabs-id})))

      ptk/WatchEvent
      (watch [_ _ _]
        (let [add-tabs           (when tabs
                                   (dreef.tabs/add-tabs-evt
                                    {:id      tabs-id
                                     :tabs    []
                                     :view-id view-id}))
              add-view-component (case component
                                   :editor (add-editor-evt
                                            {:id component-id}))]
          (rx/from (cond-> [add-view-component]
                           tabs (conj add-tabs))))))))


(mf/defc view [{:keys [view-id]}]
  (let [{:keys        [component-type]
         component-id :component
         tabs-id      :tabs}
        (mf/deref (subscribe [:view view-id]))

        tabs? (some? tabs-id)]
    [:> box {:data-view view-id
             :position  "relative"
             :height    "100%"}
     (when tabs?
       [:& tabs {:tabs-id tabs-id}])

     [:> box {:height (if tabs?
                        (str "calc(100% - " tabs-height "px)")
                        "100%")}
      (case component-type
        :editor [:& editor {:id      component-id
                            :tabs-id tabs-id}]
        nil)]]))
