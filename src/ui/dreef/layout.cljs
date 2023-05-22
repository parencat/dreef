(ns dreef.layout
  (:require
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [beicon.core :as rx]
   [applied-science.js-interop :as j]
   [dreef.state :refer [state subscribe emit!]]
   [dreef.styles :refer [colors]]
   [dreef.editor-tool :refer [editor]]
   ["ui-box" :default box]))


(defn vertical? [type]
  (= type :vertical))


(defn calculate-group-layout [state group-id {:keys [width height] :as dimensions}]
  (let [{:keys [type children]} (get-in state [:pane-group group-id])
        items-count    (count children)
        prop           (if (vertical? type) :height :width)
        group-size     (get dimensions prop)
        item-size      (Math/floor (/ group-size items-count))
        last-item      (last children)
        last-item-size (+ (- group-size
                             (* items-count item-size))
                          item-size)]
    (-> state
        (update-in [:pane-group group-id] merge {:width width :height height})
        (as-> $
              (reduce
               (fn [s item]
                 (let [size      (if (= item last-item) last-item-size item-size)
                       item-dims (assoc {:width width :height height} prop size)]
                   (if-some [pane-id (:pane item)]
                     (update-in s [:pane pane-id] merge item-dims)
                     (calculate-group-layout s (:group item) item-dims))))
               $ children)))))


(defn ->group-items [children]
  (loop [group []
         items children]
    (let [[child & rest] items
          has-more? (seq rest)
          pane?     (some? (:pane child))
          type      (if pane? :pane :pane-group)
          group     (cond-> group
                      :always (conj {:type type
                                     :item child})
                      has-more? (conj {:type :gutter
                                       :item {:item-type type
                                              :item-id   (or (:pane child) (:group child))}}))]
      (if has-more?
        (recur group rest)
        group))))


(defn mouse-track [{:keys [group-id gutter-type item-type item-id]} event]
  (j/call event :preventDefault)
  (let [vertical? (vertical? gutter-type)
        value     (if vertical?
                    (j/get event :pageX)
                    (j/get event :pageY))]
    (emit! (ptk/data-event ::resize-pane
                           {:group-id        group-id
                            :item-type       item-type
                            :item-id         item-id
                            :gutter-position value}))))


(defn gutter-mouse-down [gutter-props]
  (let [abort-controller (new js/AbortController)
        abort-signal     (j/get abort-controller :signal)
        body             (j/get js/document :body)]
    (j/call body :addEventListener "mousemove"
      #(mouse-track gutter-props %) #js {:signal abort-signal})
    (j/call body :addEventListener "mouseup"
      #(j/call abort-controller :abort) #js {:signal abort-signal})))


(mf/defc gutter [{:keys [gutter-type] :as gutter-props}]
  (let [vertical?   (vertical? gutter-type)
        horizontal? (not vertical?)]
    [:> box {:on-mouse-down #(gutter-mouse-down gutter-props)
             :cursor        (if vertical? "col-resize" "row-resize")
             :position      "relative"
             :selectors     {:&:before
                             {:content   "''"
                              :position  "absolute"
                              :width     (if vertical? "10px" "100%")
                              :height    (if vertical? "100%" "10px")
                              :left      (when vertical? "50%")
                              :top       (when horizontal? "50%")
                              :transform (if vertical?
                                           "translateX(-50%)"
                                           "translateY(-50%)")}
                             :&:after
                             {:content         "''"
                              :position        "absolute"
                              :backgroundColor (:polar1 colors)
                              :width           (if vertical? "2px" "100%")
                              :height          (if vertical? "100%" "2px")
                              :left            (when vertical? "50%")
                              :top             (when horizontal? "50%")
                              :transform       (if vertical?
                                                 "translateX(-50%)"
                                                 "translateY(-50%)")}}}]))


(mf/defc view [{:keys [view-id]}]
  (let [{view-component :component} (mf/deref (subscribe [:view view-id]))]
    (case view-component
      :editor [:& editor]
      nil)))


(mf/defc pane [{:keys [pane-id]}]
  (let [{:keys [width height] view-id :view} (mf/deref (subscribe [:pane pane-id]))]
    [:> box {:data-pane   (str "pane-" pane-id)
             :overflow    "scroll"
             :flex-grow   0
             :flex-shrink 0
             :style       {:width  (str width "px")
                           :height (str height "px")}}
     [:& view {:view-id view-id}]]))


(defn set-group-items-dimensions [group-id item-props]
  (ptk/reify ::set-group-items-dimensions
    ptk/UpdateEvent
    (update [_ state]
     ;; get group from state
     ;; get all group items
     ;; set new dims for specified item
     ;; calculate rest items sizes
     ;; return new state
      (update-in state [:pane-group group-id] assoc :prp "dimensions"))))


(defn calc-group-layout-on-resize [group-id unmount]
  (ptk/reify ::set-group-dimensions
    ptk/WatchEvent
    (watch [_ state stream]
      (->> stream
           (rx/filter #(and (ptk/type? ::resize-pane %)
                            (-> % deref :group-id (= group-id))))
           (rx/map #(println "new value is" (-> % deref :group-id)))
           ;;(rx/map #(set-group-items-dimensions group-id (unchecked-get % "data")))
           (rx/take-until unmount)))))


(mf/defc render-group [{:keys [group-id]}]
  (let [{:keys [type children width height]} (mf/deref (subscribe [:pane-group group-id]))
        vertical?   (vertical? type)
        group-items (->group-items children)]

    (mf/use-effect
     (fn []
       (let [unmount (rx/subject)]
         (emit! (calc-group-layout-on-resize group-id unmount))
         ;; unmount  callback
         #(rx/push! unmount true))))

    (println group-id width height)
    [:> box {:key            group-id
             :data-group     (str "group-" group-id)
             :display        "flex"
             :flex-direction (if vertical? "column" "row")
             :flex-grow      0
             :flex-shrink    0
             :style          {:width  (str width "px")
                              :height (str height "px")}}
     (for [{:keys [type item]} group-items]
       (case type
         :pane
         [:& pane {:key     (:pane item)
                   :pane-id (:pane item)}]
         :pane-group
         [:& render-group {:key      (:group item)
                           :group-id (:group item)}]
         :gutter
         [:& gutter {:key         (str "gutter-for" (:item-id item))
                     :group-id    group-id
                     :gutter-type (if vertical? :horizontal :vertical)
                     :item-type   (:item-type item)
                     :item-id     (:item-id item)}]))]))


(defn calculate-full-layout [{:keys [width height]}]
  (ptk/reify ::set-group-dimensions
    ptk/UpdateEvent
    (update [_ state]
      (calculate-group-layout state :root {:width width :height height}))))


(mf/defc layout-manager []
  (let [el-ref (mf/use-ref)
        ready? (mf/use-state false)]

    (mf/use-layout-effect
     (fn []
       (let [element-rect (j/call-in el-ref [:current :getBoundingClientRect])
             width        (j/get element-rect :width)
             height       (j/get element-rect :height)]
         ;; weird thing if state update happens in between the render cycles components wouldn't react on it
         (emit! (calculate-full-layout {:width width :height height}))
         (swap! ready? true))))

    [:> box {:ref     el-ref
             :height  "100%"
             :display "flex"}
     (when @ready?
       [:& render-group {:group-id :root}])]))
