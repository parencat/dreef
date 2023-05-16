(ns dreef.layout
  (:require
   [rumext.v2 :as mf]
   [applied-science.js-interop :as j]
   [dreef.state :refer [state subscribe]]
   [dreef.styles :refer [colors]]
   [dreef.editor-tool :refer [editor]]
   ["ui-box" :default box]))


(defn vertical? [type]
  (= type :vertical))


(defn mouse-track [{:keys [group-ref gutter-type item-type item-id]} event]
  (j/call event :preventDefault)
  (let [vertical?    (vertical? gutter-type)
        element-rect (j/call-in group-ref [:current :getBoundingClientRect])
        size         (if vertical?
                       (* (min (max (- (j/get event :pageX) (j/get element-rect :left))
                                    (+ (j/get element-rect :left) 40))
                               (- (j/get element-rect :right) 40))
                          (/ 100 (j/get element-rect :width)))

                       (* (min (max (- (j/get event :pageY) (j/get element-rect :top))
                                    (+ (j/get element-rect :top) 40))
                               (- (j/get element-rect :bottom) 40))
                          (/ 100 (j/get element-rect :height))))]
    (swap! state update-in [item-type item-id] assoc :size size)))


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


(mf/defc pane [{:keys [pane-id size]}]
  (let [{:keys [view width height]} (mf/deref (subscribe [:pane pane-id]))]
    [:> box {:data-pane   (str "pane-" pane-id)
             :overflow    "scroll"
             :flex-grow   0
             :flex-shrink 0
             :flex-basis  (or size "100%")
             :style       {:width  width
                           :height height}}
     (name view)]))


(defn ->group-items [children]
  (let [default-item-size (/ 100 (count children))]
    (loop [group                []
           items                children
           remaining-group-size 100]
      (let [[child & rest] items
            has-more? (seq rest)
            item-size (cond (some? (:size child)) (:size child)
                            (not has-more?) remaining-group-size
                            :otherwise default-item-size)
            pane?     (some? (:pane child))
            type      (if pane? :pane :pane-group)
            group     (cond-> group
                        :always (conj {:type type
                                       :item child
                                       :size (str item-size "%")})
                        has-more? (conj {:type :gutter
                                         :item {:item-type type
                                                :item-id   (or (:pane child) (:group child))}}))]
        (if has-more?
          (recur group rest (- remaining-group-size item-size))
          group)))))


(defn pane-group-state [group-id]
  (subscribe
   (fn [state]
     (let [group    (get-in state [:pane-group group-id])
           children (map
                     (fn [{:keys [group pane] :as child}]
                       (if (some? group)
                         (assoc child :size (get-in state [:pane-group group :size]))
                         (assoc child :size (get-in state [:pane pane :size]))))
                     (:children group))]
       {:type     (:type group)
        :children children}))))


(mf/defc render-group [{:keys [group-id size]}]
  (let [{:keys [type children]} (mf/deref (pane-group-state group-id))
        group-ref   (mf/use-ref)
        vertical?   (vertical? type)
        group-items (->group-items children)]

    (mf/use-layout-effect
     (fn []
       (let [element-rect (j/call-in group-ref [:current :getBoundingClientRect])]
         (swap! state update-in [:pane-group group-id] assoc
                :left (j/get element-rect :left)
                :top (j/get element-rect :top)
                :width (j/get element-rect :width)
                :height (j/get element-rect :height)))))

    [:> box {:key            group-id
             :ref            group-ref
             :data-group     (str "group-" group-id)
             :display        "flex"
             :flex-direction (if vertical? "column" "row")
             :flex-grow      0
             :flex-shrink    0
             :flex-basis     (or size "100%")}
     (for [{:keys [type item size]} group-items]
       (case type
         :pane
         [:& pane {:key     (:pane item)
                   :pane-id (:pane item)
                   :size    size}]
         :pane-group
         [:& render-group {:key      (:group item)
                           :group-id (:group item)
                           :size     size}]
         :gutter
         [:& gutter {:key         (str "gutter-for" (:item-id item))
                     :gutter-type (if vertical? :horizontal :vertical)
                     :group-ref   group-ref
                     :item-type   (:item-type item)
                     :item-id     (:item-id item)}]))]))


(mf/defc layout-manager []
  [:> box {:height  "100%"
           :display "flex"}
   [:& render-group {:group-id :root}]])
