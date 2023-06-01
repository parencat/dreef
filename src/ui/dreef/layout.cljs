(ns dreef.layout
  (:require
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [beicon.core :as rx]
   [applied-science.js-interop :as j]
   [dreef.state :refer [subscribe emit! get-next-id]]
   [dreef.styles :refer [colors]]
   [dreef.editor-tool :refer [editor]]
   [dreef.utils :refer [assoc-some]]
   ["ui-box" :default box]))


(defn vertical? [type]
  (= type :vertical))


(def min-pane-size
  100)


(defn calculate-group-layout [state group-id dimensions & {:keys [new-item]}]
  (let [{:keys [type children]} (get-in state [:pane-group group-id])
        {:keys [width height left right top bottom]} dimensions
        items-count    (count children)
        vertical?      (vertical? type)
        prop           (if vertical? :height :width)
        group-size     (get dimensions prop)
        item-size      (Math/floor (/ group-size items-count))
        last-item-size (+ (- group-size
                             (* items-count item-size))
                          item-size)
        new-item-size  (get new-item prop)
        state          (update-in state [:pane-group group-id] merge dimensions)]
    (if (nil? (seq children))
      state
      (loop [s      state
             items  children
             coords {:left left :top top}]
        (let [item         (first items)
              rest-items   (rest items)
              item-type    (if (some? (:pane item)) :pane :pane-group)
              item-id      (or (:pane item) (:pane-group item))
              current-size (get-in s [item-type item-id prop])
              size         (cond (and (not= (:id new-item) item-id)
                                      (some? new-item-size)
                                      (some? current-size))
                                 (max min-pane-size
                                      (- current-size (/ new-item-size (dec items-count))))

                                 (and (= (:id new-item) item-id)
                                      (some? current-size)
                                      (not= items-count 1))
                                 current-size

                                 (seq rest-items) item-size
                                 :otherwise last-item-size)
              item-dims    (cond-> {:width width :height height}
                             :always (assoc prop size)
                             vertical? (assoc :top (:top coords)
                                              :bottom (+ (:top coords) size)
                                              :left left
                                              :right right)
                             (not vertical?) (assoc :left (:left coords)
                                                    :right (+ (:left coords) size)
                                                    :top top
                                                    :bottom bottom))
              new-coords   (if vertical?
                             (update coords :top + size)
                             (update coords :left + size))
              new-state    (if (= item-type :pane)
                             (update-in s [:pane item-id] merge item-dims)
                             (calculate-group-layout s (:pane-group item) item-dims))]
          (if-not (seq rest-items)
            new-state
            (recur new-state
                   rest-items
                   new-coords)))))))


(defn handle-add-pane-group [state {:keys [group-id type parent children order]
                                    :or   {children []
                                           group-id (get-next-id)}
                                    :as   group-props}]
  (let [parent-group       (get-in state [:pane-group parent])
        parent-child-count (count (:children parent-group))
        order              (if (and (some? order)
                                    (< order parent-child-count))
                             order
                             parent-child-count)
        new-children       (-> (concat (take order (:children parent-group))
                                       [{:pane-group group-id}]
                                       (drop order (:children parent-group)))
                               vec)
        group              (assoc-some {:id       group-id
                                        :type     type
                                        :parent   parent
                                        :children children}
                             :width (:width group-props)
                             :height (:height group-props))
        dimensions         (select-keys parent-group [:width :height :left :right :top :bottom])]
    (-> state
        (assoc-in [:pane-group group-id] group)
        (assoc-in [:pane-group parent :children] new-children)
        (calculate-group-layout (:id parent-group) dimensions :new-item group))))


(defn remove-child-by-id [children type id]
  (remove
   (fn [child]
     (let [child-id (get child type)]
       (= child-id id)))
   children))


(defn handle-remove-pane-group [state group-id]
  (let [{:keys [parent children]} (get-in state [:pane-group group-id])]
    (-> state
        (update :pane-group dissoc group-id)
        (update-in [:pane-group parent :children] remove-child-by-id :pane-group group-id)
        (as-> $
          (reduce
           (fn [s child]
             (let [child-type (if (some? (:pane child)) :pane :pane-group)
                   child-id   (get child child-type)]
               (if (= child-type :pane)
                 (update s :pane dissoc child-id)
                 (handle-remove-pane-group s child-id))))
           $ children)))))


(defn handle-add-pane [state {:keys [pane-id parent order view]
                              :or   {pane-id (get-next-id)}}]
  (let [group             (get-in state [:pane-group parent])
        group-child-count (count (:children group))
        order             (if (and (some? order)
                                   (< order group-child-count))
                            order
                            group-child-count)
        new-children      (-> (concat (take order (:children group))
                                      [{:pane pane-id}]
                                      (drop order (:children group)))
                              vec)
        pane              {:id     pane-id
                           :view   view
                           :parent parent}
        dimensions        (select-keys group [:width :height :left :right :top :bottom])]
    (-> state
        (assoc-in [:pane pane-id] pane)
        (assoc-in [:pane-group parent :children] new-children)
        (calculate-group-layout parent dimensions :new-item pane))))


(defn shrink-items-prop [state group-id prop edge edge-fn percent change-opposite-edge?]
  (let [{:keys [children] :as group} (get-in state [:pane-group group-id])
        first-child (first children)
        last-child  (last children)]
    (reduce (fn [s item]
              (let [item-type             (if (some? (:pane item)) :pane :pane-group)
                    item-id               (get item item-type)
                    item-props            (get-in s [item-type item-id])
                    current-value         (get item-props prop)
                    current-edge          (get item-props edge)
                    opposite-edge         (case edge
                                            :top :bottom
                                            :bottom :top
                                            :left :right
                                            :right :left)
                    current-opposite-edge (get item-props opposite-edge)
                    new-value             (/ (* current-value percent)
                                             100)
                    delta                 (- new-value current-value)
                    opposite-edge-new-val (if (or (= item last-child)
                                                  (not change-opposite-edge?))
                                            current-opposite-edge
                                            (edge-fn current-opposite-edge delta))
                    new-state             (update-in s [item-type item-id] assoc
                                            prop new-value
                                            edge (if (= item first-child)
                                                   (get group edge)
                                                   (edge-fn current-edge delta))
                                            opposite-edge opposite-edge-new-val)]
                (if (= item-type :pane)
                  new-state
                  (shrink-items-prop new-state item-id prop edge edge-fn percent true))))
            state
            children)))


(defn calculate-items-layout [state group-id item-type item-id gutter-position]
  (let [{:keys [type children] :as group} (get-in state [:pane-group group-id])
        vertical?      (vertical? type)
        item           (get-in state [item-type item-id])
        item-edge      (if vertical? :bottom :right)

        [_ next-item] (drop-while #(not= (get % item-type) item-id) children)
        next-item-type (if (some? (:pane next-item)) :pane :pane-group)
        next-item-id   (get next-item next-item-type)
        next-item-edge (if vertical? :top :left)
        next-item      (get-in state [next-item-type next-item-id])

        delta          (- (get item item-edge) gutter-position)
        current-next   #{item-id next-item-id}
        change-prop    (if vertical? :height :width)

        group-size     (get group change-prop)
        item-size      (get item change-prop)
        next-item-size (get next-item change-prop)

        rest-width     (->> children
                            (filter #(not (contains? current-next (or (:pane %) (:pane-group %)))))
                            (map #(let [id   (or (:pane %) (:pane-group %))
                                        type (if (some? (:pane %)) :pane :pane-group)]
                                    (get-in state [type id change-prop])))
                            (reduce +))

        change-val     (min (max min-pane-size (- item-size delta))
                            (- group-size rest-width min-pane-size))

        next-item-val  (max min-pane-size
                            (min (+ next-item-size delta)
                                 (- group-size rest-width min-pane-size)))

        item-edge-val  (+ (get item next-item-edge) change-val)

        item-dims      (assoc item change-prop change-val
                                   item-edge item-edge-val)
        next-item-dims (assoc next-item change-prop next-item-val
                                        next-item-edge item-edge-val)]

    (cond-> state
      :always (update-in [item-type item-id] merge item-dims)
      :always (update-in [next-item-type next-item-id] merge next-item-dims)
      (= item-type :pane-group) (shrink-items-prop item-id change-prop item-edge + (/ (* change-val 100) item-size) false)
      (= next-item-type :pane-group) (shrink-items-prop next-item-id change-prop next-item-edge - (/ (* next-item-val 100) next-item-size) false))))


(defn ->group-items [children]
  (if (seq children)
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
                                                :item-id   (or (:pane child) (:pane-group child))}}))]
        (if has-more?
          (recur group rest)
          group)))
    []))


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


(defn set-group-items-dimensions [{:keys [group-id item-type item-id gutter-position]}]
  (ptk/reify ::set-group-items-dimensions
    ptk/UpdateEvent
    (update [_ state]
      (calculate-items-layout state group-id item-type item-id gutter-position))))


(defn calculate-full-layout [dimensions]
  (ptk/reify ::set-group-dimensions
    ptk/UpdateEvent
    (update [_ state]
      (calculate-group-layout state :root dimensions))))


(defn calc-group-layout-on-resize [group-id unmount]
  (ptk/reify ::set-group-dimensions
    ptk/WatchEvent
    (watch [_ state stream]
      (->> stream
           (rx/filter #(and (ptk/type? ::resize-pane %)
                            (-> % deref :group-id (= group-id))))
           (rx/debounce 5)
           (rx/map #(set-group-items-dimensions (deref %)))
           (rx/take-until unmount)))))


(defn add-pane-group [group-props]
  (ptk/reify ::add-pane-group
    ptk/UpdateEvent
    (update [_ state]
      (handle-add-pane-group state group-props))))


(defn remove-pane-group [group-id]
  (ptk/reify ::remove-pane-group
    ptk/UpdateEvent
    (update [_ state]
      (let [{:keys [parent]} (get-in state [:pane-group group-id])
            dimensions (-> (get-in state [:pane-group parent])
                           (select-keys [:width :height :left :right :top :bottom]))]
        (-> state
            (handle-remove-pane-group group-id)
            (calculate-group-layout parent dimensions))))))


(defn add-pane [pane-props]
  (ptk/reify ::add-pane
    ptk/UpdateEvent
    (update [_ state]
      (handle-add-pane state pane-props))))


(defn remove-pane []
  (ptk/reify ::remove-pane
    ptk/UpdateEvent
    (update [_ state]
      state)))


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


(mf/defc pane-group [{:keys [group-id]}]
  (let [{:keys [type children width height]} (mf/deref (subscribe [:pane-group group-id]))
        vertical?   (vertical? type)
        group-items (->group-items children)]

    (mf/use-effect
     (fn []
       (let [unmount (rx/subject)]
         (emit! (calc-group-layout-on-resize group-id unmount))
         ;; unmount  callback
         #(rx/push! unmount true))))

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
         [:& pane-group {:key      (:pane-group item)
                         :group-id (:pane-group item)}]
         :gutter
         [:& gutter {:key         (str "gutter-for" (:item-id item))
                     :group-id    group-id
                     :gutter-type (if vertical? :horizontal :vertical)
                     :item-type   (:item-type item)
                     :item-id     (:item-id item)}]))]))


(mf/defc layout-manager []
  (let [el-ref (mf/use-ref)
        ready? (mf/use-state false)]

    (mf/use-layout-effect
     (fn []
       (let [element-rect (j/call-in el-ref [:current :getBoundingClientRect])
             width        (j/get element-rect :width)
             height       (j/get element-rect :height)
             left         (j/get element-rect :left)
             right        (j/get element-rect :right)
             top          (j/get element-rect :top)
             bottom       (j/get element-rect :bottom)]
         ;; weird thing if state update happens in between the render cycles components wouldn't react on it
         (emit! (calculate-full-layout {:width width :height height :left left :right right :top top :bottom bottom}))
         (swap! ready? true))))

    [:> box {:ref     el-ref
             :height  "100%"
             :display "flex"}
     (when @ready?
       [:& pane-group {:group-id :root}])]))



(comment
 (def gr
   {:id       10
    :type     :horizontal
    :parent   :root
    :children [{:pane 5} {:pane-group 11}]})

 (def pn
   {:id   4
    :view 31})

 (def vw
   {:id        31
    :component :editor})

 (emit! (add-pane-group
         {:type   :horizontal
          :parent :root}))

 (emit! (add-pane
         {:view   nil
          :parent 2}))

 (emit! (add-pane-group
         {:type   :vertical
          :parent 2}))

 (emit! (add-pane-group
         {:type   :horizontal
          :parent 2}))

 (emit! (add-pane
         {:view   nil
          :parent 7}))

 (emit! (set-group-items-dimensions
         {:group-id        :root
          :item-type       :pane-group
          :item-id         1
          :gutter-position 300}))

 (emit! (remove-pane-group 1)))
