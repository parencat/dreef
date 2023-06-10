(ns dreef.tabs
  (:require
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [applied-science.js-interop :as jsi]
   [dreef.state :refer [emit! subscribe get-next-id]]
   [dreef.styles :refer [icon colors]]
   ["ui-box" :default box]))


(defn index-of [pred coll]
  (let [[pred-key pred-val] (first pred)]
    (reduce (fn [idx item]
              (if (= (get item pred-key) pred-val)
                (reduced idx)
                (inc idx)))
            0 coll)))


(defn remove-nth [idx coll]
  (-> (concat
       (take idx coll)
       (drop (inc idx) coll))
      vec))


(defn update-nth [idx update-fn coll]
  (let [item (update-fn (get coll idx))]
    (-> (concat
         (take idx coll)
         (list item)
         (drop (inc idx) coll))
        vec)))


(defn insert-nth [idx item coll]
  (-> (concat
       (take idx coll)
       (list item)
       (drop idx coll))
      vec))


(defn ancestor? [child parent]
  (loop [p child]
    (let [next-p (jsi/get p :parentElement)]
      (cond
        (= p parent) true
        (some? next-p) (recur next-p)
        :otherwise false))))


(defn select-tab-evt [id title]
  (ptk/reify ::select-tab
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:tabs id :active] title))))


(defn close-tab-evt [id title]
  (ptk/reify ::close-tab
    ptk/UpdateEvent
    (update [_ state]
      (let [tabs      (get-in state [:tabs id :items])
            tab-idx   (index-of {:title title} tabs)
            next-item (cond (>= (dec tab-idx) 0)
                            (->> (dec tab-idx) (get tabs) :title)

                            (<= (inc tab-idx) (dec (count tabs)))
                            (->> (inc tab-idx) (get tabs) :title)

                            :otherwise nil)]
        (update-in state [:tabs id] assoc
          :items (remove-nth tab-idx tabs)
          :active next-item)))))


(defn close-tab-click [tabs-id title event]
  (jsi/call event :stopPropagation)
  (emit! (close-tab-evt tabs-id title)))


(defn add-tabs-evt [{:keys [id tabs view-id]}]
  (ptk/reify ::add-tabs
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:tabs id] {:items  tabs
                                :active (-> tabs first :title)})
          (assoc-in [:view view-id :tabs] id)))))


(defn swap-tabs-evt [id from to]
  (ptk/reify ::swap-tabs
    ptk/UpdateEvent
    (update [_ state]
      (let [tabs     (get-in state [:tabs id :items])
            from-idx (index-of {:title from} tabs)
            to-idx   (index-of {:title to} tabs)
            from-tab (get tabs from-idx)
            tabs     (remove-nth from-idx tabs)
            tabs     (insert-nth to-idx from-tab tabs)]
        (assoc-in state [:tabs id :items] tabs)))))


(mf/defc tab
  [{:keys [tabs-id title type active move-tab
           dragging? set-dragging dragged-over? set-drag-over]}]
  (let [current-tab-el (mf/use-ref)
        tab-close      (mf/use-callback (partial close-tab-click tabs-id title))
        tab-select     (mf/use-callback #(emit! (select-tab-evt tabs-id title)))
        on-drag-over   (mf/use-callback #(jsi/call % :preventDefault))
        on-drop        (fn [event]
                         (jsi/call event :stopPropagation)
                         (let [item (jsi/call-in event [:dataTransfer :getData] "text/plain")]
                           (move-tab item)))
        on-drag-start  (mf/use-callback (fn [event]
                                          (jsi/call-in event [:dataTransfer :setData] "text/plain" title)
                                          (jsi/assoc-in! event [:dataTransfer :effectAllowed] "move")
                                          (set-dragging title)))
        on-drag-end    (mf/use-callback #(do (set-dragging nil) (set-drag-over nil)))
        on-drag-enter  (mf/use-callback #(set-drag-over title))
        on-drag-leave  (mf/use-callback (fn [event]
                                          (let [target  (jsi/get event :target)
                                                current (jsi/get current-tab-el :current)]
                                            (when-not (ancestor? target current)
                                              (set-drag-over nil)))))
        tab-color      (cond
                         dragged-over? (:polar4 colors)
                         active (:polar2 colors)
                         :otherwise (:polar1 colors))
        title-color    (if active (:snow2 colors) (:snow0 colors))
        opacity        (if dragging? 0.2 1)
        no-icon        (= type :no-icon)]
    [:> box {:key              title
             :ref              current-tab-el
             :class            (when active "active")
             :flex-basis       60
             :width            200
             :display          "flex"
             :flex-wrap        "nowrap"
             :align-items      "center"
             :justify-content  "flex-start"
             :cursor           "pointer"
             :background-color tab-color
             :opacity          opacity
             :selectors        {"&:hover:not(.active)"
                                {:backgroundColor (:polar0 colors)}}
             :on-click         tab-select

             :draggable        true
             :on-drag-start    on-drag-start
             :on-drag-end      on-drag-end
             :on-drag-enter    on-drag-enter
             :on-drag-over     on-drag-over
             :on-drag-leave    on-drag-leave
             :on-drop          on-drop}

     ;; switch icon depends on type
     (when-not no-icon
       [:> box {:is     "span"
                :margin 8}
        [:& icon {:type :code}]])

     [:> box {:is            "span"
              :flex          "1 1"
              :overflow      "hidden"
              :text-overflow "ellipsis"
              :margin-left   (when no-icon 16)
              :color         title-color}
      title]

     [:> box {:is               "button"
              :margin           8
              :padding          0
              :line-height      1
              :border-radius    "50%"
              :cursor           "pointer"
              :border           "none"
              :display          "block"
              :background-color "transparent"
              :color            "inherit"
              :white-space      "nowrap"
              :selectors        {:&:hover
                                 {:backgroundColor (:polar3 colors)}}
              :on-click         tab-close}
      [:& icon {:type :x}]]]))


(mf/defc tabs [{:keys [tabs-id]}]
  (let [{:keys [items active]} (mf/deref (subscribe [:tabs tabs-id]))
        tabs-el          (mf/use-ref)
        dragged-tab      (mf/use-state nil)
        dragged-over-tab (mf/use-state nil)
        set-dragging     (mf/use-callback #(reset! dragged-tab %))
        set-drag-over    (mf/use-callback #(reset! dragged-over-tab %))
        move-tab         #(emit! (swap-tabs-evt tabs-id % @dragged-over-tab))]
    (when-not (empty? items)
      [:> box {:ref              tabs-el
               :display          "flex"
               :overflow-x       "scroll"
               :class            "no-scroll"
               :height           32
               :background-color (:polar1 colors)}
       (for [{:keys [title type]} items]
         [:& tab {:title         title
                  :tabs-id       tabs-id
                  :type          type
                  :move-tab      move-tab
                  :active        (= title active)
                  :dragging?     (= title @dragged-tab)
                  :set-dragging  set-dragging
                  :dragged-over? (= title @dragged-over-tab)
                  :set-drag-over set-drag-over}])])))

