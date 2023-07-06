(ns dreef.tabs
  (:require
   [rumext.v2 :as mf]
   [potok.core :as ptk]
   [applied-science.js-interop :as j]
   [dreef.state :refer [emit! subscribe get-next-id]]
   [dreef.styles :refer [icon colors]]
   [dreef.utils :as utils]
   ["ui-box" :default box]))


(defn ancestor? [child parent]
  (loop [p child]
    (let [next-p (j/get p :parentElement)]
      (cond
        (= p parent) true
        (some? next-p) (recur next-p)
        :otherwise false))))


(defn select-tab-evt [{:keys [id tab]}]
  (ptk/reify ::select-tab
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:tabs id :active] tab))))


(defn close-tab-evt [{:keys [id tab]}]
  (ptk/reify ::close-tab
    ptk/UpdateEvent
    (update [_ state]
      (let [tabs      (get-in state [:tabs id :items])
            tab-idx   (utils/index-of {:id tab} tabs)
            next-item (cond (>= (dec tab-idx) 0)
                            (->> (dec tab-idx) (get tabs) :title)

                            (<= (inc tab-idx) (dec (count tabs)))
                            (->> (inc tab-idx) (get tabs) :title)

                            :otherwise nil)]
        (update-in state [:tabs id] assoc
                   :items (utils/remove-nth tab-idx tabs)
                   :active next-item)))))


(defn close-tab-click [{:keys [tabs-id tab]} event]
  (j/call event :stopPropagation)
  (emit! (close-tab-evt {:id tabs-id :tab tab})))


(defn add-tabs-evt [{:keys [id tabs view-id]}]
  (ptk/reify ::add-tabs
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:tabs id] {:items  tabs
                                :active (-> tabs first :title)})
          (assoc-in [:view view-id :tabs] id)))))


(defn add-tab-evt [{:keys [id tab]}]
  (ptk/reify ::add-tab
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:tabs id :items] conj tab))))


(defn swap-tabs-evt [id from to]
  (ptk/reify ::swap-tabs
    ptk/UpdateEvent
    (update [_ state]
      (let [tabs     (get-in state [:tabs id :items])
            from-idx (utils/index-of {:id from} tabs)
            to-idx   (utils/index-of {:id to} tabs)
            from-tab (get tabs from-idx)
            tabs     (utils/remove-nth from-idx tabs)
            tabs     (utils/insert-nth to-idx from-tab tabs)]
        (assoc-in state [:tabs id :items] tabs)))))


(mf/defc tab
  [{:keys [tabs-id tab-id title type active move-tab
           dragging? set-dragging dragged-over? set-drag-over]}]
  (let [current-tab-el (mf/use-ref)
        tab-close      (mf/use-callback (partial close-tab-click {:tabs-id tabs-id :tab tab-id}))
        tab-select     (mf/use-callback #(emit! (select-tab-evt {:id tabs-id :tab tab-id})))
        on-drag-over   (mf/use-callback #(j/call % :preventDefault))
        on-drop        (fn [event]
                         (j/call event :stopPropagation)
                         (let [item (j/call-in event [:dataTransfer :getData] "text/plain")]
                           (move-tab item)))
        on-drag-start  (mf/use-callback (fn [event]
                                          (j/call-in event [:dataTransfer :setData] "text/plain" tab-id)
                                          (j/assoc-in! event [:dataTransfer :effectAllowed] "move")
                                          (set-dragging tab-id)))
        on-drag-end    (mf/use-callback #(do (set-dragging nil) (set-drag-over nil)))
        on-drag-enter  (mf/use-callback #(set-drag-over tab-id))
        on-drag-leave  (mf/use-callback (fn [event]
                                          (let [target  (j/get event :target)
                                                current (j/get current-tab-el :current)]
                                            (when-not (ancestor? target current)
                                              (set-drag-over nil)))))
        tab-color      (cond
                         dragged-over? (:polar4 colors)
                         active (:polar2 colors)
                         :otherwise (:polar1 colors))
        title-color    (if active (:snow2 colors) (:snow0 colors))
        opacity        (if dragging? 0.2 1)
        no-icon        (= type :no-icon)]
    [:> box {:ref              current-tab-el
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


(def ^:const tabs-height
  32)


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
               :height           tabs-height
               :background-color (:polar1 colors)}
       (for [{:keys [id title type]} items]
         [:& tab {:key           id
                  :tab-id        id
                  :title         title
                  :tabs-id       tabs-id
                  :type          type
                  :move-tab      move-tab
                  :active        (= id active)
                  :dragging?     (= id @dragged-tab)
                  :set-dragging  set-dragging
                  :dragged-over? (= id @dragged-over-tab)
                  :set-drag-over set-drag-over}])])))

