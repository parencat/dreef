(ns dreef.tool
  (:require
   [rumext.v2 :as mf]
   [dreef.styles :refer [icon colors]]
   ["ui-box" :default box]))


(mf/defc tabs [{:keys [tabs]}]
  [:> box {:display          "flex"
           :overflow-x       "scroll"
           :class            "no-scroll"
           :height           32
           :background-color (:polar1 colors)}
   (for [{:keys [title type active]} tabs]
     (let [tab-color   (if active (:polar2 colors) (:polar1 colors))
           title-color (if active (:snow2 colors) (:snow0 colors))
           no-icon     (= type :no-icon)]
       [:> box {:key              title
                :class            (when active "active")
                :flex-basis       60
                :width            200
                :display          "flex"
                :flex-wrap        "nowrap"
                :align-items      "center"
                :justify-content  "flex-start"
                :cursor           "pointer"
                :background-color tab-color
                :selectors        {"&:hover:not(.active)"
                                   {:backgroundColor (:polar0 colors)}}}

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
                                    {:backgroundColor (:polar3 colors)}}}
         [:& icon {:type :x}]]]))])

