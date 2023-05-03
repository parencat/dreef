(ns dreef.styles
  (:require
   [rumext.v2 :as mf]
   ["ui-box" :default box]))


(def colors
  {:polar0  "#242933"
   :polar1  "#2E3440"
   :polar2  "#3B4252"
   :polar3  "#434C5E"
   :polar4  "#4C566A"

   :snow0   "#D8DEE9"
   :snow1   "#E5E9F0"
   :snow2   "#ECEFF4"

   :frost0  "#8FBCBB"
   :frost1  "#88C0D0"
   :frost2  "#81A1C1"
   :frost3  "#5E81AC"

   :aurora0 "#BF616A"
   :aurora1 "#D08770"
   :aurora2 "#EBCB8B"
   :aurora3 "#A3BE8C"
   :aurora4 "#B48EAD"})


(def icons
  {:x    "M16.24 14.83a1 1 0 01-1.41 1.41L12 13.41l-2.83 2.83a1 1 0 01-1.41-1.41L10.59 12 7.76 9.17a1 1 0 011.41-1.41L12 10.59l2.83-2.83a1 1 0 011.41 1.41L13.41 12l2.83 2.83z"
   :code "M20.59 12l-3.3-3.3a1 1 0 111.42-1.4l4 4a1 1 0 010 1.4l-4 4a1 1 0 01-1.42-1.4l3.3-3.3zM3.4 12l3.3 3.3a1 1 0 01-1.42 1.4l-4-4a1 1 0 010-1.4l4-4A1 1 0 016.7 8.7L3.4 12zm7.56 8.24a1 1 0 01-1.94-.48l4-16a1 1 0 111.94.48l-4 16z"})


(mf/defc icon [{:keys [type color]}]
  [:svg {:viewBox "0 0 24 24"
         :fill    "currentColor"
         :display "block"
         :height  "1em"
         :width   "1em"}
   [:path {:d     (get icons type)
           :color color}]])
