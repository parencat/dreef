(ns dreef.layout-test
  (:require [cljs.test :refer [deftest is]]
            [dreef.layout :as sut]))


(def one-groups-two-panes
  {:pane-group {:root {:id       :root
                       :children [{:pane-group 1}]
                       :type     :vertical
                       :top      0 :bottom 800
                       :left     0 :right 1025
                       :width    1025 :height 800}
                1     {:id       1
                       :parent   :root
                       :children [{:pane 2} {:pane 3}]
                       :type     :horizontal
                       :top      0 :bottom 800
                       :left     0 :right 1025
                       :width    1025 :height 800}}
   :pane       {2 {:id     2
                   :parent 1
                   :top    0 :bottom 800
                   :width  512 :height 800
                   :left   0 :right 512
                   :view   nil}
                3 {:id     3
                   :parent 1
                   :top    0 :bottom 800
                   :left   512 :right 1025
                   :width  513 :height 800
                   :view   nil}}})


(def three-groups-five-panes
  {:pane-group {:root {:id :root
                       :children [{:pane-group 1} {:pane-group 2}]
                       :type :vertical
                       :top 0 :bottom 800
                       :left 0 :right 1025
                       :width 1025 :height 800}
                1 {:id 1
                   :type :horizontal
                   :parent :root
                   :children [{:pane 3} {:pane 4}]
                   :top 0 :bottom 400
                   :width 1025 :height 400
                   :left 0 :right 1025}
                2 {:id 2
                   :type :horizontal
                   :parent :root
                   :children [{:pane 5} {:pane-group 6}]
                   :top 400 :bottom 800
                   :width 1025 :height 400
                   :left 0 :right 1025}
                6 {:id 6
                   :type :vertical
                   :parent 2
                   :children [{:pane 7} {:pane 8}]
                   :top 400 :bottom 800
                   :width 513 :height 400
                   :left 512} :right 1025}
   :pane       {3 {:id     3
                   :parent 1
                   :top    0 :bottom 400
                   :left   0 :right 512
                   :width  512 :height 400
                   :view   nil}
                4 {:id     4
                   :parent 1
                   :top    0 :bottom 400
                   :left   512 :right 1025
                   :width  513 :height 400
                   :view   nil}
                5 {:id     5
                   :parent 2
                   :top    400 :bottom 800
                   :left   0 :right 512
                   :width  512 :height 400
                   :view   nil}
                7 {:id     7
                   :parent 6
                   :top    400 :bottom 600
                   :left   512 :right 1025
                   :width  513 :height 200
                   :view   nil}
                8 {:id     8
                   :parent 6
                   :top    600 :bottom 800
                   :left   512 :right 1025
                   :width  513 :height 200
                   :view   nil}}})


(deftest calculate-items-layout-test
  (let [group-id        1
        pane-id         2
        gutter-position 350
        result          (sut/calculate-items-layout one-groups-two-panes group-id :pane pane-id gutter-position)]
    (is (= (get-in result [:pane 2 :width]) 350))
    (is (= (get-in result [:pane 2 :right]) 350))
    (is (= (get-in result [:pane 3 :width]) 675))
    (is (= (get-in result [:pane 3 :left]) 350)))

  (let [group-id        :root
        pane-group-id   1
        gutter-position 300
        result          (sut/calculate-items-layout three-groups-five-panes group-id :pane-group pane-group-id gutter-position)]
    (is (= (get-in result [:pane-group 1 :height]) 300))
    (is (= (get-in result [:pane-group 1 :bottom]) 300))
    (is (= (get-in result [:pane-group 2 :height]) 500))
    (is (= (get-in result [:pane-group 2 :top]) 300))
    (is (= (get-in result [:pane-group 6 :top]) 300))
    (is (= (get-in result [:pane-group 6 :bottom]) 800))
    (is (= (get-in result [:pane 5 :top]) 300))
    (is (= (get-in result [:pane 5 :bottom]) 800))
    (is (= (get-in result [:pane 5 :height]) 500))
    (is (= (get-in result [:pane 7 :top]) 300))
    (is (= (get-in result [:pane 7 :bottom]) 550))
    (is (= (get-in result [:pane 7 :height]) 250))
    (is (= (get-in result [:pane 8 :top]) 550))
    (is (= (get-in result [:pane 8 :bottom]) 800))
    (is (= (get-in result [:pane 8 :height]) 250))))
