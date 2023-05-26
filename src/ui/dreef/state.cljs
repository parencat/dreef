(ns dreef.state
  (:require
   [okulary.core :as ol]
   [beicon.core :as rx]
   [potok.core :as ptk]))


(def next-id
  (atom 0))


(defn get-next-id []
  (swap! next-id inc))


(defonce state
  (ptk/store
   {:state
    {:pane-group {:root {:id       :root
                         :type     :vertical
                         :children [{:pane-group 10} {:pane-group 12}]}

                  10    {:id       10
                         :type     :horizontal
                         :parent   :root
                         :children [{:pane 5} {:pane-group 11}]}

                  11    {:id       11
                         :type     :vertical
                         :parent   10
                         :children [{:pane 4} {:pane 6}]}

                  12    {:id       12
                         :type     :horizontal
                         :parent   :root
                         :children [{:pane 3} {:pane 7} {:pane 8}]}}

     :pane       {4 {:id   4
                     :view 31}

                  5 {:id   5
                     :view :editor-2}

                  6 {:id   6
                     :view :editor-3}

                  3 {:id   3
                     :view :console}

                  7 {:id   7
                     :view :console-1}

                  8 {:id   8
                     :view :console-2}}

     :view       {31 {:id        31
                      :component :editor}}}}))


(defn emit!
  ([] nil)
  ([event]
   (ptk/emit! state event)
   nil)
  ([event & events]
   (apply ptk/emit! state (cons event events))
   nil))


(defn map-selector [selector]
  (fn [s]
    (reduce-kv
     (fn [acc k v]
       (assoc acc k (get-in s v)))
     {}
     selector)))


(defn subscribe
  ([selector]
   (subscribe selector state))

  ([selector parent-state]
   (let [[selector-fn compare-fn]
         (cond (vector? selector) [(ol/in selector) identical?]
               (keyword selector) [selector identical?]
               (map? selector) [(map-selector selector) =]
               :else [selector =])]
     (ol/derived selector-fn parent-state compare-fn))))
