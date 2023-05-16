(ns dreef.state
  (:require
   [okulary.core :as ol]))


(defonce state
  (ol/atom
   {:pane-group {:root {:id       :root
                        :type     :vertical
                        :children [{:group 10} {:pane 3}]}

                 10    {:id       10
                        :type     :horizontal
                        :parent   :root
                        :children [{:pane 5} {:group 11}]}

                 11    {:id       11
                        :type     :vertical
                        :parent   10
                        :children [{:pane 4} {:pane 6}]}}

    :pane       {4 {:id   4
                    :view :editor-1}

                 5 {:id   5
                    :view :editor-2}

                 6 {:id   6
                    :view :editor-3}

                 3 {:id   3
                    :view :console}}}))


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
   (let [selector-fn (cond (map? selector) (map-selector selector)
                           (vector? selector) (ol/in selector)
                           :else selector)]
     (ol/derived selector-fn parent-state))))
