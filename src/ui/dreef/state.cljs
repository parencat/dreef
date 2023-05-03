(ns dreef.state
  (:require
   [okulary.core :as ol]))


(defonce state
  (ol/atom
   {:layout {:left-pane   {:visible true}
             :right-pane  {:visible true}
             :bottom-pane {:visible true}}}))


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
