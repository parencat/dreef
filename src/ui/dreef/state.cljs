(ns dreef.state
  (:require
   [okulary.core :as ol]
   [potok.core :as ptk]))


(def next-id
  (atom 0))


(defn get-next-id []
  (swap! next-id inc))


(defonce state
  (ptk/store
   {:state
    {:pane-group    {:root {:id       :root
                            :type     :vertical
                            :children []}}
     :pane          {}

     :view          {}
     :active-view   nil

     :tabs          {}

     :editor        {}
     :active-editor nil

     :script        {}}}))


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
