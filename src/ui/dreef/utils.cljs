(ns dreef.utils)


(defn assoc-some
  "Associates a key k, with a value v in a map m, if and only if v is not nil."
  ([m k v]
   (if (nil? v) m (assoc m k v)))

  ([m k v & kvs]
   (reduce (fn [m [k v]] (assoc-some m k v))
           (assoc-some m k v)
           (partition 2 kvs))))


(defn find-first
  "Finds the first item in a collection that matches a predicate. Returns a
  transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result x]
        (if (pred x)
          (ensure-reduced (rf result x))
          result)))))
  ([pred coll]
   (reduce (fn [_ x] (when (pred x) (reduced x))) nil coll)))


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
