(ns test.util)

(defn rand-int1 [n]
  (inc (rand-int n)))

(defn hash-nth [hashmap]
  (get hashmap (rand-nth (keys hashmap))))

(defn getUniqueRndKey [nr col]
  (let [k (keys col)]
    (loop [nr nr
           keys k
           acu '()]
      (if (> nr 0)
        (let [rndk (rand-nth keys)]
          (recur (dec nr) (dissoc keys rndk) (lazy-seq rndk)))
        acu))))