(ns test.util)

(defn rand-int1 [n]
  (inc (rand-int n)))

(defn hash-nth [hashmap]
  (get hashmap (rand-nth (keys hashmap))))

(defn getUniqueRndKey [nr col]
  (assert (<= nr (count col)) "mehr items als col hat")
  (let [recurFunc (fn [nr keyMap acu i]
                    (let [item (rand-nth keyMap)]
                      (println item)
                      (if (< i nr)
                        (recur nr (filterv #(not (= item %)) keyMap) (conj acu item) (inc i))
                        (conj acu item))))]
    (recurFunc nr (into [] (map #(first %) col)) [] 1)))




