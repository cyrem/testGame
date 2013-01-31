(ns test.util)

(defn rand-int1 [n]
  (inc (rand-int n)))

(defn hash-nth [hashmap]
  (get hashmap (rand-nth (keys hashmap))))

(defn getUniqueRndKey [nr col]
  (loop [items (keys col)
         n nr
         acu []]
    (cond
      (> n (count items)) (throw (Exception. "anzahl größer als items in der col"))
      (<= n 0) acu
      :else (let [item (rand-nth items)] 
              (recur (filter #(not (= % item)) items) (dec n) (conj acu item))))))



(defun retFileAsVector [path])

