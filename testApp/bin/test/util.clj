(ns test.util)

(defn rand-int1 [n]
  (inc (rand-int n)))

(defn hash-nth [hashmap]
  (get hashmap (rand-nth (keys hashmap))))