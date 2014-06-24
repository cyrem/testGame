(ns test.util)

(defn rand-int1 [n]
  (inc (rand-int n)))

(defn hash-nth [hashmap]
  (get hashmap (rand-nth (keys hashmap))))

(defn- getUniqe [col nr]
  (loop [items  col
         n nr
         acu []]
    (cond
      (> n (count items)) (throw (Exception. "anzahl größer als items in der col"))
      (<= n 0) acu
      :else (let [item (rand-nth items)] 
              (recur (filter #(not (= % item)) items) (dec n) (conj acu item))))))

(defn getUniqueRndKey [col nr]
  (cond 
    (map? col) (getUniqe (keys col) nr)
    (vector? col) (getUniqe col nr)))


(defn getModPos [x y maxX] 
  "assumes coordinates 0 indexed, doesnt check max Values"
  (+ x (* maxX y)))

(defn getCoordPos [nr maxX]
  "returns 0 indexed coords"
  [(mod nr maxX)  (java.lang.Math/floorDiv nr maxX)])

(defn retFileAsVector [path])

