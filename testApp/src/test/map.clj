(ns test.map
  (:require [test.util :as util]))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"a" "b"})
(def systemName ["blub" "sadf"])
(def systemFeatures ["asdf" "fghf"])

(defrecord system [name planets solarStr features])
(defrecord planet [class size features])


(defn createPlanet []
  (planet. (util/hash-nth planetClasses) (util/rand-int1 8) (util/hash-nth planetFeatures)))

(defn createSystem [name]
  (system. name
           (assoc (hash-map) (take (util/rand-int1 4) (createPlanet)))
           (util/rand-int1 8)
           (rand-nth systemFeatures)))

(defn getRandPos [x y hashmap]
  (let [rndPos (str (util/rand-int1 (dec x)) "-" (util/rand-int1 (dec y)))]
    (if (contains? hashmap rndPos)
      (recur x y hashmap)
      rndPos)))

(defn placeSystems [nr]
  (take nr (lazy-seq (createSystem (rand-nth systemName)))))

(defn createMap [x y anzSysteme])


