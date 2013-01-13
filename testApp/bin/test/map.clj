(ns test.map
  (:require [test.util :as test]))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"a" "b"})
(def systemName ["blub" "sadf"])
(def systemFeatures ["asdf" "fghf"])

(defrecord system [name planets solarStr features])
(defrecord planet [class size features])


(defn createPlanet []
  (planet. (hash-nth planetClasses) (rand-int1 8) (hash-nth planetFeatures)))

(defn createSystem [name]
  (system. name
           (assoc (hash-map) (take (rand-int1 4) (createPlanet)))
           (rand-int1 8)
           (rand-nth systemFeatures)))

(defn getRandPos [x y hashmap]
  (let [rndPos (str (rand-int1 (dec x)) "-" (rand-int1 (dec y)))]
    (if (contains? hashmap rndPos)
      (recur x y hashmap)
      rndPos)))

(defn placeSystems [nr]
  (take nr (lazy-seq (createSystem (rand-nth systemName)))))

(defn createMap [x y anzSysteme])


