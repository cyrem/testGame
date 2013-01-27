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
           (into [] (for [_ (range (util/rand-int1 4))]
                      (test.map/createPlanet)))
           (util/rand-int1 8)
           (rand-nth systemFeatures)))


(defn placeSystems [nr]
  (createSystem (rand-nth systemName)))

(defn createMap [x y anzSysteme]
  ;hashmap mit [x y] vector als key
  )



