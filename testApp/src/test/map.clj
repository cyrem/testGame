(ns test.map
  (:require [test.util :as util]))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"Desert" "Ice" "Terran" "Vulcan"})
(def systemName ["Alpha Centauri" "Sol" "Alpha Proxima" "Deneb"])
(def systemFeatures {"asdf" "fghf"})

(defrecord Universe [starMap civList])
(defrecord PSystem [name planets solarStr features owner])
(defrecord Planet [class size features])


(defn createPlanet []
  (Planet. (util/hash-nth planetClasses) (util/rand-int1 8) (util/hash-nth planetFeatures)))

(defn createSystem [_]
  (PSystem. (rand-nth systemName)
            (into [] (for [_ (range (util/rand-int1 4))]
                       (test.map/createPlanet)))
            (util/rand-int1 5)
            (util/hash-nth systemFeatures)
            nil ))


(defn createMap [[dimX dimY]]
  (into [] (for [x (range dimX)
                 y (range dimY)]
             [[x y] (agent nil)])))

(def testUni (Universe. (createMap [3 3]) [] ))

(defn placeSystems [uni nr]
  (doall
    (map 
      #(do
         (send (second  %1) createSystem ))
      (util/getUniqueRndKey (:starMap uni) nr))))

(defn getAllSystems [uni]
  (reduce #(when (second %2)
             (conj %1 (first %2)))
          []
          (:starMap uni)))



