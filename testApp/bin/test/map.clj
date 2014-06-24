(ns test.map
  (:require [test.util :as util]))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"a" "b"})
(def systemName ["blub" "sadf"])
(def systemFeatures ["asdf" "fghf"])

(defrecord Universe [starMap civList])
(defrecord PSystem [name planets solarStr features owner])
(defrecord Planet [class size features])


(defn createPlanet []
  (Planet. (util/hash-nth planetClasses) (util/rand-int1 8) (util/hash-nth planetFeatures)))

(defn createSystem [name]
  (PSystem. name
            (into [] (for [_ (range (util/rand-int1 4))]
                       (test.map/createPlanet)))
            (util/rand-int1 8)
            (rand-nth systemFeatures)
            nil ))


(defn createMap [[dimX dimY]]
  (into [] (for [x (range dimX)
                 y (range dimY)]
             [[x y] (agent nil)])))

(def testUni (Universe. (createMap [20 20]) [] ))

(defn placeSystems [uni nr]
  (doall
    (map 
      #(send (get (:starMap uni) %1)  (createSystem (rand-nth systemName)))
      (test.util/getUniqueRndKey (:starMap uni) nr))))

(defn getAllSystems [uni]
  (let [starMap (:starMap uni)]
    (reduce #() starMap)
  )
  )




