(ns test.map
  (:require [test.util :as util]))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"a" "b"})
(def systemName ["blub" "sadf"])
(def systemFeatures ["asdf" "fghf"])

(defrecord universe [starMap systemList unitList civList])
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


(defn createMap [[dimX dimY]]
 (into {} (for [x (range dimX)
                y (range dimY)]
            [[x y] (ref nil)])))

;(def testUni (universe. (createMap [20 20]) [] [] []))

(defn placeSystems [nr uni]
  (map (fn [refToLoc]
           (sync nil
                 (ref-set (get (:starMap uni) refToLoc) (createSystem (rand-nth systemName)))))
         (test.util/getUniqueRndKey nr (:starMap uni))))

