(ns test.map
  (:refer-clojure :exclude [* - + == /])
  (:require [test.util :as util]
            [test.dataTypesMatrix :as data]))


(def ressources ["Iron" "Silicium" "Crystal" "Carbon" "Hydrogen" "Xenon"])

(def ressourcesPlanetType {"Desert" {"Iron" 1
                                     "Silicium" 1.3
                                     "Crystal" 1.3
                                     "Carbon" 0.9
                                     "Hydrogen" 0.5
                                     "Xenon" 0.4}
                           "Ice" {"Iron" 1
                                  "Silicium" 0.6
                                  "Crystal" 0.4
                                  "Carbon" 0.5
                                  "Hydrogen" 0.7
                                  "Xenon" 0.5}
                           "Terran" {"Iron" 1
                                     "Lead" 0.9
                                     "Silicium" 0.9
                                     "Crystal" 0.4
                                     "Carbon" 0.9
                                     "Hydrogen" 0.8
                                     "Xenon" 0.5}
                           "Vulcan" {"Iron" 1.4
                                     "Silicium" 0.7
                                     "Crystal" 1.5
                                     "Carbon" 0.7
                                     "Hydrogen" 0.5
                                     "Xenon" 0.9}
                           "Ocean" {"Iron" 0.7
                                    "Silicium" 0.5
                                    "Crystal" 0.4
                                    "Carbon" 0.6
                                    "Hydrogen" 1.4
                                    "Xenon" 0.7}
                           }
  )

(def planetFeatures {"Hostile WildLife" ["Fierce wildlife eats your people"
                                         {:maxPop 0.8 :popGrowth 0.7}]
                     "Radiation Belt" ["Boosts research output, that still doesn't make it healthy"
                                       {:sciPoints 1.2 :maxPop 0.9 :popGrowth 0.9}]
                     "Asteroid Impact" ["More ressources, but less livingspace"
                                        {:resPoints 1.1 :maxPop 0.9}]
                     "Microbes" ["damages buildings"
                                 {:upkeep 1.2}]
                     })

(def planetClasses  {"Desert" ["sand in every hole" 
                               {:resPoints 0.7}]
                     "Ice" ["Ice in every direction, look out for your heating bill"
                            {:resPoints 0.7 :upkeep 1.2}]
                     "Terran" ["Pretty Normal" 
                               {}]
                     "Vulcan" ["Pompeji all over again, 24/7" 
                               {:resPoints 1.3 :upkeep 1.3}]
                     "Ocean" ["pack your fishing rod" 
                              {:resPoints 1}]
                     })

(def systemName ["Alpha Centauri" "Sol" "Alpha Proxima" "Deneb" "Kayleh" "Sirius" "Ross 154"
                 "Ross 248" "Epsilon Eridani" "Procyon" "Epsilon Indi" "Tau Ceti" "Gliese 674"
                 ])

(def systemFeatures {"Asteroid Belt" ["lots of small stuff to run into" 
                                      {:upkeep 1.1}]
                     "Unstable Star" ["constant stellar erruption" 
                                      {:upkeep 1.2 :sciPoints 1.2}]
                     })


(defrecord Universe [starMap civList size])
(defrecord PSystem [name planets solarStr features owner])

;planetstats = Temperature, Ressource, science upkeep
(defrecord Planet [class size features ress])

(defn createPlanet []
  (let [class  (util/hash-nth planetClasses)]
  (Planet. class  (util/rand-int1 8) (util/getUniqueRndKey planetFeatures 1) (get ressourcesPlanetType class))))

(defn createSystem [_]
  (PSystem. (rand-nth systemName)
            (into [] (for [_ (range (util/rand-int1 4))]
                       (test.map/createPlanet)))
            (util/rand-int1 5)
            (util/hash-nth systemFeatures)
            nil ))

(defn createMap [coords]
  (into [] (for [x (range (first (:coords (:size coords))))
                 y (range  (second (:coords (:size coords))))]
             [(data/makeXYPoint x y) (agent nil)])))

(def uniSize (data/->Rectangle
               (data/->XYPoint (data/makeXYPoint 1 1))
               (data/->XYPoint (data/makeXYPoint 5 5))))

(def testUni (Universe. (createMap uniSize) [] uniSize))

(defn placeSystems [uni nr]
  (doall
    (map 
      #(send (second %1) createSystem )
      (util/getUniqueRndKey (:starMap uni) (inc nr)))))

;(placeSystems testUni 6)

(defn getAllSystems [uni]
  (reduce (fn [col val]
            (if (instance? test.map.PSystem  @(second val))
              (conj col (nth val 0))
              col))
          []
          (:starMap uni)))


