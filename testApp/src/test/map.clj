(ns test.map)
(use test.util)


(def mapX 200)
(def mapY 200)
(def planetNr (Math/pow (+ mapX mapY) 2))

(def planetFeatures {"c" 0 "d" 0})
(def planetClasses  {"a" "b"})
(def systemName ["blub" "sadf"])
(def systemFeatures ["asdf" "fghf"])

(def sysMap (atom (hash-map)))

(defrecord system [name planets solarStr features])
(defrecord planet [name class size features])



(defn createPlanet [name]
  (planet. name (hash-nth planetClasses) (rand-int1 8) (hash-nth planetFeatures)))

(defn createSystem [name]
  (system. name
           (into {} (dotimes [i (rand-int1 4)]
                      (createPlanet name)))
           (rand-int1 8)
           (rand-nth systemFeatures)))

(defn getRandPos [x y hashmap]
  (let [rndPos (str (rand-int1 (dec x)) "-" (rand-int1 (dec y)))]
    (if (contains? hashmap rndPos)
      (recur x y hashmap)
      rndPos)))

(defn placeSystems
  ([systemM nr]
    (placeSystems (assoc systemM (getRandPos mapX mapY @sysMap) (createSystem (rand-nth systemName))) nr 1))
  ([systemM nr i]
    (if (< i nr)
      (recur (assoc systemM  (getRandPos mapX mapY @sysMap)(createSystem (rand-nth systemName))) nr (inc i))
      systemM)))

(defn createMap [x y]
  (reset! sysMap (placeSystems @sysMap 5))
  (println @sysMap))


