(ns test.core

  (:require [seesaw core graphics swingx]
        test.map
        test.render
        test.civ)
  (:import [javax.swing JFrame]
           [java.awt Canvas Graphics2D Color Graphics]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO])
  
    (:refer-clojure :exclude [* - + == /])
    (:use [clojure.core.matrix operators])
  )

(def running (atom true))
(set! *warn-on-reflection* true)
(seesaw.core/native!)

(def windX 800)
(def windY 600)
(def img (ImageIO/read (clojure.java.io/file "D:/Button1.png" )))


(defprotocol Scan 
  (scan [this objectList]"was kann ich sehen")
  (scan-pos [this]))

(defprotocol Move
  (move [this world dest]"bewegung")
  (move-pos [this objectList dest] "bewegung möglich"))

(defprotocol Fight
  (fight [this target]"angriff")
  (in-range [this target] "nein, zu weit weg")
  (fight-pos [this target]))

(defprotocol Update-tick
  (update-tick [this objectList] "entscheiden sie jetzt"))


;size = 1 corvette, 2 frig, 3 destroyer 4 cruiser 5 battleship 6 carrier
;components = weapons, shields, sensors, reactors, cargo,special,armor
(defrecord Weapon [name attackNr baseHit strength range blastRadius consumption mods])
(defrecord Shields [name points strength consumption mods])
(defrecord Armor [name points strength mods])
(defrecord Reactor [name production mods])


(defrecord GObj [name img size coords direction speed components])

(def but (agent (GObj. "test" img 1 [80 80] [1 0] 5 [])))
(def daf (agent (GObj. "asdf" img 1 [250 80] [-1 0] 11 [])))


(def objL(ref []))

(dosync
  (alter objL conj but)
  (alter objL conj daf))

(defn rectIntersect [[minX minY] [maxX maxY] [minRectX minRectY] [maxRectX maxRectY]] 
  (not (or (> minRectX maxX)
           (< maxRectX minX)
           (> minRectY maxY)
           (< maxRectY minY))))

(defn partialIntersect [[minX minY] [maxX maxY] [minRectX minRectY] [maxRectX maxRectY]]
  (or 
    (>= minRectX minX)
    (<= maxRectX maxX)
    (>= minRectY minY)
    (<= maxRectY maxY)))

(defn mVec [dir speed]
   (* dir speed))

(defn mirrorDir [inpVec]
  (into [] (map #(* -1 %) inpVec)))

(defn moveObj [obj]
  (let [coordsMax [(.getWidth ^BufferedImage (:img obj))
                   (.getHeight ^BufferedImage (:img obj))]
        d (mVec (:direction obj) (:speed obj))
        newCoords (+ (:coords obj) d)
        newCoordsMax (+ newCoords coordsMax)]
    
    (cond 
      (or
        (partialIntersect newCoords newCoordsMax [1 1] [windX windY]) 
        (reduce #(let [entityCoords (:coords @%2)
                       maxEntityCoords (+ entityCoords [(.getWidth ^BufferedImage (:img @%2)) (.getHeight ^BufferedImage (:img @%2))])]
                   (if (or %1
                           (rectIntersect newCoords newCoordsMax entityCoords maxEntityCoords))
                     true
                     nil)
                   ) false (filter #(not (= (:name obj) (:name @%))) @objL))) (assoc obj :direction (mirrorDir (:direction obj)))
      :else (assoc obj :coords newCoords))))


(defn dImage [c ^Graphics g]
  (dorun (map #(let [[x y] (:coords @%)]
      (doto g
        (.drawImage (:img @%) x y nil))) @objL)))


(def pan (seesaw.core/canvas :id :canvas :paint dImage))
(def gW (agent (seesaw.core/frame :title "gameWindow"
                       :content pan
                       :width windX
                       :height windY)))

;(seesaw.core/listen @gW :key-pressed (fn [e] (println "button pressed" e)))

(defn drawFrame [x]
  (seesaw.core/show! x)
  (loop [i x]
    (if (= @running true)
      (do
        (seesaw.core/repaint! i)
        (Thread/sleep 30)
        (recur i))
      i)))

(defn paintLoop []
  (send gW drawFrame)
  (loop [it 0]
    
    ;(println it)
    (dorun (map #(send-off % moveObj) @objL))
    
    (when (>= it 50)
      (reset! running nil))
    (Thread/sleep 100)
    

      (if (= true @running)
        (recur (inc it))
        nil)))


(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
