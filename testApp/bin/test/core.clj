(ns test.core
  (:use [seesaw core graphics swingx]
        test.map
        test.render
        test.civ
        ;korma.db
        )
  (:import [javax.swing JFrame]
           [java.awt Canvas Graphics2D Color Graphics]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def running (atom true))
(set! *warn-on-reflection* true)
(native!)

(def windX 800)
(def windY 600)

(def img (ImageIO/read (clojure.java.io/file "D:/Button.png" )))
(defrecord gObj [name img size coords direction speed])

(def but (agent (gObj. "test" img [(.getWidth ^BufferedImage img) (.getHeight ^BufferedImage img)] [1 1] [1 1] 5)))
(def daf (agent (gObj. "asdf" img [(.getWidth ^BufferedImage img) (.getHeight ^BufferedImage img)] [250 250] [1 0] 11)))

(defn rectIntersect [[minX minY] [maxX maxY] [minRectX minRectY] [maxRectX maxRectY]] 
  (not (or (> minRectX maxX)
           (< maxRectX minX)
           (> minRectY maxY)
           (< maxRectY minY))))

(defn mVec [[x y] [dx dy] speed]
  (let [f (fn [val max]
            (cond 
               (> val max) 1
               (< val 1) max
               :else val))
        xOut (f (+ x (* dx speed)) windX)
        yOut (f (+ y (* dy speed)) windY)]
    [xOut yOut]))

(defn mirrorDir [inpVec]
  (into []  (map #(* -1 %) inpVec)))

(defn moveObj [obj]
  (let [newCoords (mVec (:coords obj) (:direction obj) (:speed obj))
        colOjbL (filter #(rectIntersect newCoords (:size obj) (:coords @%) (:size @%)) @objL)]
    (println colOjbL)
    (if (empty? colOjbL)
      (assoc obj :coords newCoords)
      (assoc obj :direction (mirrorDir (:direction obj))))))

(def objL(ref []))

(dosync
  (alter objL conj but)
  (alter objL conj daf))

(defn dImage [c ^Graphics g]
  (dorun (map #(let [[x y] (:coords @%)]
      (doto g
        (.drawImage (:img @%) x y nil)))@objL)))

(def pan (canvas :id :canvas :paint dImage))
(def gW (agent (frame :title "gameWindow"
                       :content pan
                       :width windX
                       :height windY)))

(listen @gW :key-pressed (fn [e] (println "button pressed" e)))

(defn drawFrame [x]
  (show! x)
  (loop [i x]
    (if (= @running true)
      (do
        (repaint! i)
        (Thread/sleep 30)
        (recur i))
      i)))

(defn paintLoop []
  (send gW drawFrame)
  (loop [it 0]
    
    (println it)
    (dorun (map #(send-off % moveObj) @objL))
    
    (when (>= it 100)
      (reset! running nil))
    
      (Thread/sleep 100)
      (if (= true @running)
        (recur (inc it))
        nil)))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
