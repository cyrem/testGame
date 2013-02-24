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
(def windX 800)
(def windY 600)
(def running (atom true))

(set! *warn-on-reflection* true)
(native!)

(def img (ImageIO/read (clojure.java.io/file "D:/Button.png" )))
(defrecord gObj [name img coords speed direction])
;speed = pixel pro sekunde
(def but (agent (gObj. "test" img [0 0] 50 [0 1])))
(def daf (agent (gObj. "asdf" img [100 200] 50 [1 0])))

(defn mVec [[x y] [dx dy]]
  (let [f (fn [val max]
            (cond 
               (> val max) 0
               (< val 0) max
               :else val))
        xOut (f (+ x dx) windX)
        yOut (f (+ y dy) windY)]
    [xOut yOut]))

(defn moveObj [obj]
  (assoc obj :coords (mVec (:coords obj) (:direction obj))))

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
        (Thread/sleep 100)
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
