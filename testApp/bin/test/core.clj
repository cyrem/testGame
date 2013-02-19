(ns test.core
  (:use [seesaw core graphics swingx]
        test.map
        test.render
        test.civ
        korma.db)
  (:import [javax.swing JFrame]
           [java.awt Canvas Graphics2D Color Graphics]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))
(def windX 800)
(def windY 600)
(def running (atom true))

(set! *warn-on-reflection* true)

(def img (ImageIO/read (clojure.java.io/file "D:/Button.png" )))
(defrecord gObj [name img coords])
(def but (agent (gObj. "test" img [0 0])))
(def daf (agent (gObj. "asdf" img [100 200])))

(defn mVec [[x y] [dx dy]]
  (let [f (fn [val max]
            (cond 
               (> val max) 0
               (< val 0) max
               :else val))
        xOut (f (+ x dx) windX)
        yOut (f (+ y dy) windY)]
    [xOut yOut]))

(defn moveObj [obj dVec]
  (assoc obj :coords (mVec (:coords obj) dVec)))

(def objL(ref []))

(dosync
  (alter objL conj but)
  (alter objL conj daf))


(defn dImage [c g]
  (doseq [i @objL] 
    (let [[x y] (:coords @i)]
      (doto g
      (.drawImage (:img @i) x y nil)))))

(def pan (canvas :id :canvas :paint dImage))
(def gW (agent (frame :title "gameWindow"
                       :content pan
                       :width windX
                       :height windY)))

(defn drawFrame [x]
  (when (= @running true)
    (repaint! x)
    (. Thread (sleep 100))
    (send-off gW #'drawFrame)
    gW))


(native!)

(defn paintLoop []
  (invoke-later (show! @gW))
  (send gW drawFrame)
  (loop [it 0]
    (when (> it 1000)
      (reset! running nil))

      (send (nth @objL 0) moveObj [0 1])
      (send (nth @objL 1) moveObj [1 0])
      (println it)
      (. Thread (sleep 100))
      
      (if (= true @running)
        (recur (inc it))
        nil)))


(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
