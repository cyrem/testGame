(ns test.quadTreeWithTypes
  (:require [clojure.zip :as zip]
            [test.dataTypesMatrix :as dTM]
            )
  (:use [clojure.core.match :only [match]]
        [test.dataTypesMatrix])
  (:import [test.dataTypesMatrix Rectangle XYPoint]  ))


(set! *warn-on-reflection* true)
(definterface IQuadNode
  (setC [v]))

;volatile-mutable
;(deftype Point [^{:volatile-mutable true} x]
(deftype QuadNode [^int d ^int md ^Rectangle b c ^clojure.lang.PersistentVector ^volatile-mutable o]
  IQuadNode
  ;(get-name [this] (. this lname))
  (setC [this v] (set! (.c this) v)))


(defn zipperCreate [coll]
  (zip/zipper
    (fn [node]
      (and 
        (= (type node) test.quadTreeWithTypes.QuadNode)
        (< (.d ^QuadNode node) (.md ^QuadNode node))))
    (fn [node]
      (.c ^QuadNode node))
    (fn [node children]
      (.setC ^QuadNode node children))
    coll))


(def q (QuadNode. 0 4 (Rectangle.
                           (XYPoint. [0 0])
                           (XYPoint. [100 100]))
                   [] 
                   []))


(defn buildQuadTree [^QuadNode node]
  (cond (< (.d ^QuadNode node) (.md ^QuadNode node)) (let [splitted (dTM/split (.b node))
                                                           newDepth (inc (.d node))]
                                                       ;(println "< " (.d node))
                                                       ;(println splitted)
                                                       (QuadNode. (.d ^QuadNode node) (.md ^QuadNode node) (.b ^QuadNode node) [(buildQuadTree (QuadNode. newDepth (.md node) (nth splitted 0) [] []))
                                                                                                                                (buildQuadTree (QuadNode. newDepth (.md node) (nth splitted 1) [] []))
                                                                                                                                (buildQuadTree (QuadNode. newDepth (.md node) (nth splitted 2) [] []))
                                                                                                                                (buildQuadTree (QuadNode. newDepth (.md node) (nth splitted 3) [] []))]
                                                                  (.o node)))
        
        (= (.d node) (.md node)) (let [splitted (split ^Rectangle (.b node))
                                       newDepth (inc (.d node))]
                                   (QuadNode.   (.d node) (.md node) (.b node) [](.o node)))
        )
  )
;(.b (zip/node (findWithBounds (zipperCreate bQuad) tR3)))
(defn findWithBounds [^QuadNode node ^Rectangle inner]
  "takes node and a rectangle and finds the fitting node"
  (let [unzippedNode (zip/node node)
        nodeBounds(.b unzippedNode)
        fitsInThisNode (within? nodeBounds inner)
        subSector (getSector (.b unzippedNode) inner)
        childrenEmpty? (empty? (.c unzippedNode))
        canGoDeeper (< (.d unzippedNode) (.md unzippedNode))]
        (println childrenEmpty? fitsInThisNode subSector canGoDeeper (.d unzippedNode))
    (match [childrenEmpty? fitsInThisNode subSector canGoDeeper];catch outside of provided bounds -> -5 1 to 100(x,y)
           [_ false nil _] (zip/up node)
           [_ true _ false] node
           [_ _ :nw true] (recur (-> node
                                   zip/down) inner)
           
           [_ _ :ne true] (recur (-> node
                                   zip/down
                                   zip/right) inner)
           
           [_ _ :sw true] (recur (-> node
                                   zip/down
                                   zip/right
                                   zip/right) inner)
           
           [_ _ :se true] (recur (-> node
                                   zip/down
                                   zip/right
                                   zip/right
                                   zip/right) inner)
           :else (throw (Exception. "omgwtfbbq!!112431"))
           )))

(def tR (Rectangle. (XYPoint.  [0 0]) (XYPoint.  [100 100])))
(def tR2 (Rectangle. (XYPoint.  [6 6]) (XYPoint.  [20 1])))
(def tR3 (Rectangle. (XYPoint. [1001 2000])(XYPoint. [3000 3000])))

(def bQuad (buildQuadTree q))



(defn benchQnode [^long nr]
  (if (< nr 1)
    nil
    (do 
      (zipperCreate bQuad)
      (recur (dec nr)))))

(defn print-tree [original]
  (loop [loc original]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
                (do (println (.b(zip/node loc)))
                    loc))))))

;(time (benchQnode 1000))
;(->QuadNode 1 1 [][])
;(def exampleQuad (->QuadNode 0 1 [(->QuadNode 1 1 ["a"][])(->QuadNode 1 1 ["b"][])(->QuadNode 1 1 ["c"][])(->QuadNode 1 1 ["d"][])] []))



