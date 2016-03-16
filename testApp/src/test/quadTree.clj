(ns test.quadTree
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.zip :as zip]
            [test.dataTypesMatrix])
  (:use [clojure.core.match :only (match)]
        [test.dataTypesMatrix]
        [clojure.core.matrix]
        [clojure.core.matrix operators])
  (:import [test.dataTypesMatrix Rectangle XYPoint]))

(set! *warn-on-reflection* true)
(set-current-implementation :vectorz) 

(defprotocol QuadGameElem
  (getId [this])
  (getRectangle[this]))

;"objects in this Node, depth, maxDepth, bounds, childs"
(defrecord QuadNode [o d mD ^Rectangle b c])


(defn zipperCreate [coll]
  (zip/zipper
    (fn [node]
      (when node
        (or 
          (contains? node :c)
          (and (class? node)
               (instance? 'test.quadTree.QuadNode node)))))
    (fn [node]
      (:c node))
    (fn [node children]
      (assoc node :c children))
    coll))

(defn- insertIntoQuad [unzippedNode val]
  (assoc unzippedNode :o (conj (:o unzippedNode) val)))

(defn- deleteFromQuad [unzippedNode val])

(defn delete [node val])

;(defn subDivide [loc]
;  (let [unzippedNode (zip/node loc)
;        splitted (split ^Rectangle (:b unzippedNode))
;        newDepth (inc (:d unzippedNode))]
;    (-> 
;      loc
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 0) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 1) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 2) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 3) []))
;      (zip/root)
;      (zipperCreate)
;      )))


(defn findWithBounds [^QuadNode node ^Rectangle inner]
  "takes node and a rectangle and finds the fitting node"
  (let [unzippedNode (zip/node node)
        nodeBounds(:b unzippedNode)
        fitsInThisNode (within? nodeBounds inner)
        subSector (getSector (:b unzippedNode) inner)
        childrenEmpty? (empty? (:c unzippedNode))
        canGoDeeper (< (:d unzippedNode) (:mD unzippedNode))]
    ;    (println childrenEmpty? fitsInThisNode subSector canGoDeeper)
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

(defn getAllElements [^QuadNode node]
  "returns a list with all objects from this node and all subnodes"  
  (let [intF (fn [n ret]
               (if (zip/end? n)
                 ret
                 (if (or(empty? (:o (zip/node n)))
                        (nil? (:o (zip/node n))))
                   (recur (zip/next n ) ret)
                   (recur (zip/next n ) (concat ret (:o (zip/node n)))))
                 ))]
    (intF node [])))


(defn insert [^QuadNode node val]
  (->
    (findWithBounds node val)
    (zip/edit insertIntoQuad val)
    (zip/root)
    (zipperCreate)))

(defn buildQuadTree [^QuadNode node]
  (cond (< (:d node) (:mD node)) (let [splitted (split ^Rectangle (:b node))
                                       newDepth (inc (:d node))]
                                   (QuadNode.  (:o node) (:d node) (:mD node) (:b node)[(buildQuadTree (QuadNode. [] newDepth (:mD node) (nth splitted 0) []))
                                                                                         (buildQuadTree (QuadNode. [] newDepth (:mD node) (nth splitted 1) []))
                                                                                         (buildQuadTree (QuadNode. [] newDepth (:mD node) (nth splitted 2) []))
                                                                                         (buildQuadTree (QuadNode. [] newDepth (:mD node) (nth splitted 3) []))]))
        (= (:d node) (:mD node)) (let [splitted (split ^Rectangle (:b node))
                                       newDepth (inc (:d node))]
                                   (QuadNode.  (:o node) (:d node) (:mD node) (:b node) []))))


(def tR (Rectangle. (XYPoint. (matrix [0 0])) (XYPoint. (matrix [100 100]))))
(def tR2 (Rectangle. (XYPoint. (matrix [6 6])) (XYPoint. (matrix [20 1]))))

(def q (QuadNode. [] 0 4 (Rectangle.
                            (XYPoint. (matrix [0 0]))
                            (XYPoint. (matrix  [100 100]))) []))




;(def bQuad (buildQuadTree q))
;(def zipCr (zipperCreate bQuad))
;(def zipAg (agent zipCr))





