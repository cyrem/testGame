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


(defprotocol ZipOps 
  (branch? [node])
  (children[node])
  (make-node[node children]))

(defrecord QuadNode [o d ^Rectangle b c]
  ZipOps
  (branch? [node]
    true)
  (children[node]
    (:c node))
  (make-node[node children]
    (assoc node :c children)))

(defn zipperCreate [coll]
    (zip/zipper
      branch?
      children
      make-node
      coll))

(defn- insertIntoQuad [unzippedNode val]
  (assoc unzippedNode :o (conj (:o unzippedNode) val)))


(defn insert [node val]
  (->
    (findWithBounds node val)
    (zip/edit insertIntoQuad val)
    (zip/root)
    (zipperCreate)))
  
  
(defn- deleteFromQuad [unzippedNode val])

(defn delete [node val])
  
;(defn subDivide [loc]
;  (let [unzippedNode (zip/node loc)
;        splitted (split ^Rectangle (:b unzippedNode))
;        newDepth (inc (:d unzippedNode))]
;    (-> loc
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 0) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 1) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 2) []))
;      (zip/append-child  (->QuadNode [] newDepth (nth splitted 3) []))
;      (zip/root)
;      (zipperCreate)
;      )))


(defn findWithBounds [node ^Rectangle inner]
  "takes node and a rectangle and finds the fitting node"
  (let [unzippedNode (zip/node node)
        nodeBounds(:b unzippedNode)
        fitsInThisNode (within? nodeBounds inner)
        subSector (getSector (:b unzippedNode) inner)
        childrenEmpty? (empty? (:c unzippedNode))
        canGoDeeper (< (:d unzippedNode) 4)]
      (match [childrenEmpty? fitsInThisNode subSector canGoDeeper]
             [_ false nil _] (zip/up node)
             [_ true false _] node
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
             :else "omgwtfbbq!!112431")))

(defn buildQuadTree [node]
  (cond (< (:d node) 4) (let [splitted (split ^Rectangle (:b node))
                              newDepth (inc (:d node))]
                          (->QuadNode  (:o node) (:d node) (:b node)[(buildQuadTree(->QuadNode [] newDepth (nth splitted 0) []))
                                                                     (buildQuadTree(->QuadNode [] newDepth (nth splitted 1) []))
                                                                     (buildQuadTree(->QuadNode [] newDepth (nth splitted 2) []))
                                                                     (buildQuadTree(->QuadNode [] newDepth (nth splitted 3) []))]))
        (= (:d node) 4) (let [splitted (split ^Rectangle (:b node))
                              newDepth (inc (:d node))]
                          (->QuadNode  (:o node) (:d node) (:b node)[]))))


(def tR (->Rectangle (->XYPoint (matrix [0 0])) (->XYPoint (matrix [100 100]))))
(def tR2 (->Rectangle (->XYPoint (matrix [6 6])) (->XYPoint (matrix [20 1]))))

(def q (->QuadNode [] 0 (->Rectangle
                                   (->XYPoint (matrix [0 0]))
                                   (->XYPoint (matrix  [200 200]))) []))

(def bQuad (buildQuadTree q))
(def zipCr (zipperCreate bQuad))
(def zipAg (agent zipCr))



