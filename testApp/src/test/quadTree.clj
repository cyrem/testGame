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

(defn insert [node val])
  
(defn delete [node val])
  
(defn subDivide [loc]
  (let [unzippedNode (zip/node loc)
        splitted (split ^Rectangle (:b unzippedNode))
        newDepth (inc (:d unzippedNode))]
    (-> loc
      (zip/append-child  (->QuadNode [] newDepth (nth splitted 0) []))
      (zip/append-child  (->QuadNode [] newDepth (nth splitted 1) []))
      (zip/append-child  (->QuadNode [] newDepth (nth splitted 2) []))
      (zip/append-child  (->QuadNode [] newDepth (nth splitted 3) []))
      )))


(defn findWithBounds [node ^Rectangle inner]
  "takes node and a rectangle and finds the fitting node"
  (let [unzippedNode (zip/node node)
        nodeBounds(:b unzippedNode)
        fitsInThisNode (within? nodeBounds inner)
        subSector (getSector (:b unzippedNode) inner)
        hasChildren (empty? (:c unzippedNode))
        canGoDeeper (< (:d unzippedNode) 6)]
    
;    (println hasChildren)
;    (println fitsInThisNode)
;    (println subSector)
;    (println canGoDeeper)
    
      (match [hasChildren fitsInThisNode subSector canGoDeeper]
             [_ false false _] nil ;throw error
             [true true _ _] node
             ;[true false _ true] subDivide?
             [false _ :nw true] (recur (-> node
                                         zip/down) inner)
             
             [false _ :ne true] (recur (-> node
                                         zip/down
                                         zip/right) inner)
             
             [false _ :sw true] (recur (-> node
                                         zip/down
                                         zip/right
                                         zip/right) inner)
             
             [false _ :se true] (recur (-> node
                                         zip/down
                                         zip/right
                                         zip/right
                                         zip/right) inner)
             :else "omgwtfbbq!!112431"
             )))

(defn zipperCreate [coll]
    (zip/zipper
      branch?
      children
      make-node
      coll))



(defn buildQuadTree [root]
  (let [tree (zipperCreate root)
        buildRec (fn[node]
                   (let [unzippedNode (zip/node node)]
                     (println unzippedNode)
                     (match [unzippedNode] 
                            ;[o d ^Rectangle b c]
                            ;(_ :guard #((< % 6)))
                            [{:o _ :d (_ :guard #(< % 6)) :b _ :c _}] "asdfsf"
                            [_] nil 
                            )   
                     )
                   )
        ]
    (buildRec tree)
     )
  )


(def tR (->Rectangle (->XYPoint (matrix [1 1])) (->XYPoint (matrix [100 100]))))
(def tR2 (->Rectangle (->XYPoint (matrix [6 6])) (->XYPoint (matrix [20 1]))))

(def q (->QuadNode [] 0 (->Rectangle
                                   (->XYPoint (matrix [0 0]))
                                   (->XYPoint (matrix  [200 200]))) []))
(def rzip (zipperCreate q))

(def rtest (zip/root (zip/append-child rzip tR2)))
(def bsZip (subDivide rzip))



