(ns test.quadTree
    (:require [clojure.zip :as zip]
              [test.datatypes])
    (:use [clojure.core.match :only (match)]
          [test.datatypes])
    (:import [test.datatypes Rectangle XYPoint]))

(set! *warn-on-reflection* true)

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
        splitted (split (:b unzippedNode))
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
    
      (match [hasChildren fitsInThisNode subSector canGoDeeper]
             [true false false _] nil ;throw error
             [true true _ _] node
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
  (let [tree  (zipperCreate root)
        ]
   
    )
  
  (match [root]
         ;[o d ^Rectangle b c]
         [nil] nil
         [{:o _ :d (_ :guard #(< % 6)) :b _ :c _}] "asdfsf"
         )
  )


(def rzip 
  (zipperCreate (->QuadNode [] 0 (->Rectangle (->XYPoint 0 0) (->XYPoint 200 200)) [])))

(def rtest (zip/root (zip/append-child rzip tR2)))

(def bsZip (subDivide rzip))



