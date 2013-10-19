(ns test.quadTree
    (:require [clojure.zip :as zip]
              [test.datatypes])
    (:use [clojure.core.match :only (match)])
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


  (defn insert [node val]
    
;        (let [outer ^Rectangle (.b (^QuadNode zip/node node))]
;    (when (within? outer inner)
;    
;    (let [[nw ne sw se] (split outer)
;          checkNw (within? nw inner)
;          checkNe (within? ne inner)
;          checkSw (within? sw inner)
;          checkSe (within? se inner)]
;
;      (match [checkNw checkNe checkSw checkSe]
;             [false false false false] ; insert in current node
;             [true true true true]
;             
;      )))
;    )
)
  
  (defn delete [node val])
  (defn subDivide[loc]
   (
     
    ))
  
  (defn findWithBounds [node ^Rectangle inner]
    "takes node and a rectangle and finds the fitting node"
    (let [unzippedNode (zip/node node)
          nodeBounds(:b unzippedNode)
          fitsInThisNode (within? nodeBounds inner)
          subSector (getSector(:b unzippedNode) inner)
          hasChildren(empty? (:c unzippedNode))
          ]
(println hasChildren)
      
      (match [hasChildren fitsInThisNode subSector]
             [true false _] nil
             [_ false _] nil
             [_ true false] node
             [false true :nw] (recur (-> node
                                       zip/down) inner)
             [false true :ne] (recur (-> node
                                       zip/down
                                       zip/right) inner)
             [false true :sw] (recur (-> node
                                       zip/down
                                       zip/right
                                       zip/right) inner)
             [false true :se] (recur (-> node
                                       zip/down
                                       zip/right
                                zip/right
                                zip/right) inner)
             [_ _ _] "omgwtfbbq!!112431"
             )
      ;this = root
    ;(->Rectangle (->XYPoint 25 25) (->XYPoint 4 5))

    ))

  (defn zipperCreate [coll]
    (zip/zipper
            branch?
            children
            make-node
            coll))

   
  (def rzip 
  (zipperCreate (->QuadNode [] 0 (->Rectangle (->XYPoint 0 0) (->XYPoint 200 200)) [])))
  (def rtest (zip/root (zip/append-child rzip tR2)))
  
  (def bsZip (let [ unzippedNode (zip/node rzip)
                   splitted (split (:b unzippedNode))]
               (-> rzip
                 (zip/append-child  (->QuadNode [] 1 (nth splitted 0) []))
                 (zip/append-child  (->QuadNode [] 1 (nth splitted 1) []))
                 (zip/append-child  (->QuadNode [] 1 (nth splitted 2) []))
                 (zip/append-child  (->QuadNode [] 1 (nth splitted 3) []))
                 zip/root
                 )))
  





