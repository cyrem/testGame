(ns test.quadTree
    (:require [clojure.zip :as zip]
              [test.datatypes])
    (:use [clojure.core.match :only (match)]
          [test.datatypes])
    (:import [test.datatypes Rectangle]
             [test.datatypes XYPoint])
    )


(set! *warn-on-reflection* true)

(defprotocol ZipOps 
  (branch? [node])
  (children[node])
  (make-node[node children]))



(defrecord QuadNode [o ^Rectangle b c]
  ZipOps
  (branch? [node]
    true)
  (children[node]
    (:c node))
  (make-node[node children]
    (QuadNode. (:o node) (:b node) children)))


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
    )
  
  (defn findInBounds [node inner]
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
  (zipperCreate (->QuadNode [] (->Rectangle (->XYPoint 0 0) (->XYPoint 200 200)) [])))
  (def rtest (zip/root (zip/append-child rzip tR2)))
  
  (def bsZip (let [ unzippedNode (zip/node rzip)
                   splitted (split (:b unzippedNode))]
               (-> rzip
                 (zip/append-child  (->QuadNode [] (nth splitted 0) []))
                 (zip/append-child  (->QuadNode [] (nth splitted 1) []))
                 (zip/append-child  (->QuadNode [] (nth splitted 2) []))
                 (zip/append-child  (->QuadNode [] (nth splitted 3) []))
                 zip/root
                 )))
  





