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



(deftype QuadNode [o ^Rectangle b c]
  Object
  (toString [this]
    (pr-str "QuadNode:
 o:" o "
 b: " b "
 c: " c))
  
  ZipOps
  (branch? [node]
    true)
  (children[node]
    (.c node))
  (make-node[node children]
    (QuadNode. (.o node) (.b node) children)))


  (defn insert [node val])
  (defn delete [node val])
  (defn subDivide[node]
   ; (let [[nw ne sw se] (split (.b node))])
    )
  
  (defn findInBounds [node rec]
    ;this = root
    ;(->Rectangle (->XYPoint 25 25) (->XYPoint 4 5))
    
    (let [ rec1 ^Rectangle (.b (^QuadNode zip/node node))]
      
      (println rec1)
      )
    )


(def rzip (zip/zipper
            branch?
            children
            make-node
            (->QuadNode [] (->Rectangle (->XYPoint 0 0) (->XYPoint 200 200)) [])))




