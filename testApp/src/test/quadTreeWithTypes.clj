(ns test.quadTreeWithTypes
  (:require [clojure.zip :as zip]
            [test.dataTypesMatrix :as dTM]
            )
  (:use [clojure.core.match :only [match]]
        [test.dataTypesMatrix]
        )
  (:import [test.dataTypesMatrix Rectangle XYPoint]  ))

; (ns stuff
;      (:use [com.example-ns :only [IFoo])
;      (:import [com.example-ns Foo])) 

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
                                                       (println "< " (.d node))
                                                       (println splitted)
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


;(def bQuad (buildQuadTree q))

;(->QuadNode 1 1 [][])
;(def exampleQuad (->QuadNode 0 1 [(->QuadNode 1 1 ["a"][])(->QuadNode 1 1 ["b"][])(->QuadNode 1 1 ["c"][])(->QuadNode 1 1 ["d"][])] []))



