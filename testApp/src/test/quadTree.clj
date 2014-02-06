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

(defn zipperCreate [coll]
    (zip/zipper
      branch?
      children
      make-node
      coll))

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
      (zip/root)
      (zipperCreate)
      )))


(defn findWithBounds [node ^Rectangle inner]
  "takes node and a rectangle and finds the fitting node"
  (let [unzippedNode (zip/node node)
        nodeBounds(:b unzippedNode)
        fitsInThisNode (within? nodeBounds inner)
        subSector (getSector (:b unzippedNode) inner)
        childrenEmpty? (empty? (:c unzippedNode))
        canGoDeeper (< (:d unzippedNode) 6)]
;    (println hasChildren)
;    (println fitsInThisNode)
;    (println subSector)
;    (println canGoDeeper)
      (match [childrenEmpty? fitsInThisNode subSector canGoDeeper]
             [_ false false _] nil ;throw error
             [_ true false _] node
             ;[true false _ true] (recur (subDivide node) inner)
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
             :else "omgwtfbbq!!112431"
             )))

;(defn buildQuadTree [root]
;  (let [tree (zipperCreate root)
;        buildRec (fn[node]
;                   (let [unzippedNode (zip/node node)]
;                     (println unzippedNode)
;                     (match [unzippedNode] 
;                            ;[o d ^Rectangle b c]
;                            ;(_ :guard #((< % 6)))
;                            [{:o _ :d (_ :guard #(< % 6)) :b _ :c _}] "asdfsf"
;                            [_] nil 
;                            )   
;                     )
;                   )
;        ]
;    (buildRec tree)
;     )
;  )

(defn buildQuadTree [node]
  ;terminierede funktion!!111111h2312312hfdsaföajsökfgöhs
  (match [node]
         [{:o _ :d (_ :guard #(< % 4)) :b _ :c empty?}] (let [splitted (split ^Rectangle (:b node))
                                                              newDepth (inc (:d node))]
                                                          (recur (assoc node :c [(->QuadNode [] newDepth (nth splitted 0) [])
                                                                                 (->QuadNode [] newDepth (nth splitted 1) [])
                                                                                 (->QuadNode [] newDepth (nth splitted 2) [])
                                                                                 (->QuadNode [] newDepth (nth splitted 3) [])])))
         [{:o _ :d (_ :guard #(< % 4)) :b _ :c (_ :guard #(not (empty? %)))}] (mapv buildQuadTree (:c node))
         [_] nil
         )
  
; (cond (and (< (:d node) 4)
;            (empty? (:c node))) (let [splitted (split ^Rectangle (:b node))
;                                      newDepth (inc (:d node))]
;                                  (recur (assoc node :c [(->QuadNode [] newDepth (nth splitted 0) [])
;                                                  (->QuadNode [] newDepth (nth splitted 1) [])
;                                                  (->QuadNode [] newDepth (nth splitted 2) [])
;                                                  (->QuadNode [] newDepth (nth splitted 3) [])])))
;      )
)


(def tR (->Rectangle (->XYPoint (matrix [1 1])) (->XYPoint (matrix [100 100]))))
(def tR2 (->Rectangle (->XYPoint (matrix [6 6])) (->XYPoint (matrix [20 1]))))

(def q (->QuadNode [] 0 (->Rectangle
                                   (->XYPoint (matrix [0 0]))
                                   (->XYPoint (matrix  [200 200]))) []))

(def rzip (zipperCreate q))

(def bsZip (subDivide rzip))



