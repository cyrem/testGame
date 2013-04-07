(ns test.quadTree
  (:require [clojure.zip :as zip]))

(def dadum [{:x 4 :y 35 :v "sffsfda copy"}
            {:x 5 :y 40 :v "sdfaf"}
            {:x 11 :y 55 :v "sdfaf1"}
            {:x 99 :y 22 :v "sdfaf2"}
            {:x 100 :y 100 :v "sdfaf3"}])

(defn subDiv [[^int minX ^int minY ^int maxX ^int maxY]]
  (let
    [midX (int (/ (+ minX maxX) 2))
     midY (int (/ (+ minY maxY) 2))]
    
    {:nw [minX minY midX midY]
     :ne [midX midY maxX midY]
     :sw [minX midY midX maxY]
     :se [midY midY maxX maxY]}))

(defn rectIntersect [[minX minY maxX maxY] [minRectX minRectY maxRectX maxRectY]] 
  (not (or (> minRectX maxX)
           (< maxRectX minX)
           (> minRectY maxY)
           (< maxRectY minY)))) 

(defn selectSubNode [[midX midY] x y]
  (cond
    (and (< x midX) (< y midY)) :sw
    (and (< x midX) (> y midY)) :nw
    (and (> x midX) (< y midY)) :se
    (and (> x midX) (> y midY)) :ne))

(defrecord node [bounds vals nw ne sw se])

(def coords [1 1 100 100])

(defn insQuad [node items bounds]
  (cond 
    (empty? items) node
    (empty? node) (let [[i & r] items 
                        [^int minX ^int minY ^int maxX ^int maxY] bounds
                        midX (int (/ (+ minX maxX) 2))
                        midY (int (/ (+ minY maxY) 2))
                        selected (selectSubNode [midX midY] (:x i) (:y i))
                        subDived (subDiv bounds)]
                    (recur (node. bounds i nil nil nil nil) r bounds))
    :else (let [[i & r] items 
                [^int minX ^int minY ^int maxX ^int maxY] bounds
                midX (int (/ (+ minX maxX) 2))
                midY (int (/ (+ minY maxY) 2))
                selected (selectSubNode [midX midY] (:x i) (:y i))
                subDived (subDiv bounds)]
            (recur (get node selected) r (get subDived selected)))))



(definterface ITreeNode
  (^long item [])
  (left [])
  (right []))

(deftype TreeNode [left right ^long item]
  ITreeNode
  (item [this] item)
  (left [this] left)
  (right [this] right))



(defn children [node])
(defn make-node [node])



(def dfas '(() (()(()() 3) 2) 1))

(def blub (zip/zipper #(list? %1) 
                      #(list (nth %1 1) (nth %1 2)) 
                      #(if (< %2 (nth %1 3))
                         (list %2 (nth %1 2) (nth %1 3))
                         (list (nth %1 1) %2 (nth %1 3)))
                      dfas))








