(ns test.quadTree
  (:require [clojure.zip :as zip]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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

;(defrecord node [bounds vals nw ne sw se])
;
;(def coords [1 1 100 100])
;
;(defn insQuad [node items bounds]
;  (cond 
;    (empty? items) node
;    (empty? node) (let [[i & r] items 
;                        [^int minX ^int minY ^int maxX ^int maxY] bounds
;                        midX (int (/ (+ minX maxX) 2))
;                        midY (int (/ (+ minY maxY) 2))
;                        selected (selectSubNode [midX midY] (:x i) (:y i))
;                        subDived (subDiv bounds)]
;                    (recur (node. bounds i nil nil nil nil) r bounds))
;    :else (let [[i & r] items 
;                [^int minX ^int minY ^int maxX ^int maxY] bounds
;                midX (int (/ (+ minX maxX) 2))
;                midY (int (/ (+ minY maxY) 2))
;                selected (selectSubNode [midX midY] (:x i) (:y i))
;                subDived (subDiv bounds)]
;            (recur (get node selected) r (get subDived selected)))))

(defrecord BinNode [v l r])

(defn createZip [in]
  (zip/zipper (fn [_] true) 
              (fn [node] (list (:l node) (:r node)))
              (fn  [node children] 
                (BinNode. (:v node) (first children)(second children)))
              in))

(def testbin (BinNode. 1 (BinNode. 2 nil nil) (BinNode. 3 (BinNode. 4 nil nil) nil)))


(defn getPos [node value]
  (cond
    (or (empty? node) 
        (nil? (:v (zip/node node)))) node
    (< value (:v (zip/node node))) (recur (zip/down node) value)
    :else (recur (zip/right(zip/down node)) value)))

(defn quadIns [node val]
  (let [node (createZip node)]
    (zip/root(zip/edit (getPos node val) (fn [_] (BinNode. val nil nil))))))


(defn massIns [dataS items]
  (reduce quadIns items))

 







