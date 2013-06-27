(ns test.quadTree
  (:require [clojure.zip :as zip]
            ))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)

(defprotocol QuadOperations
  (insert [this v] "inserts a new point")
  (queryRange [this r] "finds points in the supplied range")
  )

(defrecord XY [x y])
(defrecord AABB [center halfD])


(defrecord QuadTree [boundary nW nE sW sE]
  QuadOperations
  (insert [this v] (println this))
  (queryRange[_ r])
  )




















(comment




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

(defn selectSubNode [[midX midY] [x y]]
  (cond
    (and (<= x midX) (<= y midY)) :sw
    (and (<= x midX) (> y midY)) :nw
    (and (> x midX) (<= y midY)) :se
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

(defn createBinZip [in]
  (zip/zipper (fn [_] true) 
              (fn [node] (list (:l node) (:r node)))
              (fn  [node children] 
                (BinNode. (:v node) (first children)(second children)))
              in))

(def testbin (BinNode. 1 (BinNode. 2 nil nil) (BinNode. 3 (BinNode. 4 nil nil) nil)))

(defn getPosBin [node value]
  (cond
    (or (empty? node) 
        (nil? (:v (zip/node node)))) node
    (< value (:v (zip/node node))) (recur (zip/down node) value)
    :else (recur (zip/right(zip/down node)) value)))

(defn binIns [node val]
  (let [node (createBinZip node)]
    (zip/root(zip/edit (getPosBin node val) (fn [_] (BinNode. val nil nil))))))



(defrecord QuadNode [v b nw ne sw se])

(def testquad (QuadNode. [1 1] [1 1 100 100] nil nil nil nil ))


(defn createQuadZip [in]
  (zip/zipper (fn [_] true) 
              (fn [node] (let [{:keys [v b nw ne sw se]} node] [b nw ne sw se]))
              (fn  [node [_ b nw ne sw se]]
                 (QuadNode. (:v node) b nw ne sw se))
              in))

(def zipQuad (createQuadZip testquad))

(defn getPosQuad [node coords [^int minX ^int minY ^int maxX ^int maxY]]
  (cond
    (or (empty? node) 
        (nil? (:v (zip/node node)))) [node [minX minY maxX maxY]]
    :else (let [iNode (zip/node node)
                midX  (int (/ (+ minX maxX) 2))
                midY  (int (/ (+ minY maxY) 2))
                selected (selectSubNode [midX midY] coords)
                out (case selected
                      :nw (zip/right (zip/down node))
                      :ne (zip/right (zip/right (zip/down node)))
                      :sw (zip/right (zip/right (zip/right (zip/down node))))
                      :se (zip/right (zip/right (zip/right (zip/right (zip/down node))))))
                outB (case selected
                       :nw [minX minY midX midY]
                       :ne [midX midY maxX midY]
                       :sw [minX midY midX maxY]
                       :se [midY midY maxX maxY])
                ]
            (recur out coords outB))))
  

(defn quadIns [node [val bounds]]
  (let [node (createQuadZip node)]
    (zip/root (let [[pos b] (getPosQuad node val bounds)] (zip/edit pos (fn [_] (QuadNode. val b nil nil nil nil)))))))


(def dadim [
            
            [[5 5] [1 1 100 100]]
            [[7 12] [1 1 100 100]]
            [[8 55] [1 1 100 100]]
            [[55 53] [1 1 100 100]]
            [[5 22] [1 1 100 100]]
            
            ])









)






