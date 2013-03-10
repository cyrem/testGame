(ns test.quadTree)


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

(defrecord node [bound v nw ne sw se])

(comment (defn quadIns [node x y v]
  (cond
    (empty? node) (node. x y v nil nil nil nil)
    :else (let [selectedNode (selectSubNode node x y)]
            (assoc node selectedNode (quadIns (get node selectedNode) x y v)) ))))


(def dadum [{:x 4 :y 35 :v "sffsfda copy"}
            {:x 5 :y 40 :v "sdfaf"}
            {:x 11 :y 55 :v "sdfaf1"}
            {:x 99 :y 22 :v "sdfaf2"}
            {:x 100 :y 100 :v "sdfaf3"}
            ])


(defn createQuad [items depth [^int minX ^int minY ^int maxX ^int maxY]]
  (when (> depth 0)
    (let
    [midX (int (/ (+ minX maxX) 2))
     midY (int (/ (+ minY maxY) 2))
     bounds [midX midY]
     filtered (reduce (fn [acu vals]
                        (let [sec (selectSubNode bounds (:x vals) (:y vals))]
                          (case sec
                            :nw (assoc acu :1 (conj (get acu 1)vals))
                            :ne (assoc acu :2 (conj (get acu 2)vals))
                            :sw (assoc acu :3 (conj (get acu 3)vals))
                            :se (assoc acu :4 (conj (get acu 4)vals)))
                          )) {:nw [] :ne [] :sw [] :se []} items)]
    
    (node. [minX minY maxX maxY] items
           (createQuad (:nw filtered) (dec depth) [minX minY midX midY])
           (createQuad (:ne filtered) (dec depth) [midX midY maxX midY])
           (createQuad (:sw filtered) (dec depth) [minX midY midX maxY])
           (createQuad (:se filtered) (dec depth) [midY midY maxX maxY])))))

(def dadi (createQuad dadum 3 [1 1 100 100]))

