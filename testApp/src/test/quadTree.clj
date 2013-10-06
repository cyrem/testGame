(ns test.quadTree
    (:use [clojure.core.match :only (match)])
    (:require clojure.zip))

(set! *warn-on-reflection* true)


(defn binTree [node val]
  (cond
    (nil? val) {:v nil :l nil :r nil}
    (nil? node) {:v val :l nil :r nil}
    (nil? (:v node)) (assoc node :v val)
    (< val (:v node)) (assoc node :l (binTree (:l node) val))
    :else (assoc node :r (binTree (:r node) val))))


(defn binTree2 [node val ]
  (let [bTL (fn[])]
    
    (if (nil? val)
      node
      
    )
  
  
  (cond
    (nil? val) {:v nil :l nil :r nil}
    (nil? node) {:v val :l nil :r nil}
    (nil? (:v node)) (assoc node :v val)
    (< val (:v node)) (assoc node :l (binTree (:l node) val))
    :else (assoc node :r (binTree (:r node) val)))))


;(binTree (binTree (binTree nil 5) 1) 9)










