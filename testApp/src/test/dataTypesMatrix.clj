(ns test.dataTypesMatrix
  (:refer-clojure :exclude [* - + == /])
  (:use [clojure.core.match :only (match)]
        [clojure.core.matrix]
        [clojure.core.matrix operators]
        ))


(set! *warn-on-reflection* true)
(set-current-implementation :vectorz) 


(defrecord XYPoint [coords])

(defprotocol RectangleOps
  (split [this] "splits the rectangle into  4 sectors, returns them as vectors")
  (getSector [this ^Rectangle p] "returns the sector within the rectangle belongs")
  (within? [this ^Rectangle p] "checks if the rectangle is complete inside")
  (intersect? [this ^Rectangle p] "checks intersection"))

(defrecord Rectangle [^XYPoint pos ^XYPoint size]
  RectangleOps
  
   (split[this]
     (let [midPoint  (* (:coords size) 0.5)]
       [(Rectangle. (->XYPoint (:coords pos)) (->XYPoint midPoint));oben links
        (Rectangle. (->XYPoint (+ (:coords pos) (* [0.5 0] (:coords size)) )) (->XYPoint (+ (:coords pos) (* [1 0.5] (:coords size)) )));oben rechts
        (Rectangle. (->XYPoint (+ (:coords pos) (* [0 0.5] (:coords size)) )) (->XYPoint (* [0.5 1] (:coords size))));unten links
        (Rectangle. (->XYPoint  midPoint) (->XYPoint (:coords size))) ;unten rechts
        ]))
   
   (within? [this p]
     (let [^XYPoint posThis (:coords (:pos this))
           ^XYPoint sizeThis (:coords(:size this))
           ^XYPoint posP (:coords(:pos ^Rectangle p))
           ^XYPoint sizeP (:coords(:size ^Rectangle p))]
       (and 
         (>= (first posP) (first posThis)) ;linke seite größer als äußeres
         (<= (+ (first sizeP) (first posP)) (+ (first sizeThis) (first posThis))) ;rechte seite kleiner als...
         (>= (second posP) (second posThis)) ;obere seite unter dem äußeren
         (<= (+ (second sizeP) (second posP)) (+ (second sizeThis) (second posThis))) ; untere Seite über dem Äußeren
         )))
   
   
   (intersect? [this p]
     (let [^XYPoint posThis (:coords (:pos ^Rectangle this))
           ^XYPoint sizeThis (:coords(:size ^Rectangle this))
           ^XYPoint posP (:coords(:pos ^Rectangle p))
           ^XYPoint sizeP (:coords(:size ^Rectangle p))]
       (not (or (> (first posThis) (+ (first posP) (first sizeP)))
                (< (+ (first posThis) (first sizeThis)) (first posP))
                (> (second posThis) (+ (second posP) (second sizeP)))
                (< (+ (second posThis) (second sizeThis)) (second posP))))))
   
   ; auf matrizen anpassen
   (getSector [this p]
     (when (within? this p)
       (let [halfP (* [0.5 0.5](:coords (:size ^Rectangle this)))
             ^int vertMid (second halfP)
             ^int horMid (first halfP)
             ^XYPoint posP (:coords (:pos ^Rectangle p))
             ^XYPoint sizeP (:coords (:size ^Rectangle p))
             top? (or 
                    (< (second posP) horMid)
                    (< (+ (second posP) (second sizeP)) horMid))
             left? (or
                     (< (first posP) vertMid)
                     (< (+ (first posP) (first sizeP)) vertMid))]
         
         (match [top? left?]
                [false false] :se
                [false true] :sw
                [true false] :ne
                [true true] :nw
                [_ _] nil))))
   )







