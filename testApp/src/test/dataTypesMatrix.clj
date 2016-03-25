(ns test.dataTypesMatrix
  (:refer-clojure :exclude [* - + == /])
  (:use [clojure.core.match :only (match)]
        [clojure.core.matrix]
        [clojure.core.matrix operators]))

(set! *warn-on-reflection* true)
'(set-current-implementation :vectorz) 


(deftype XYPoint [coords]
  Object
  (toString [_] 
           (pr-str coords)))


(deftype Rectangle [^XYPoint pos ^XYPoint size]
  Object
  (toString [_] 
    (pr-str pos size)))


(defmacro with-rec-access[recBody recInside & body]
  `(let [posThis# (.coords ^XYPoint (.pos ~recBody))
         sizeThis# (.coords ^XYPoint (.size ~recBody))
         posP# (.coords ^XYPoint (.pos ~recInside))
         sizeP# (.coords ^XYPoint (.size ~recInside))
         
         ~'maxRecBodyX (+ (first posThis#) (first sizeThis#))
         ~'maxRecBodyY (+ (second posThis#) (second sizeThis#))
         ~'minRecBodyX (first posThis#)
         ~'minRecBodyY (second posThis#)
         
         ~'maxRecInsideX (+ (first posP#) (first sizeP#))
         ~'maxRecInsideY (+ (second posP#) (second sizeP#))
         ~'minRecInsideX (first posP#)
         ~'minRecInsideY (second posP#)]
     ~@body))

(defn split [^Rectangle rec]
  "splits the rectangle into  4 sectors, returns them as vectors"
(let [pos (.pos rec)
      size (.size rec)
      midPoint  (* (.coords ^XYPoint size) 0.5)]
    [(Rectangle. (->XYPoint (.coords ^XYPoint pos)) (->XYPoint midPoint));oben links
   (Rectangle. (->XYPoint (+ (.coords ^XYPoint pos) (* [0.5 0] (.coords ^XYPoint size)) )) (->XYPoint  (* [1 0.5] (.coords ^XYPoint size)) ));oben rechts
   (Rectangle. (->XYPoint (+ (.coords ^XYPoint pos) (* [0 0.5] (.coords ^XYPoint size)) )) (->XYPoint (* [0.5 1] (.coords ^XYPoint size))));unten links
   (Rectangle. (->XYPoint  midPoint) (->XYPoint (.coords ^XYPoint size))) ;unten rechts
   ]))

(defn within? [^Rectangle this ^Rectangle p] 
  "checks if the rectangle is complete inside"
(with-rec-access this p
  (and
    (>= minRecInsideX minRecBodyX)
    (<= maxRecInsideX maxRecBodyX)
    (<= maxRecInsideY maxRecBodyY)
    (>= minRecInsideY minRecBodyY))))

(defn intersect? [^Rectangle this ^Rectangle p] 
  "checks intersection"
  (with-rec-access this p
    (not (or (> minRecBodyX maxRecInsideX)
             (< maxRecBodyX minRecInsideX)
             (> minRecBodyY maxRecInsideY)
             (< maxRecBodyY minRecInsideY))
)))

(defn getSector [^Rectangle this ^Rectangle p] 
  "returns the sector within the rectangle belongs"
  (when (within? this p)
       (let [halfP (* [0.5 0.5](.coords ^XYPoint (.size ^Rectangle this)))
             ^int vertMid (second halfP)
             ^int horMid (first halfP)
             ^XYPoint posP (.coords ^XYPoint (.pos ^Rectangle p))
             ^XYPoint sizeP (.coords ^XYPoint (.size ^Rectangle p))
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







