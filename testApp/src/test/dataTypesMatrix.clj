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

(defprotocol RectangleOps
  (split [this] "splits the rectangle into  4 sectors, returns them as vectors")
  (getSector [this p] "returns the sector within the rectangle belongs")
  (within? [this p] "checks if the rectangle is complete inside")
  (intersect? [this p] "checks intersection"))




(deftype Rectangle [^XYPoint pos ^XYPoint size]
  RectangleOps
  (split[this]
    (let [midPoint  (* (.coords size) 0.5)]
      [(Rectangle. (->XYPoint (.coords pos)) (->XYPoint midPoint));oben links
       (Rectangle. (->XYPoint (+ (.coords pos) (* [0.5 0] (.coords size)) )) (->XYPoint  (* [1 0.5] (.coords size)) ));oben rechts
       (Rectangle. (->XYPoint (+ (.coords pos) (* [0 0.5] (.coords size)) )) (->XYPoint (* [0.5 1] (.coords size))));unten links
       (Rectangle. (->XYPoint  midPoint) (->XYPoint (:coords size))) ;unten rechts
       ]))
  
  (within? [^Rectangle this p]
    (let [^XYPoint posThis (.coords ^XYPoint (.pos this))
          ^XYPoint sizeThis (.coords ^XYPoint(.size this))
          ^XYPoint posP (.coords ^XYPoint(.pos ^Rectangle p))
          ^XYPoint sizeP (.coords ^XYPoint(.size ^Rectangle p))]
      (and 
        (>= (first posP) (first posThis)) ;linke seite größer als äußeres
        (<= (+ (first sizeP) (first posP)) (+ (first sizeThis) (first posThis))) ;rechte seite kleiner als...
        (>= (second posP) (second posThis)) ;obere seite unter dem äußeren
        (<= (+ (second sizeP) (second posP)) (+ (second sizeThis) (second posThis))) ; untere Seite über dem Äußeren
      )
      ))
  
   
   (intersect? [this p]
     (let [^XYPoint posThis (.coords ^XYPoint(.pos ^Rectangle this))
           ^XYPoint sizeThis (.coords ^XYPoint(.size ^Rectangle this))
           ^XYPoint posP (.coords ^XYPoint(.pos ^Rectangle p))
           ^XYPoint sizeP (.coords ^XYPoint(.size ^Rectangle p))]
       (not (or (> (first posThis) (+ (first posP) (first sizeP)))
                (< (+ (first posThis) (first sizeThis)) (first posP))
                (> (second posThis) (+ (second posP) (second sizeP)))
                (< (+ (second posThis) (second sizeThis)) (second posP))))
       ))
   
   ; auf matrizen anpassen
   (getSector [this p]
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
   
   Object
   (toString [_] 
     (pr-str pos size))
   )





(defmacro with-rec-access[^Rectangle this ^Rectangle p & body]
      `(let [^XYPoint ~'posThis (.coords ^XYPoint (.pos  ~this))
          ^XYPoint ~'sizeThis (.coords ^XYPoint(.size ~this))
          ^XYPoint ~'posP (.coords ^XYPoint(.pos  ~p))
          ^XYPoint ~'sizeP (.coords ^XYPoint(.size  ~p))]
         ~@body
         )
  )

(defn trulyWithn? [^Rectangle this ^Rectangle p]
  (with-rec-access this p
                         (not (or (> (first posThis) (+ (first posP) (first sizeP)))
                         (< (+ (first posThis) (first sizeThis)) (first posP))
                         (> (second posThis) (+ (second posP) (second sizeP)))
                         (< (+ (second posThis) (second sizeThis)) (second posP))))
                         )
  )







