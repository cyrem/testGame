(ns test.datatypes
  (:refer-clojure :exclude [* - + == /])
  (:use [clojure.core.match :only (match)]
        [clojure.core.matrix operators]))

;Koordinaten fangen mit 1,1 oben links an. max ist unten rechts

(set! *warn-on-reflection* true)

(defprotocol RectangleOps
  (getVertMidpoint[this])
  (getHorMidpoint [this])
  (split [this] "splits the rectangle into  4 sectors, returns them as vectors")
  (getSector [this ^Rectangle p] "returns the sector within the rectangle belongs")
  (within? [this ^Rectangle p] "checks if the rectangle is complete inside")
  (intersect? [this ^Rectangle p] "checks intersection")
  )

(deftype XYPoint [^int x ^int y]
  Object
  (toString [this]
    (pr-str "XYPoint: x: " x " y: " y))
  
    clojure.core.match.protocols/IMatchLookup
    (val-at [this k not-found]
      (case k
        :x (.x this)
        :y (.y this)
        not-found)))

(deftype Rectangle [^XYPoint pos ^XYPoint size]
  Object
  (toString [this]
     (pr-str "Rectangle: pos: " pos " size: " size))
  
  clojure.core.match.protocols/IMatchLookup
  (val-at [this k not-found]
    (case k
      :pos (.pos this)
      :size (.size this)
      not-found))
  
  RectangleOps
  (getVertMidpoint [this]
    (int (/ (+ (.x pos)(.x size)) 2)))
  
  (getHorMidpoint[this]
    (int (/ (+ (.y pos)(.y  size)) 2)))
  
  (split[this]
    (let [subWidth (getVertMidpoint this)
          subHeight (getHorMidpoint this)
          x (.x pos)
          y (.y pos)]
          [(Rectangle. (->XYPoint  x y)(->XYPoint subWidth subHeight))
           (Rectangle. (->XYPoint (+ x subWidth) y)(->XYPoint subWidth subHeight))
           (Rectangle. (->XYPoint x (+ subHeight y))(->XYPoint subWidth subHeight))
           (Rectangle. (->XYPoint (+ x subWidth) (+ subHeight y))(->XYPoint subWidth subHeight))]))
  
  (within? [this p]
    (let [^XYPoint posThis (.pos this)
          ^XYPoint sizeThis (.size this)
          ^XYPoint posP (.pos ^Rectangle p)
          ^XYPoint sizeP (.size ^Rectangle p)]
      (and 
        (>= (.x posP) (.x posThis)) ;linke seite größer als äußeres
        (<= (+ (.x sizeP) (.x posP)) (+ (.x sizeThis) (.x posThis))) ;rechte seite kleiner als...
        (>= (.y posP) (.y posThis)) ;obere seite unter dem äußeren
        (<= (+ (.y sizeP) (.y posP)) (+ (.y sizeThis) (.y posThis))) ; untere Seite über dem Äußeren
        )))
  
  (intersect? [this p]
    (let [^XYPoint posThis (.pos ^Rectangle this)
          ^XYPoint sizeThis (.size ^Rectangle this)
          ^XYPoint posP (.pos ^Rectangle p)
          ^XYPoint sizeP (.size ^Rectangle p)]
      (not (or (> (.x posThis) (+ (.x posP) (.x sizeP)))
               (< (+ (.x posThis) (.x sizeThis)) (.x posP))
               (> (.y posThis) (+ (.y posP) (.y sizeP)))
               (< (+ (.y posThis) (.y sizeThis)) (.y posP))))))
  
  (getSector [this p]
    (let [^int vertMid (getVertMidpoint this)
          ^int horMid (getHorMidpoint this)
          ^XYPoint posP (.pos ^Rectangle p)
          ^XYPoint sizeP (.size ^Rectangle p)
          top? (or 
                 (< (.y posP) horMid)
                 (< (+ (.y posP) (.y sizeP)) horMid))
          left? (or
                  (< (.x posP) vertMid)
                  (< (+ (.x posP) (.x sizeP)) vertMid))]
      
      (match [top? left?]
             [false false] :se
             [false true] :sw
             [true false] :ne
             [true true] :nw))))


(def tR (->Rectangle (->XYPoint 1 1) (->XYPoint 99 99)))
(def tR2 (->Rectangle (->XYPoint 25 25) (->XYPoint 4 5)))





