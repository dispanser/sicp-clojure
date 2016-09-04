(ns sicp-chapter2)

;; Section 2 - Building Abstractions with Data
(defn linear-combination [a b x y]
  (+ (* a x) (* b y)))

;; second example with generalized add and mul, doesn't compile in clojure.
(defn linear-combination [a b x y]
  (add (mul a x) (mul b y)))

;; Section 2.1 - Introduction to Data Abstraction

;; Section 2.1.1 - Example: Arithmetic Operations for Rational Numbers

;; the sample code in this section assumes that we do have make-rat, nomer and denom
;; to build rational numbers and extract the nominator and denominator respectively.
;; Since clojure is a little picky here and doesn't let us define add-rat without
;; previously defining make-rat and friends, we're just building some preliminary
;; implementations based on a vector with two elements.
(defn make-rat [nomer denom] [nomer denom])
(def nomer first)
(def denom second)

(defn add-rat [x y]
  (make-rat (+ (* (nomer x) (denom y))
               (* (nomer y) (denom x)))
            (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (nomer x) (denom y))
               (* (nomer y) (denom x)))
            (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat (* (nomer x) (nomer y))
            (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat (* (nomer x) (denom y))
            (* (nomer y) (denom x))))
(defn equal-rat? [x y]
  (= (* (nomer x) (denom y))
     (* (nomer y) (denom x))))
(defn print-rat [x]
  (println (nomer x) "/" (denom x)))

;; copied from section 1.2.5
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn make-rat [nomer denom]
  (let [d (gcd nomer denom)]
    [(quot nomer d) (quot denom d)]))

;; ex 2.1
;; Q: improve make-rat to normalize the representation of positive numbers
;;    (both nomer and denom positive) and negative numbers (nomer negative,
;;    denom positive).
;; A: works for me. gcd seems to handle all the cases properly:
(make-rat -1 -2) ;; [1 2]
(make-rat -1 2)  ;; [-1 2]
(make-rat 1 -2)  ;; [-1 2]

;; ex 2.2
;; Q: represent line segments as pairs of start point and end-point, create data
;;    abstraction for segment, based on a data abstraction for points, and a
;;    procedure midpoint-segment that copmutes the midpoint for a line segment.
;; A:
(defn make-segment [s e] [s e])
(def start-segment first)
(def end-segment second)

(defn make-point [x y] [x y])
(def x-point first)
(def y-point second)

(defn print-point [p]
  (print "(" (x-point p) "," (y-point p) ")"))

;; copied from chapter 1, ex 1.34 solution - averages a list of values
(defn avg [ & args] (/ (reduce + args) (count args)))

(defn midpoint-segment [s]
  (let [start-point (start-segment s)
        end-point (end-segment s)]
    (make-point (avg (x-point start-point )
                     (x-point end-point))
                (avg (y-point start-point)
                     (y-point end-point)))))

;; ex 2.3
;; Q: design representation for rectangles on a plane, and provide procedures
;;    to compute perimeter and area.
;;    Can you design your methods in a way that they also work with a different
;;    representation?
;; A: I'm assuming that the rectangle is aligned with horizontal / vertical lines
;;    (hence the 'on a plane'), so implementing via lower-left and upper-right
;;    corner is sufficient.
;;    A second representation will use lower-left corner and width + height.
(defn make-rectangle [p1 p2] [p1 p2])
(def rect-lower-left first)
(def rect-upper-right second)
(defn rect-width [r]
  (let [p1 (rect-lower-left r)
        p2 (rect-upper-right r)]
    (- (x-point p2) (x-point p1))))
(defn rect-height [r]
  (let [p1 (rect-lower-left r)
        p2 (rect-upper-right r)]
    (- (y-point p2) (y-point p1))))

(defn rect-perimeter [r]
  (+ (* 2 (rect-width r))
     (* 2 (rect-height r))))

(defn rect-area [r]
  (* (rect-width r)
     (rect-height r)))

;; second representation; rect-perimeter and rect-area keep working without changes.
(defn make-rectangle [p1 width height] [p1 width height])
(def rect-width second)
(defn rect-height [r] (r 2))
