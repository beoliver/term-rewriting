(ns term-rewriting.fol-test
  (:require [clojure.test :refer :all]
            [term-rewriting.fol :as fol]
            [term-rewriting.unifiers :as u]
            [term-rewriting.matrix :as matrix]
            [term-rewriting.parsers.leancop :as parser]
            [term-rewriting.emitters.leancop :as emit]
            [term-rewriting.substitutions :as s]
            [term-rewriting.reductions.rules :as reduction-rules]))

(defn variable [x] (fol/variable x))
(defn const [x] (fol/constant x))
(defn func [l & args] (fol/function l args))

;;; domain ----------------------------------------------------------------------

(def x (variable "x"))
(def y (variable "y"))
(def z (variable "z"))

(def a (const "a"))
(def b (const "b"))
(def c (const "c"))

(defn f [& args] (fol/function "f" args))
(defn g [& args] (fol/function "g" args))
(defn h [& args] (fol/function "h" args))

;;; -----------------------------------------------------------------------------

(deftest mgu-test
  (is (not (u/unifies? [(f (f (g x y) x) z)
                        (f z             x)])))
  (is (u/unifies? [(f a b z)
                   (f a z b)]))
  (is (u/unifies? [(f (g a) (h x)    y      )
                   (f   x     y   (h (g z)) )])))

(deftest substitution-test
  (let [sigma {x (f x y)
               y (g x y)}
        t (h x y)]
    (is (= (fol/equality a (h (f x y) (g x y)))
           (s/substitute (fol/equality a t) sigma)))))
