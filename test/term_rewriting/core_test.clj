(ns term-rewriting.core-test
  (:require [clojure.test :refer :all]
            [term-rewriting.core :as core]
            [term-rewriting.term :as term]))

(defn variable [x] (core/variable x))
(defn const [x] (core/constant x))
(defn func [l & args] (core/function l args))

;;; domain ----------------------------------------------------------------------

(def x (variable "x"))
(def y (variable "y"))
(def z (variable "z"))

(def a (const "a"))
(def b (const "b"))
(def c (const "c"))

(defn f [& args] (core/function "f" args))
(defn g [& args] (core/function "g" args))
(defn h [& args] (core/function "h" args))

;;; -----------------------------------------------------------------------------

(deftest mgu-test
  (is (not (core/unifies? [(f (f (g x y) x) z)
                           (f z             x)])))
  (is (core/unifies? [(f a b z)
                      (f a z b)]))
  (is (core/unifies? [(f (g a) (h x)    y      )
                      (f   x     y   (h (g z)) )])))


(deftest substitution-test
  (let [sigma {x (f x y)
               y (g x y)}
        t (h x y)]
    (is (= (h (f x y) (g x y))
           (term/substitute t sigma)))))
