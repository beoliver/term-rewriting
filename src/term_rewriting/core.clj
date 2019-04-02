(ns term-rewriting.core
  (:require [term-rewriting.term :as term]
            [term-rewriting.substitutions :as substitutions]
            [term-rewriting.unifiers :as unifiers]))

(defrecord Variable [vlabel]
  term/ITerm
  (variable?  [term] true)
  (function?  [term] false)
  (label      [term] vlabel)
  (args       [term] nil)
  (vars       [term] #{term})
  (subterms   [term] #{term})
  (substitute [term sigma] (substitutions/lookup sigma term)))

(defn variable [label] (->Variable label))

(defrecord Function [flabel fargs]
  term/ITerm
  (variable?  [term] false)
  (function?  [term] true)
  (label      [term] flabel)
  (args       [term] fargs)
  (vars       [term]
    (reduce
     (fn [xs arg]
       (into xs (term/vars arg))) #{} fargs))
  (subterms   [term]
    (reduce
     (fn [xs arg]
       (into xs (term/subterms arg))) #{term} fargs))
  (substitute [term sigma]
    (let [term' (substitutions/lookup sigma term)]
      (if (= term term')
        (update term :fargs #(map (fn [arg] (term/substitute arg sigma)) %))
        term'))))

(defn function [label args] (->Function label (seq args)))

(defn constant [label] (->Function label nil))

(defn mgu
  "each pair is a vector [s t]. Returns a mgu if one exists."
  [& pairs]
  (apply unifiers/most-general-unifier-martelli-and-montanari-algorithm-1 pairs))

(defn unifies? [& pairs]
  (boolean (apply mgu pairs)))
