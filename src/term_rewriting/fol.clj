(ns term-rewriting.fol
  (:require [term-rewriting.term :as term]
            [term-rewriting.predicate :as pred]
            [term-rewriting.substitutions :as substitutions]
            [term-rewriting.unifiers :as unifiers]
            [clojure.set :as set]))

(defrecord Variable [vlabel]
  term/ITerm
  (variable?  [term] true)
  (function?  [term] false)
  (label      [term] vlabel)
  (args       [term] nil)
  (vars       [term] #{term})
  (subterms   [term] #{term})
  substitutions/ISubstitutable
  (substitute [term sigma] (substitutions/lookup sigma term)))

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
  substitutions/ISubstitutable
  (substitute [term sigma]
    (let [term' (substitutions/lookup sigma term)]
      (if (= term term')
        (update term :fargs #(seq (map (fn [arg] (substitutions/substitute arg sigma)) %)))
        term'))))

(defrecord Predicate [plabel pargs sign]
  pred/IPredicate
  (label [pred] plabel)
  (args  [pred] pargs)
  (vars  [pred]
    (reduce
     (fn [xs arg]
       (into xs (term/vars arg))) #{} pargs))
  (subterms [pred]
    (reduce
     (fn [xs arg]
       (into xs (term/subterms arg))) #{} pargs))
  (positive? [pred] sign)
  substitutions/ISubstitutable
  (substitute [pred sigma]
    (update pred :pargs #(seq (map (fn [arg] (substitutions/substitute arg sigma)) %)))))

(defrecord Equality [comparator left right sign]
  pred/IPredicate
  (label [e] ::equality)
  (args  [e] [left right])
  (vars  [e] (set/union (term/vars left) (term/vars right)))
  (subterms [e] (set/union (term/subterms left) (term/subterms right)))
  (positive? [e] sign)
  substitutions/ISubstitutable
  (substitute [e sigma] (let [tmp [(substitutions/substitute left sigma) (substitutions/substitute right sigma)]
                              [l r] (if comparator (sort comparator tmp) tmp)]
                          (-> e
                              (assoc :left l)
                              (assoc :right r)))))

(defn variable [label] (->Variable label))

(defn function [label args] (->Function label (seq args)))

(defn constant [label] (->Function label nil))

(defn predicate
  ([label] (predicate label nil true))
  ([label args] (predicate label args true))
  ([label args sign] (predicate label args (boolean sign))))

(defn equality
  ([left right] (equality left right true))
  ([left right sign] (equality left right sign nil))
  ([left right sign comparator]
   (let [[l r] (if comparator (sort comparator [left right]) [left right])]
     (map->Equality {:comparator comparator :left l :right r :sign (boolean sign)}))))

(defn not-equality? [x]
  (and (pred/predicate? x)
       (not= ::equality (pred/label x))))

(defn equality? [x]
  (and (pred/predicate? x)
       (= ::equality (pred/label x))))

(defn pos-predicate? [x]
  (and (not= ::equality (pred/label x))
       (pred/positive? x)))

(defn neg-predicate? [x]
  (and (not= ::equality (pred/label x))
       (not (pred/positive? x))))

(defn pos-equality? [x]
  (and (equality? x)
       (pred/positive? x)))

(defn neg-equality? [x]
  (not (pos-equality? x)))
