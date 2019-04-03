(ns term-rewriting.fol
  (:require [term-rewriting.term :as term]
            [term-rewriting.predicate :as pred]
            [term-rewriting.substitutions :as substitutions]
            [term-rewriting.unifiers :as unifiers]
            [term-rewriting.emitters.leancop :as emit]
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
  (substitute [term sigma] (substitutions/lookup sigma term))
  emit/LeanCopEmitter
  (emit-str   [term] (str vlabel)))

(defn variable
  ([] (variable (gensym "X_")))
  ([label]
   (->Variable label)))

(defn variable? [x] (instance? Variable x))

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
        (update term :fargs #(mapv (fn [arg] (substitutions/substitute arg sigma)) %))
        term')))
  emit/LeanCopEmitter
  (emit-str [term]
    (if (empty? fargs)
      (str flabel) ; constant
      (->> fargs
           (map emit/emit-str)
           (interpose ", ")
           (apply str)
           (format "%s(%s)" flabel)))))

(defn function [label args] (->Function label (vec args)))

(defn function? [x] (instance? Function x))

(defn constant [label] (->Function label []))

(defn constant? [x] (and (function? x)
                         (empty? (term/args x))))

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
    (update pred :pargs #(mapv (fn [arg] (substitutions/substitute arg sigma)) %)))
  emit/LeanCopEmitter
  (emit-str [pred]
    (let [pred-str (if (empty? pargs)
                     (str plabel)
                     (->> pargs
                          (map emit/emit-str)
                          (interpose ", ")
                          (apply str)
                          (format "%s(%s)" plabel)))]
      (if sign
        pred-str
        (format "-(%s)" pred-str)))))

(defn predicate
  ([label] (predicate label [] true))
  ([label args] (predicate label args true))
  ([label args sign] (->Predicate label (vec args) (boolean sign))))

(defn predicate? [x] (instance? Predicate x))

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
                              (assoc :right r))))
  emit/LeanCopEmitter
  (emit-str [e]
    (let [eq-str (format "%s = %s" (emit/emit-str left) (emit/emit-str right))]
      (if sign
        eq-str
        (format "-(%s)" eq-str)))))

(defn equality
  ([left right] (equality left right true))
  ([left right sign] (equality left right sign nil))
  ([left right sign comparator]
   (let [[l r] (if comparator (sort comparator [left right]) [left right])]
     (map->Equality {:comparator comparator :left l :right r :sign (boolean sign)}))))


(defn equality? [x] (instance? Equality x))

(defn pos-predicate? [x]
  (and (predicate? x)
       (pred/positive? x)))

(defn neg-predicate? [x]
  (and (predicate? x)
       (not (pred/positive? x))))

(defn pos-equality? [x]
  (and (equality? x)
       (pred/positive? x)))

(defn neg-equality? [x]
  (and (equality? x)
       (not (pred/positive? x))))
