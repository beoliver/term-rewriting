(ns term-rewriting.clause
  (:require [term-rewriting.substitutions :as s]
            [term-rewriting.predicate :as p]
            [term-rewriting.emitters.leancop :as emit]
            [clojure.set :as set]))

(defprotocol IClause
  (subterms [clause])
  (vars [clause])
  (index [clause])
  (formulae [clause])
  (add-formula [clause formula])
  (remove-formula [clause formula]))

(defrecord Clause [index forms]
  IClause
  (subterms [clause]
    (reduce set/union (map p/subterms forms)))
  (vars [clause]
    (reduce set/union (map p/vars forms)))
  (index [clause] (:index clause))
  (formulae [clause] forms)
  (add-formula [clause formula]
    (update clause :forms (fnil conj #{}) formula))
  (remove-formula [clause formula]
    (update clause :forms disj formula))
  s/ISubstitutable
  (substitute [clause sigma]
    (assoc clause :forms (set (map #(s/substitute % sigma) forms))))
  emit/LeanCopEmitter
  (emit-str [clause]
    (->> forms
         (map emit/emit-str)
         (interpose ", ")
         (apply str)
         (format "[%s]"))))

(defn clause? [x]
  (instance? Clause x))

(defn unit-clause? [clause]
  (= (count (formulae clause)) 1))

(defn empty-clause? [clause]
  (= (count (formulae clause)) 0))

(defn clause
  ([index] (clause index #{}))
  ([index formulae]
   (map->Clause {:index index :forms (set formulae)})))

(defn swap-formulae [clause formulae]
  (assoc clause :forms (set formulae)))
