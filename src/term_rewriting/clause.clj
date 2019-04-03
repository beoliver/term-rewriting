(ns term-rewriting.clause
  (:require [term-rewriting.substitutions :as s]
            [term-rewriting.predicate :as p]
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
    (assoc clause :forms (set (map #(s/substitute % sigma) forms)))))

(defn clause? [x]
  (instance? Clause x))

(defn unit-clause? [clause]
  (= (count (formulae clause)) 0))

(defn clause
  ([index] (clause index #{}))
  ([index formulae]
   (map->Clause {:index index :forms (set formulae)})))
