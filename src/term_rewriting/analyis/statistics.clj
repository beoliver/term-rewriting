(ns term-rewriting.analyis.statistics
  (:require [term-rewriting.matrix :as matrix]
            [term-rewriting.term :as term]
            [term-rewriting.clause :as clause]
            [term-rewriting.fol :as fol]))

(defmulti matrix-statistics (fn [matrix label] label))

(defmethod matrix-statistics "id"
  [matrix _]
  (:problem-id matrix))

(defmethod matrix-statistics "positive predicates"
  [matrix _]
  (->> (:predicates matrix)
       (filter (fn [[label m]]
                 (some? (seq (get m true)))))
       count))

(defmethod matrix-statistics "negated predicates"
  [matrix _]
  (->> (:predicates matrix)
       (filter (fn [[label m]]
                 (some? (seq (get m false)))))
       count))

(defmethod matrix-statistics "predicates"
  [matrix _]
  (count (keys (:predicates matrix))))

(defmethod matrix-statistics "subterms"
  [matrix _]
  (count (term/subterms matrix)))

(defmethod matrix-statistics "variables"
  [matrix _]
  (count (term/vars matrix)))

(defmethod matrix-statistics "clauses"
  [matrix _]
  (count (matrix/clause-indexes matrix)))

(defmethod matrix-statistics "unit clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (filter clause/unit-clause?)
       count))

(defmethod matrix-statistics "pos eq clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (filter #(some fol/pos-equality? (clause/formulae %)))
       count))

(defmethod matrix-statistics "pos eq unit clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (filter #(and (clause/unit-clause? %)
                     (fol/pos-equality? (first (clause/formulae %)))))
       count))

(defmethod matrix-statistics "neg eq clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (filter #(some fol/neg-equality? (clause/formulae %)))
       count))

(defmethod matrix-statistics "neg eq unit clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (filter #(and (clause/unit-clause? %)
                     (fol/neg-equality? (first (clause/formulae %)))))
       count))

(defmethod matrix-statistics "max clause size"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (map (fn [c] (count (clause/formulae c))))
       (apply max)))

(defmethod matrix-statistics "min clause size"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (map (fn [c] (count (clause/formulae c))))
       (apply min)))

(defmethod matrix-statistics "avg clause size"
  [matrix _]
  (let [clauses (vals (:index->clause matrix))
        counts (map #(count (clause/formulae %)) clauses)]
    (float (/ (apply + counts)
              (count clauses)))))


(defmethod matrix-statistics "eq free clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (reduce (fn [n clause]
                 (if (some fol/equality? (clause/formulae clause))
                   n
                   (inc n))) 0)))

(defmethod matrix-statistics "pred free clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (reduce (fn [n clause]
                 (if (some fol/predicate? (clause/formulae clause))
                   n
                   (inc n))) 0)))

(defmethod matrix-statistics "var free clauses"
  [matrix _]
  (->> (:index->clause matrix)
       vals
       (reduce (fn [n clause]
                 (if (seq (clause/vars clause))
                   n
                   (inc n))) 0)))



(def default-fields [
                     "id"
                     "clauses"
                     "unit clauses"
                     "pos eq clauses"
                     "neg eq clauses"
                     "pos eq unit clauses"
                     "neg eq unit clauses"
                     "var free clauses"
                     "eq free clauses"
                     "pred free clauses"
                     "min clause size"
                     "max clause size"
                     "avg clause size"
                     "predicates"
                     "positive predicates"
                     "negated predicates"
                     "variables"
                     "subterms"
                     #_"var free pos predicates"
                     #_"var free neg predicates"
                     ])

(defn generate-stats
  ([matrix] (generate-stats matrix default-fields))
  ([matrix fields]
   (reduce (fn [acc field]
             (conj acc [field (matrix-statistics matrix field)]))
           [] fields)))
