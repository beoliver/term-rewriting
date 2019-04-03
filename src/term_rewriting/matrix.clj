(ns term-rewriting.matrix
  (:require [term-rewriting.clause :as clause]
            [term-rewriting.substitutions :as s]
            [term-rewriting.predicate :as p]
            [term-rewriting.fol :as fol]
            [term-rewriting.emitters.leancop :as emit]
            [clojure.set :as set]))

(defprotocol IMatrix
  (clause-by-index [matrix clause-index])
  (next-index [matrix])
  (insert-clause [matrix clause])
  (delete-clause-by-index [matrix clause-index])
  (update-clause [matrix clause]))

(defrecord Matrix [problem-id index->clause term->indexes predicates pos-eqs neg-eqs]
  emit/LeanCopEmitter
  (emit-str [matrix]
    (->> (vals index->clause)
         (map emit/emit-str)
         (interpose ", ")
         (apply str)
         (format "cnf('%s', conjecture, [%s])." problem-id)))
  IMatrix
  (clause-by-index [matrix clause-index]
    (get index->clause clause-index))
  (next-index [matrix]
    (inc (apply max (or (keys index->clause) [0]))))
  (insert-clause [matrix clause]
    (let [index (clause/index clause)
          subterms (clause/subterms clause)
          forms (clause/formulae clause)
          clause-contains-pos-eq? (some fol/pos-equality? forms)
          clause-contains-neg-eq? (some fol/neg-equality? forms)]
      (-> matrix
          (assoc-in [:index->clause index] clause)
          (update :pos-eqs #(if clause-contains-pos-eq? (set/union % #{index}) %))
          (update :neg-eqs #(if clause-contains-neg-eq? (set/union % #{index}) %))
          (update :predicates #(reduce (fn [table form]
                                         (cond (fol/pos-predicate? form)
                                               (update-in table [(p/label form) true] (fnil conj #{}) index)
                                               (fol/neg-predicate? form)
                                               (update-in table [(p/label form) false] (fnil conj #{}) index)
                                               :else table))
                                       %
                                       forms))
          (update :term->indexes #(reduce (fn [mappings term]
                                            (update mappings term (fnil conj #{}) index))
                                          % subterms)))))
  (delete-clause-by-index [matrix clause-index]
    (let [clause (clause-by-index matrix clause-index)]
      (if-not clause
        matrix
        (let [labels (remove #{::fol/equality} (map p/label (clause/formulae clause)))]
          (-> matrix
              (update :index->clause dissoc clause-index)
              (update :pos-eqs disj clause-index)
              (update :neg-eqs disj clause-index)
              (update :predicates #(reduce (fn [predicates label]
                                             (let [preds (-> predicates
                                                             (update-in [label true] disj clause-index)
                                                             (update-in [label false] disj clause-index))]
                                               (if (empty? (set/union (get-in predicates [label true])
                                                                      (get-in predicates [label false])))
                                                 (dissoc preds label)
                                                 preds)))
                                           % labels))
              (update :term->indexes #(reduce (fn [mappings term]
                                                (let [xs (disj (get mappings term #{}) clause-index)]
                                                  (if (empty? xs)
                                                    (dissoc mappings term)
                                                    (assoc mappings term xs))))
                                              % (set (clause/subterms clause)))))))))
  (update-clause [matrix clause]
    (-> matrix
        (delete-clause-by-index (clause/index clause))
        (insert-clause clause)))
  s/ISubstitutable
  (substitute [matrix sigma]
    (let [dom (set (s/domain sigma))
          candidate-clause-indexes (reduce set/union (select-keys term->indexes dom))]
      (reduce (fn [matrix clause-index]
                (let [clause (clause-by-index matrix clause-index)
                      clause' (s/substitute clause sigma)]
                  (if (not= clause clause')
                    (update-clause matrix clause')
                    matrix)))
              matrix
              candidate-clause-indexes))))

(defn matrix
  ([id] (matrix id nil))
  ([id clauses]
   (reduce (fn [matrix clause]
             (insert-clause matrix clause))
           (map->Matrix
            {:problem-id id
             :index->clause {}
             :term->indexes {}
             :predicates {}
             :pos-eqs #{}
             :neg-eqs #{}})
           (set clauses))))
