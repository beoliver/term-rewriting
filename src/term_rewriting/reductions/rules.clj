(ns term-rewriting.reductions.rules
  (:require [term-rewriting.reductions.proto :as proto]
            [term-rewriting.matrix :as matrix]
            [term-rewriting.clause :as clause]
            [term-rewriting.unifiers :as unifiers]
            [term-rewriting.fol :as fol]
            [term-rewriting.term :as term]
            [term-rewriting.predicate :as pred]
            [clojure.set :as set]))

(defrecord Positive_Equality_Unit_Clause []
  proto/LocalReductionRule
  (candidate-clause-indexes [rule matrix]
    (matrix/indexes-of-clauses-containing-positive-equality matrix))
  (apply-local-rule [rule clause]
    (let [e (first (clause/formulae clause))]
      (cond (not (clause/unit-clause? clause)) proto/local-noop
            (not (unifiers/unifies? e))        proto/local-noop
            :else
            (proto/local-valid clause {:message "found a mgu"
                                       :proof   (unifiers/mgu e)})))))

(defrecord Equality_With_Disjoint_Vars []
  proto/LocalReductionRule
  (candidate-clause-indexes [rule matrix]
    (matrix/indexes-of-clauses-containing-positive-equality matrix))
  (apply-local-rule [rule clause]
    (let [forms (set (clause/formulae clause))
          eqs   (filter fol/pos-equality? forms)
          res   (reduce (fn [_ e]
                          (let [vs (set (pred/vars e))
                                oth (reduce set/union (map pred/vars (disj forms e)))]
                            (if (and (empty? (set/intersection vs oth))
                                     (unifiers/unifies? e))
                              (reduced e)
                              nil))) nil eqs)]
      (if-not res
        proto/local-noop
        (let [new-clause (clause/swap-formulae clause (disj forms res))]
          (proto/local-update new-clause {:messgae "found disjoint positive equation that was solvable"
                                          :proof   res}))))))

(defrecord Identity_Unit_Clause []
  proto/LocalReductionRule
  (candidate-clause-indexes [rule matrix]
    (matrix/indexes-of-clauses-containing-positive-equality matrix))
  (apply-local-rule [rule clause]
    (let [{:keys [left right] :as e} (first (clause/formulae clause))]
      (cond (not (clause/unit-clause? clause)) proto/local-noop
            (not= left right)                  proto/local-noop
            :else
            (proto/local-valid clause {:message "found unit clause with positive identity"
                                       :proof   e})))))

(defrecord Empty_Clause []
  proto/LocalReductionRule
  (candidate-clause-indexes [rule matrix]
    (matrix/clause-indexes matrix))
  (apply-local-rule [rule clause]
    (if (clause/empty-clause? clause)
      (proto/local-valid clause {:message "found empty clause"
                                 :proof   clause})
      proto/local-noop)))


;;; ------------------------------------------------------------------------------------------
;;; Running The Rules ------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------

(defn terminate-at [timeout]
  (+ (System/currentTimeMillis) timeout))

(defn terminate? [terminates-at]
  (> (System/currentTimeMillis)
     terminates-at))

(defn run-local-reduction-rule
  [{:keys [matrix history terminates-at] :as state} rule]
  (reduce (fn [state clause-index]
            (if (terminate? terminates-at)
              (reduced (update state :history assoc :timeout true))
              (let [pre-clause (matrix/clause-by-index matrix clause-index)
                    {:keys [noop clause explanation valid]} (proto/apply-local-rule rule pre-clause)]
                (if noop
                  state
                  (let [new-state (-> state
                                      (update-in [:history :logs] conj explanation)
                                      (update :history assoc :valid valid)
                                      (update :matrix matrix/update-clause clause))]
                    (if valid
                      (reduced new-state)
                      new-state))))))
          state
          (seq (proto/candidate-clause-indexes rule matrix))))

(defn run-global-reduction-rule
  [{:keys [matrix history terminates-at] :as state} rule]
  (let [{:keys [noop matrix explanation valid invalid]} (proto/apply-global-rule rule matrix)]
    (if noop
      state
      (-> state
          (assoc :matrix matrix)
          (update-in [:history :logs] conj explanation)
          (update :history assoc :valid valid)
          (update :history assoc :invalid invalid)))))

(defn run-reduction-rule [{:keys [terminates-at matrix history] :as state} rule]
  (cond (terminate? terminates-at)                  (update state :history assoc :timeout true)
        (:valid history)                            state
        (:invalid history)                          state
        (satisfies? proto/LocalReductionRule rule)  (run-local-reduction-rule state rule)
        (satisfies? proto/GlobalReductionRule rule) (run-global-reduction-rule state rule)))

(defn run-reductions
  ([matrix rules] (run-reductions matrix rules {:timeout-ms 10000 :max-iterations 100}))
  ([matrix rules {:keys [timeout-ms max-iterations] :as opts}]
   (let [terminates-at (terminate-at timeout-ms)]
     (loop [previous-state {:terminates-at terminates-at :matrix matrix :history {:logs [] :iteration 0}}]
       (cond (terminate? terminates-at)                     (update previous-state :history assoc :timeout true)
             (= max-iterations (:iteration previous-state)) (update previous-state :history assoc :max-iterations true)
             :else
             (let [{:keys [matrix history] :as state} (reduce run-reduction-rule previous-state rules)]
               (cond (:timeout history)                  state
                     (:valid   history)                  state
                     (:invalid history)                  state
                     (= matrix (:matrix previous-state)) state
                     :else                               (recur (update-in state [:history :iteration] inc)))))))))

(defn reduce-matrix [matrix]
  (run-reductions matrix [(->Positive_Equality_Unit_Clause)
                          (->Identity_Unit_Clause)
                          (->Empty_Clause)
                          (->Equality_With_Disjoint_Vars)]))
