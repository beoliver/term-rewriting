(ns term-rewriting.reductions.proto)

(defprotocol ReductionRule
  (applicable? [rule clause])
  (explain [this example]))

(defrecord LocalReductionResult
    [noop clause explanation valid])

(defrecord GlobalReductionResult
    [noop matrix explanation valid invalid])

(defprotocol LocalReductionRule
  (candidate-clause-indexes [rule matrix]
    "Should return a list of clause indexes that
     are possible candidates for the rule.
     As some rules rely on global information - even though
     they only update a clause on a local level...
     This is to avoid having to run the rule on every clause
     if global information can be used to narrow down the
     list of possible clauses that the rule can be used on")
  (apply-local-rule [rule clause]
    "where clause has an index that was returned from
     candidate-clause-indexes.
     The clausse index should not be changed.
     A local rule return a possibly new clause.
     If no clause is returned then it is removed from the matrix.
     result is wrapped in a `LocalReductionResult` record"))

(defprotocol GlobalReductionRule
  (apply-global-rule [rule matrix]
    "should update the matrix in a safe manner
     and return the result in a `ReductionResult` record"))


(def local-noop
  (map->LocalReductionResult {:noop true}))

(defn local-update [clause explanation]
  (map->LocalReductionResult {:clause clause :explanation explanation}))

(defn local-valid [clause explanation]
  (map->LocalReductionResult {:clause clause :explanation explanation :valid true}))

(def global-noop
  (map->GlobalReductionResult {:noop true}))

(defn global-update [matrix explanation]
  (map->GlobalReductionResult {:matrix matrix :explanation explanation}))

(defn global-valid [matrix explanation]
  (map->GlobalReductionResult {:matrix matrix :explanation explanation :valid true}))

(defn global-invalid [matrix explanation]
  (map->GlobalReductionResult {:matrix matrix :explanation explanation :invalid true}))
