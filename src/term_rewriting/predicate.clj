(ns term-rewriting.predicate)

(defprotocol IPredicate
  (label      [predicate])
  (args       [predicate])
  (vars       [predicate])
  (subterms   [predicate])
  (positive?  [predicate]))

(defn predicate? [x]
  (satisfies? IPredicate x))

(defn arity [x]
  (count (args x)))
