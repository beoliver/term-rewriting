(ns term-rewriting.predicate)

(defprotocol IPredicate
  (label      [predicate])
  (args       [predicate])
  (vars       [predicate])
  (subterms   [predicate])
  (positive?  [predicate]))

(defn predicate? [x]
  (instance? IPredicate x))

(defn arity [x]
  (count (args x)))
