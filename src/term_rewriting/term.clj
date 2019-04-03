(ns term-rewriting.term)

(defprotocol ITerm
  (variable?  [term])
  (function?  [term])
  (label      [term])
  (args       [term])
  (vars       [term])
  (subterms   [term]))

(defn term? [t]
  (satisfies? ITerm t))

(defn subterm? [s t]
  (contains? (set (subterms t)) s))

(defn constant? [s]
  (and (function? s)
       (empty? (args s))))

(defn arity [x]
  (count (args x)))
