(ns term-rewriting.term)

(defprotocol ITerm
  (variable?  [term])
  (function?  [term])
  (label      [term])
  (args       [term])
  (vars       [term])
  (subterms   [term])
  (substitute [term sigma] "perform simultaneous substitution given some sigma"))

(defn subterm? [s t]
  (contains? (set (subterms t)) s))

(defn constant? [s]
  (and (function? s)
       (nil? (seq (args s)))))
