(ns term-rewriting.substitutions
  (:import clojure.lang.IPersistentMap))

(defprotocol ISubstitutable
  (substitute [this sigma]))

(defprotocol ISubstitution
  (make-empty [this] "return a new empty substitution")
  (domain  [this])
  (substitutions [this] "return a seq of vectors")
  (insert [this s t])
  (lookup [this s] "must return `s` if `s` is not mapped"))

(extend-type IPersistentMap
  ISubstitution
  (make-empty [this] {})
  (domain [this] (keys this))
  (substitutions [this] (seq this))
  (insert [this s t] (assoc this s t))
  (lookup [this s] (get this s s)))
