(ns term-rewriting.emitters.leancop)

(defprotocol LeanCopEmitter
  (emit-str [this]))
