(ns term-rewriting.parsers.leancop
  (:require [term-rewriting.fol :as fol]
            [term-rewriting.clause :as clause]
            [term-rewriting.matrix :as matrix])
  (:import [java.io
            Reader
            StringReader
            PushbackReader]
           [java.lang
            StringBuilder]))
