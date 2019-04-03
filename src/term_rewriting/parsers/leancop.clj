(ns term-rewriting.parsers.leancop
  (:require [term-rewriting.fol :as fol]
            [term-rewriting.clause :as clause]
            [term-rewriting.matrix :as matrix]
            [term-rewriting.predicate :as p]
            [term-rewriting.term :as term])
  (:import [java.io
            Reader
            StringReader
            FileReader
            PushbackReader]
           [java.lang
            StringBuilder]))

(declare parse*)

(defn parse-path
  ([^String path] (parse-path path nil))
  ([^String path timeout-ms]
   (let [reader (FileReader. ^String path)]
     (parse-reader reader timeout-ms))))

(defn parse-reader
  ([reader] (parse reader nil))
  ([^Reader reader timeout-ms]
   (let [fmatrix (future (parse* reader))]
     (if-not timeout-ms
       (deref fmatrix)
       (let [matrix (deref fmatrix timeout-ms nil)]
         (or matrix
             (try (future-cancel fmatrix)
                  (.close reader)
                  (catch Exception e nil)
                  (finally nil))))))))

(declare parse-problem)

(defn- parse*
  [^Reader reader]
  (let [pb (PushbackReader. reader)
        problem-id ^StringBuilder (StringBuilder. 32)]
    (loop [c (char (.read pb))]
      (when-not (= c \')
        (recur (char (.read pb)))))
    (loop [c (char (.read pb))]
      (when-not (= c \')
        (.append problem-id c)
        (recur (char (.read pb)))))
    (loop [c (char (.read pb))]
      (if (= c \[)
        (.unread pb (unchecked-int \[))
        (recur (char (.read pb)))))
    (let [problem-id (.toString problem-id)
          problem (parse-problem pb (matrix/matrix problem-id))]
      (.read pb) ;; )
      (.read pb) ;; .
      (let [eof ^int (.read pb)]
        (assert (= eof -1) "reading finished as expected"))
      (.close pb)
      problem)))

(declare parse-clause)

(defn- parse-problem
  [^PushbackReader reader matrix]
  (.read reader) ;; [
  (loop [c (char (.read reader))
         matrix matrix
         clause-index 1]
    (case c
      \]     matrix
      \,     (recur ^char (char (.read reader)) matrix clause-index)
      \space (recur ^char (char (.read reader)) matrix clause-index)
      \[     (do (.unread reader (unchecked-int \[))
                 (let [clause (parse-clause reader clause-index)
                       updated-matrix (matrix/insert-clause matrix clause)]
                   (recur ^char (char (.read reader))
                          updated-matrix
                          (inc clause-index)))))))

(declare parse-formula)

(defn- parse-clause
  [^PushbackReader reader clause-index]
  (.read reader) ;; [
  (let [var-map (atom {})]
    ;; dirty as fuck! but need a quick fix
    (loop [c ^char (char (.read reader))
           formulae (transient #{})]
      (case c
        \]     (clause/clause clause-index (persistent! formulae))
        \,     (recur ^char (char (.read reader)) formulae)
        \space (recur ^char (char (.read reader)) formulae)
        (do (.unread reader (unchecked-int c))
            (let [formula (parse-formula reader var-map)]
              (recur ^char (char (.read reader)) (conj! formulae formula))))))))

(declare parse-term)

(defn- parse-formula
  ([^PushbackReader reader var-map]
   (let [c ^char (char (.read reader))]
     (if (= c \-) ;; start of negative predicate
       (do (.read reader) ;; (
           (let [result (parse-formula reader var-map false)]
             (.read reader) ;; )
             result))
       (do (.unread reader (unchecked-int c))
           (parse-formula reader var-map true)))))
  ([^PushbackReader reader var-map positive?]
   (let [left (parse-term reader var-map)]
     (let [c ^char (char (.read reader))
           is-predicate? (case c
                           \)     true
                           \]     true
                           \,     true
                           \space false)]
       (if is-predicate?
         (let [label (term/label left)
               args  (term/args left)
               predicate (fol/predicate label (vec args) positive?)]
           (.unread reader (unchecked-int c))
           predicate)
         (let [expected-char-is-eq (char (.read reader))
               expected-char-is-space (char (.read reader))]
           (assert (= expected-char-is-eq \=) "should be an = symbol")
           (assert (= expected-char-is-space \space) "expected space")
           (let [right (parse-term reader var-map)]
             (fol/equality left right positive?))))))))

(declare parse-variable parse-function-or-constant)

(defn- parse-term
  [^PushbackReader reader var-map]
  (let [c ^char (char (.read reader))]
    (.unread reader (unchecked-int c))
    (if (or (= c \_) (Character/isUpperCase c))
      ;; start of a variable
      (parse-variable reader var-map)
      (parse-function-or-constant reader var-map))))

(defn- parse-variable
  [^PushbackReader reader var-map]
  (let [builder ^StringBuilder (StringBuilder. 16)]
    (loop [c ^char (char (.read reader))]
      (case c
        \,     (.unread reader (unchecked-int c))
        \space (.unread reader (unchecked-int c))
        \)     (.unread reader (unchecked-int c))
        \]     (.unread reader (unchecked-int c))
        (do (.append builder c)
            (recur ^char (char (.read reader))))))
    (let [label (.toString builder)]
      (if-let [mapped-var (get @var-map label)]
        mapped-var
        (let [new-var (fol/variable)]
          (swap! var-map assoc label new-var)
          new-var)))))

(defn- parse-function-or-constant
  [^PushbackReader reader var-map]
  (let [builder ^StringBuilder (StringBuilder. 16)]
    (loop [c ^char (char (.read reader))]
      (let [is-constant? (case c
                           \)     true
                           \]     true
                           \,     true
                           \space true
                           false)]
        (cond is-constant? (let [constant (fol/constant (.toString builder))]
                             (.unread reader (unchecked-int c))
                             constant)
              (= c \()
              (let [label (.toString builder)]
                (loop [args (transient [])]
                  (let [args (conj! args (parse-term reader var-map))
                        c ^char (.read reader)]
                    (cond
                      (= c \)) (fol/function label (persistent! args))
                      (= c \,) (do (.read reader) ;; \space
                                   (recur args))))))

              :else (do (.append builder c)
                        (recur ^char (char (.read reader)))))))))
