(ns term-rewriting.unifiers
  (:require [term-rewriting.term :as term]
            [term-rewriting.substitutions :as substitutions]))

(defn most-general-unifier-martelli-and-montanari-algorithm-1
  "Based on Alberto Martelli and Ugo Montanari algorithm-1.
   Returns an object (map) that implements ISubstitution protocol"
  [& pairs]
  (let [condition-a (fn [[s t]] (and (not (term/variable? s)) (term/variable? t)))
        condition-b (fn [[s t]] (and (= s t) (term/variable? s) (term/variable? t)))
        condition-c (fn [[s t]] (and (not (term/variable? s)) (not (term/variable? t))))
        condition-d (fn [[s t]] (and (term/variable? s) (not (term/variable? t))))]
    (loop [previous-unifier (set pairs)]
      (let [result (loop [seen nil
                          [[s t :as pair] & unseen] previous-unifier]
                     (cond (nil? pair) seen
                           (condition-a pair) (recur seen (cons [t s] unseen))
                           (condition-b pair) (recur seen unseen)
                           (condition-c pair) (let [s-args (term/args s)
                                                    t-args (term/args t)]
                                                (cond (not= (term/label s) (term/label t)) ::error
                                                      (not= (count s-args) (count t-args)) ::error
                                                      :else (->> (map vector s-args t-args)
                                                                 (into unseen)
                                                                 (recur seen))))
                           (condition-d pair) (if (contains? (set (term/vars t)) s)
                                                ::error
                                                (let [sigma {s t}  ; implements the ISubstitution interface
                                                      sigma-fn (fn [[a b]]
                                                                 [(term/substitute a sigma)
                                                                  (term/substitute b sigma)])]
                                                  (recur (conj (map sigma-fn seen) pair) (map sigma-fn unseen))))
                           :else              (recur (conj seen pair) unseen)))]
        (when-not (= result ::error)
          (let [unifier (set result)]
            (if (= unifier previous-unifier)
              (into {} unifier) ; implements the ISubstitution interface
              (recur unifier))))))))
