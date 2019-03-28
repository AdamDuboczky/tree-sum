(ns matcher-starter.tests
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(defn tester [tests]
  (mfor ['(-sumid -sumtest => -sumres) tests]
        (if-not (= (eval (-sum test)) (eval (-sum res)))
          (println (mout
                     '(FAILED -sumid -sumtest => -sumres)))))
  'end-of-testing)

(defn tree? [seq]
;Helper function to check if tree or not
(and (seq? seq) (not (empty? seq))))

(defn make-node [x y z]
  ;Helper function to create node
  (list x (list y z)))

(defn left-child [tree]
  (first (first (rest tree))))

(defn right-child [tree]
  (first (rest (first (rest tree)))))

(defn add-nodes [x y]
  ;Helper function to add node into existing tree
  (+ (first x) (first y)))

(defn append
  ([tree left right]
   (if (not(nil? left))
     (conj tree left))
   (if (not(nil? right))
     (conj tree right)))
  ([left right]
   (conj left right)))

(defn gen-tree [tree]
  ;From a partially completed tree, generates the full tree
  (cond
    (not (tree? tree))
    (list tree)
    :else
    (let [a (gen-tree (first tree))
          b (gen-tree (first (rest tree)))]
      (make-node (add-nodes a b)
                 a
                 b))))

(defn zero-sum [tree]
  (let [ left (left-child tree)
        right (right-child tree)]
    (cond
      (and (not (tree? tree)) (= tree 0))
      tree
      (and (tree? tree) (= (first tree) 0))
      (append tree (zero-sum left) (zero-sum right))
      (tree? tree)
      (append (zero-sum left) (zero-sum right)))))

(defn tree-calc [tree fn]
  ;Caller to use to apply max/min function
  (apply fn (flatten (gen-tree tree))))

(defn maximum-tree-sum [tree]
  (tree-calc tree max))

(defn minimum-tree-sum [tree]
(tree-calc tree min))

(defn tree-cal-key [tree]
  ;Caller to use to apply max/min selectively or all functions
  {:max
   (maximum-tree-sum tree)
   :min
   (minimum-tree-sum tree)
   :zero-sum
   (zero-sum (gen-tree tree))})



