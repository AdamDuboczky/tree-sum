(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(defn tester [tests]
  (mfor ['(?id ?test => ?res) tests]
        (if-not (= (eval (? test)) (eval (? res)))
          (println (mout
                     '(FAILED ?id ?test => ?res)))))
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
   (if(not(nil? left)) (conj tree left))
   (if(not(nil? right)) (conj tree right)))
  ([left right]
   (cond
     (nil? left) right
     (nil? right) left
     :else
     (conj left right))))

(defn append-2
  ([tree left right]
   (if (not(nil? left))
     (conj tree left))
   (if (not(nil? right))
     (conj tree right)))
  ([left right]
   (conj left right)))

(defn append-3
  ([tree left right]
   (if (and (not(nil? left)) (not (nil? (first left))))
     (conj tree left))
   (if (and (not(nil? right)) (not (nil? (first right))))
     (conj tree right)))
  ([left right]
   (conj left right)))



;(cond
;  (and (nil? right)) (nil? left))
;nil
;:else

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
      (and (not (tree? tree)) (= 0 tree))
      tree
      (not (tree? tree))
      nil
      (= (first tree) 0)
      (list tree (zero-sum left) (zero-sum right))
      :else
      (list (zero-sum left) (zero-sum right)))))

(defn zero-sum2 [tree]
  (let [ left (left-child tree)
        right (right-child tree)]
    (cond
      (and (not (tree? tree)) (= tree 0))
      tree
      (and (tree? tree) (= (first tree) 0))
      (list tree (zero-sum2 left) (zero-sum2 right))
      (tree? tree)
      (list (zero-sum2 left) (zero-sum2 right)))))

;if coll? recur
(defn zero-sum3 [tree]
  (let [left (left-child tree)
        right (right-child tree)]
    (cond
      (and (not (tree? tree)) (= 0 tree))
      tree
      (tree? tree)
      (do (filter #(= 0 (first %)) left)
          (filter #(= 0 (first %)) right)))
    :else
    (do (zero-sum3 (rest left)) (zero-sum3 (rest right)))))

(defn zero-sum4 [tree]
  (let [ left (left-child tree)
        right (right-child tree)]
    (cond
      (and (not (tree? tree)) (= tree 0))
      tree
      (and (tree? tree) (= (first tree) 0))
      (append-2 tree (zero-sum4 left) (zero-sum4 right))
      (tree? tree)
      (append-2 (zero-sum4 left) (zero-sum4 right)))))

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


(def tests ;some simple tests
  '([t1 (+ 2 2) => 4]
     [t2 (- 3 2) => 'banana]
     [t3 (first '(cat bat rat)) => 'cat]
     [4 (maximum-tree-sum '(((1 2)3) (4 ((5 6)7))) ) => 27] ))

(def tree-tests-maximum
  '([1 (maximum-tree-sum '(((1 2)3) (4 ((5 6)7))) ) => 28]
    [2 (maximum-tree-sum '(((3 9)-3) (-1 ((12 -3)2))) ) => 19]
    [3 (maximum-tree-sum '(((1 2)9) (3 ((-2 -3)-6))) ) => 12]
    [4 (maximum-tree-sum '(((6 3)1) (5 ((9 2)4))) ) => 30]
    [5 (maximum-tree-sum '(((-4 -6)2) (1 ((-3 3)3))) ) => 4]
    [6 (maximum-tree-sum '(((-8 -3)15) (1 ((-2 -1)2))) ) => 15]
    [7 (maximum-tree-sum '(((0 -4)0) (-3 ((1 0)2))) ) => 3]
    [8 (maximum-tree-sum '(((6 4)-3) (-8 ((16 -3)2))) ) => 16]
     ))

(def tree-tests-minimum
  '( (1 (minimum-tree-sum '(((1 2)3) (4 ((5 6)7))) ) => 1)
     (2 (minimum-tree-sum '(((3 9)-3) (-1 ((12 -3)2))) ) => -3)
     (3 (minimum-tree-sum '(((1 2)9) (3 ((-2 -3)-6))) ) => -11)
     (4 (minimum-tree-sum '(((6 3)1) (5 ((9 2)4))) ) => 1)
     (5 (minimum-tree-sum '(((-4 -6)2) (1 ((-3 3)3))) ) => -10)
     (6 (minimum-tree-sum '(((-8 -3)15) (1 ((-2 -1)2))) ) => -11)
     (7 (minimum-tree-sum '(((0 -4)0) (-3 ((1 0)2))) ) => -4)
     (8 (minimum-tree-sum '(((6 4)-3) (-8 ((16 -3)2)))) => -8)
     ))

(def tree-tests-zero-sum
  '( (1 (zero-sum?  ) =>  )
     (2 (zero-sum?  ) =>  )
     (3 (zero-sum?  ) =>  )
     (4 (zero-sum?  ) =>  )
     (5 (zero-sum?  ) =>  )
     (6 (zero-sum?  ) =>  )
     (7 (zero-sum?  ) =>  )
     ))



;(filter #(zero?(first %)) '((0 (1 2)1 2) (1 2 6 (0 3 )7 8) (7 8 1)))







(defn biggest-value [tree]
  (apply max (flatten tree)))

(defn sum-tree [tree]
  (if (coll? tree)
    (apply + (map sum-tree tree))
    tree))

(defn biggest-branch [tree]
  (apply max-key sum-tree tree))

(defn branch? [x]
  (coll? x))

(declare sum-tree)

(defn sum-leaf [leaf]
  leaf)

(defn sum-branch [branch]
  (apply + (map sum-tree branch)))

(defn sum-tree [tree]
  (if (branch? tree)
    (sum-branch tree)
    (sum-leaf tree)))

(defn biggest-branch-2 [tree]
  { :pre [(branch? tree)] }
  (apply max (map sum-tree tree)))

(defn test-map [tree]
  (map inc tree))