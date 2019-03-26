(ns tree-sum.core)

;Trees
(def node '(1 ((4) (-3))))

(def zero-node '(0 ((4) (-4))))

(def tree '((1 1) (2 2)))

(def simple '(4 5))

(def left '((((1 2) 2) 2) 2))

(def problem-tree '(((-5 (3 -1)) (-2 4)) (-6 (7 (-2 5)))))

(def zero-tree '(((4 0) (-1 -3)) ((1 -2) (3 -3))))

(def full-tree '(3 ((-1 ((-3 ((-5) (2 ((3) (-1))))) (2 ((-2) (4))))) (4 ((-6) (10 ((7) (3 ((-2) (5))))))))))

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
   (if(not(nil? left)) (conj tree left))
   (if(not(nil? right)) (conj tree right)))
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
;(defn zero-sum3 [tree]
;  (let [left (left-child tree)
;        right (right-child tree)]
;    (cond
;      (and (not (tree? tree)) (zero? tree))
;        tree
;      (tree? tree)
;      (or (filter #(zero?((first %) %)) left) (filter #(zero?(first %)) right)))
;    :else
;      ((zero-sum3 (rest left)) (zero-sum3 (rest right)))))

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

(defn tree-cal-key [tree]
  ;Caller to use to apply max/min selectively or all functions
  {:max
   (apply max (flatten (gen-tree tree)))
   :min
   (apply min (flatten (gen-tree tree)))}
   :zero-sum
   (zero-sum4 (gen-tree tree)))


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



