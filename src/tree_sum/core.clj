(ns tree-sum.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(def tree '((1 1) (2 2)))

(def simple '(4 5))

(def left '((((1 2) 2) 2) 2))

(def problem-tree '(((-5 (3 -1)) (-2 4)) ((-6 (7 (-2 5))))))

(def full-tree '(3 (-1 (-3 (-5) (2 (3 -1))) (2 (-2 4))) (4 (-6) (10 (7 (3 (-2 5)))))))

(defn tree? [seq]
  (and (seq? seq) (not (empty? seq))))

(defn make-node [x y z]
  (println "making node ((" x  "(" y z "))" )
(list x (list y z)))

(defn add-nodes [x y]
  (+ (first x) (first y)))

(defn gen-tree [tree]
  ;(println tree)
  (cond
    (not (tree? tree))
      (list tree)
    :else
    (let [x (first tree) y (first (rest tree)) a (gen-tree x) b (gen-tree y)]
      ;(println a "---" b)
        (make-node (add-nodes a b)
                   a
                   b)))
  )



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

(defn biggest-value [tree]
  (apply max (flatten tree)))

