(ns tree-sum.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(def tree '((1 1) (2 2)))

(def simple '(4 5))

(def left '((((1 2) 2) 2) 2))

(def problem-tree '(((-5 (3 -1)) ((-2 4))) ((-6 (7 (-2 5))))))

(def full-tree '(5 (1 (-1 (-5) (4 (3 1))) (2 (-2 4))) (4 (-6) (10 (7 (3 (-2 5)))))))

(defn tree? [seq]
  (and (seq? seq) (not (empty? seq))))

(defn make-node [x y z]
(list x (list y z)))

(defn gen-tree [tree]
  (println tree)
  (cond
    (not (tree? tree))
      tree
    :else
    (let [x (first tree) y (first (rest tree))]
        (make-node (+ (gen-tree x) (gen-tree y))
                   (gen-tree x)
                   (gen-tree y))))
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

