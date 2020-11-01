(ns blue)

;; Team blue solution to problem 1.4

;; For this problem the standard Clojure tree-seq tree traversal function
;; was considered, along with the clojure.walk library function, but it was
;; decided that implementing the tree walking functionality ourselves would
;; demonstrate the concept of the adopted solution more clearly.

;; Proof of concept code doing a depth first traversal of a binary tree
;; and returning the total weight. This would be equivalent to simply
;; flattening the tree and summing the values of its leaf nodes.

(defn weight [t]
  (let [[left right] t]
    (if (number? left)
      (if (number? right)
        (+ left right)
        (+ left (weight right)))
      (if (number? right)
        (+ (weight left) right)
        (+ (weight left) (weight right))))))

;; As mentioned above, this could be more simply written as the following:

(defn weight [t] (reduce + (flatten t)))

;; Another approach to this is to conceptually do a depth first post order
;; traversal and replace each node with the weight of the subtree below it.

(defn weight [t]
  (if (number? t)
    t
    (let [[left right] t]
      (+ (weight left) (weight right)))))

;; Indeed this process of replacing a subtree with the value of a function
;; applied to a subtree is exactly what the walk/postwalk function from the
;; clojure.walk library does.

(require ['clojure.walk :as 'walk])

(defn weight-walk [t] (walk/postwalk weight t))

;; This builds on the simple calculation of the weight of a subtree by
;; returning two value as a list. The first value is the weight of the subtree
;; being evaluated and the second value is the maximum weight of itself and any
;; child subtrees.

(defn maximum-tree-sum [t]
  (letfn [
    (max-weight [[left right]]
      (if (number? left)
        (if (number? right)
          (let [weight (+ left right)]
            (list weight weight))
          (let [
            [right-weight m] (max-weight right)
            weight (+ left right-weight)]
              (list weight (max weight m))))
        (if (number? right)
          (let [
            [left-weight m] (max-weight left)
            weight (+ left-weight right)]
              (list weight (max weight m)))
          (let [
            [left-weight max-left] (max-weight left)
            [right-weight max-right] (max-weight right)
            weight (+ left-weight right-weight)]
             (list weight (max weight max-left max-right))))))]

    (second (max-weight t))))

;; For purposes of testing whilst developing this solution the following test
;; cases and harness was used. Running the test harness should produce no
;; output.

(def tests {
 [1 2]
   3
 [-1 -3]
  -4
 [3 [4 [-1 -1]]]
   5
 [[4 [-1 -1]] 3]
   5
 [[4 [-1 -1]] -1]
   2
 [[[-5 [3 -1]][-2 14]][-6 [7 [-2 5]]]]
   13
 [[[-5 [3 -1]][-2 4]][-6 [7 [-2 5]]]]
   10})

(doseq [[input expected] tests]
  (let [output (maximum-tree-sum input)]
    (assert (= output expected))))

;; As an alternative and slightly more readable solution, inspiration was
;; drawn from the idea of mapping the value of nodes in a post order,
;; depth first traversal of the tree as implemented in the clojure.walk
;; library.

;; In this case the mapping of nodes is from a nested sequence which
;; represents a subtree into a map containing information of the node
;; weight and maximum weight of any subtrees.

(defn maximum-tree-sum [t]
  (letfn [
    (weight-max [t]
      (if (number? t)
        {:leaf true, :weight t}
        (let [
          [left right] t
          left-node (weight-max left)
          right-node (weight-max right) 
          weight (+ (:weight left-node) (:weight right-node))]
          
          { :leaf false
            :weight weight
            :max
              (if (:leaf left-node)
                (if (:leaf right-node)
                  weight
                  (max weight (:max right-node)))
                (if (:leaf right-node)
                  (max weight (:max left-node))
                  (max weight (:max left-node) (:max right-node))))})))]
    
    (:max (weight-max t))))

;; Using the same set of tests as above.

(doseq [[input expected] tests]
  (let [output (maximum-tree-sum input)]
    (assert (= output expected))))
