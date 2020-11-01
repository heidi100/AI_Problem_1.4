(ns blue) 
;; Team blue solution to probem 1.4b

;; Here we build on the solution developed for problem 1.4. Firstly we
;; present a function that returns both the maximum and minimum of any
;; subtree. This is a minor modification of the earlier solution that now
;; also keeps track of the minimum subtree weigh encountered in walking the
;; tree and as a consequence returns a list of three values; the weight of
;; the current subtrea, the maximum subtree weight at or under that subtree,
;; and the analogous minimum value.

(defn maxmin-tree-sum [t]
  (letfn [
    (maxmin-weight [[left right]] ;; => (weight maximum minimum)
      (if (number? left)
        (if (number? right)
          (let [weight (+ left right)]
            (list weight weight weight))
          (let [
            [right-weight maximum minimum] (maxmin-weight right)
            weight (+ left right-weight)]
              (list weight (max weight maximum) (min weight minimum))))
        (if (number? right)
          (let [
            [left-weight maximum minimum] (maxmin-weight left)
            weight (+ left-weight right)]
              (list weight (max weight maximum) (min weight minimum)))
          (let [
            [left-weight max-left min-left] (maxmin-weight left)
            [right-weight max-right min-right] (maxmin-weight right)
            weight (+ left-weight right-weight)]
             (list weight
               (max weight max-left max-right)
               (min weight min-left min-right))))))]

    (rest (maxmin-weight t))))

;; The test cases used earlier have been modified to support this version of
;; the solution but the test harness code remains the same except for having
;; to call the new function.

(def tests {
 [1 2]
   '(3 3)
 [3 [4 [-1 -1]]]
   '(5 -2)
 [[4 [-1 -1]] 3]
   '(5 -2)
 [[4 [-1 -1]] -1]
   '(2 -2)
 [[[-5 [3 -1]][-2 14]][-6 [7 [-2 5]]]]
   '(13 -3)
 [[[-5 [3 -1]][-2 4]][-6 [7 [-2 5]]]]
   '(10 -3)})

(doseq [[input expected] tests]
  (let [output (maxmin-tree-sum input)]
    (assert (= output expected))))

;; Further building on the structure of this algorithm allows the incorporation
;; of code to return all subtrees that have zero weight. The wording of the
;; problem presented is open to two interpretations. It could be requesting all
;; unique subtrees with the zero weight property, or it could be requesting an
;; enumeration of all such subtrees, including duplicates.

;; We chose to present a solution to the second interpretation on the grounds
;; that given a list presentation of all zero weight subtrees, including
;; duplicates, these can be removed trivially by transforming the list into
;; a Clojure set. If it was however specified that only unique zero weight
;; subtrees were required then a small amount of efficiency could be gained
;; by using a set type value to return the collection of zero weight subtrees
;; at each recursive call to the function.

(defn maxminzero-tree-sum [t]
  (letfn [
    (maxminzero-weight [t] ;; => (weight maximum minimum zeroes)
      (let [[left right] t]
        (if (number? left)
          (if (number? right)
            (let [weight (+ left right)]
              (list weight weight weight (if (zero? weight) (list t) ())))
            (let [
              [right-weight maximum minimum zeroes] (maxminzero-weight right)
              weight (+ left right-weight)]
                (list weight
                  (max weight maximum)
                  (min weight minimum)
                  (if (zero? weight) (concat zeroes (list t)) zeroes))))
          (if (number? right)
            (let [
              [left-weight maximum minimum zeroes] (maxminzero-weight left)
              weight (+ left-weight right)]
                (list weight
                  (max weight maximum)
                  (min weight minimum)
                  (if (zero? weight) (concat zeroes (list t)) zeroes)))
            (let [
              [left-weight max-left min-left zeroes-left]
                (maxminzero-weight left)
              [right-weight max-right min-right zeroes-right]
                (maxminzero-weight right)
              weight
                (+ left-weight right-weight)]

              (list weight
                (max weight max-left max-right)
                (min weight min-left min-right)
                (let [zeroes (concat zeroes-left zeroes-right)]
                  (if (zero? weight) (concat zeroes (list t)) zeroes))))))))]

    (rest (maxminzero-weight t))))

;; Test cases. Note that the expected ordering for zero weight subtrees assumes
;; a pre-order traversal of the tree.

(def tests {
 [1 2]
   '(3 3 ())
 [1 -1]
   '(0 0 ([1 -1]))
 [-1 -3]
   '(-4 -4 ())
 [2 [1 -1]]
   '(2 0 ([1 -1]))
 [3 [4 [-1 -1]]]
   '(5 -2 ())
 [[4 [-1 -1]] 3]
   '(5 -2 ())
 [[4 [-1 -1]] -1]
   '(2 -2 ())
 [[-1 [3 -2]][-5 [4 1]]]
   '(5 0 ([-1 [3 -2]] [-5 [4 1]] [[-1 [3 -2]][-5 [4 1]]]))
 [[[-5 [3 -1]][-2 14]][-6 [7 [-2 5]]]]
   '(13 -3 ())
 [[[-5 [3 -1]][-2 4]][-6 [7 [-2 5]]]]
   '(10 -3 ())})

(doseq [[input expected] tests]
  (let [output (maxminzero-tree-sum input)]
    (assert (= output expected))))

;; A slightly simpler version of the same function, rewritten to refactor out
;; some duplicate code. The underlying algorithm remains the same.

(defn maxminzero-tree-sum [t]
  (letfn [
    (build-result [weight maximum minimum zeroes tree]
      (list weight
        (max weight maximum)
        (min weight minimum)
        (if (zero? weight) (concat zeroes (list tree)) zeroes)))

    (maxminzero-weight [t] ;; => (weight maximum minimum zeroes)
      (let [[left right] t]
        (if (number? left)
          (if (number? right)
            (let [weight (+ left right)]
              (list weight weight weight (if (zero? weight) (list t) ())))
            (let [
              [right-weight maximum minimum zeroes] (maxminzero-weight right)
              weight (+ left right-weight)]
                (build-result weight maximum minimum zeroes t)))
          (if (number? right)
            (let [
              [left-weight maximum minimum zeroes] (maxminzero-weight left)
              weight (+ left-weight right)]
                (build-result weight maximum minimum zeroes t))
            (let [
              [left-weight max-left min-left zeroes-left]
                (maxminzero-weight left)
              [right-weight max-right min-right zeroes-right]
                (maxminzero-weight right)
              weight
                (+ left-weight right-weight)]

              (build-result weight
                (max max-left max-right)
                (min min-left min-right)
                (concat zeroes-left zeroes-right) t))))))]

    (rest (maxminzero-weight t))))

;; Using same tests as above.

(doseq [[input expected] tests]
  (let [output (maxminzero-tree-sum input)]
    (assert (= output expected))))

;; For the final solution we return to the map based algorithm of problem 1.4
;; and extend it so that each node in the modified tree now contains fields
;; for the minimum subtree sums and a list of all zero sum subtrees.

;; Note that by the nature of the problem there is a similarity in the form
;; of the part of the code dealing with maximum and minimum values. This
;; could potentially be factored out as a separate inner function or
;; implemented as a macro, but both these would potentially make the code
;; harder to follow.

(defn maxminzero-tree-sum [t]
  (letfn [
    (maxminzero-weight [t]
      (if (number? t)
        {:leaf true, :weight t, :zeroes ()}
        (let [
          [left right] t
          left-node (maxminzero-weight left)
          right-node (maxminzero-weight right)
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
                  (max weight (:max left-node) (:max right-node))))
            :min
              (if (:leaf left-node)
                (if (:leaf right-node)
                  weight
                  (min weight (:min right-node)))
                (if (:leaf right-node)
                  (min weight (:min left-node))
                  (min weight (:min left-node) (:min right-node))))
            :zeroes
              (if (zero? weight)
                (concat (:zeroes left-node) (:zeroes right-node) (list t))
                (concat (:zeroes left-node) (:zeroes right-node)))})))]

    (map (fn [k] (get (maxminzero-weight t) k)) [:max :min :zeroes])))

;; And again, using same tests as above.

(doseq [[input expected] tests]
  (let [output (maxminzero-tree-sum input)]
    (assert (= output expected))))

