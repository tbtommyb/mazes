(ns mazes.cell-test
  (:require [clojure.test :refer :all]
            [mazes.cell :refer :all]))

(deftest grid-key-test
  (testing "Creating a key from a grid location"
    (is (= (grid-key (make-cell [3 4])) [3 4]))))

(deftest coords-from-cell-test
  (testing "Testing finding a coordinate from a cell and direction"
    (let [cell (make-cell [2 1])]
      (is (= (coords-from-cell cell :north)
             [2 2]))
      (is (= (coords-from-cell cell :south)
             [2 0]))
      (is (= (coords-from-cell cell :east)
             [3 1]))
      (is (= (coords-from-cell cell :west)
             [1 1])))))
