(ns mazes.grid-test
  (:require [clojure.test :refer :all]
            [mazes.grid :refer :all]))

(deftest grid-key-test
  (testing "Creating a key from a grid location"
    (is (= (grid-key 3 4) "3,4"))
    (is (= (grid-key nil nil) ","))
    (is (= (grid-key {:column 1 :row 2}) "1,2"))))

(deftest init-test
  (testing "Creating a grid"
    (is (= (init 2 2)
           {:rows 2
            :cols 2
            :cells {"0,0" {:column 0 :row 0 :links #{}}
                    "0,1" {:column 0 :row 1 :links #{}}
                    "1,0" {:column 1 :row 0 :links #{}}
                    "1,1" {:column 1 :row 1 :links #{}}}}))
    (is (= (init 0 0)
           {:rows 0
            :cols 0
            :cells {}}))
    (is (= (init 1 2)
           {:rows 1
            :cols 2
            :cells {"0,0" {:column 0 :row 0 :links #{}}
                    "1,0" {:column 1 :row 0 :links #{}}}}))))

(deftest get-cell-test
  (testing "Testing getting a cell out of a grid"
    (let [grid (init 3 3)]
      (is (= (get-cell grid 1 2)
             (make-cell 1 2)))
      (is (= (get-cell grid 5 5)
             nil)))))

(deftest direction-from-cell-test
  (testing "Testing finding a coordinate from a cell and direction"
    (let [cell (make-cell 2 1)]
      (is (= (direction-from-cell cell :north)
             [2 2]))
      (is (= (direction-from-cell cell :south)
             [2 0]))
      (is (= (direction-from-cell cell :east)
             [3 1]))
      (is (= (direction-from-cell cell :west)
             [1 1])))))

(deftest cell-has-neighbour-test
  (testing "Testing whether a cell has a neighbour"
    (let [grid (init 2 2)
          cell (get-cell grid 1 1)]
      (is (true? (cell-has-neighbour? grid cell :west)))
      (is (true? (cell-has-neighbour? grid cell :south)))
      (is (false? (cell-has-neighbour? grid cell :north)))
      (is (false? (cell-has-neighbour? grid cell :east))))))

(deftest cell-at-dir-test
  (testing "Get neighbouring cell if it exists"
    (let [grid (init 2 2)
          cell (get-cell grid 0 0)]
      (is (nil? (cell-at-dir grid cell :south)))
      (is (nil? (cell-at-dir grid cell :west)))
      (is (= (cell-at-dir grid cell :north)
             (make-cell 0 1)))
      (is (= (cell-at-dir grid cell :east)
             (make-cell 1 0))))))

(deftest direction-between-test
  (testing "Find direction between two cells if neighbouring"
    (is (nil? (direction-between (make-cell 0 0)
                                 (make-cell 5 5))))
    (is (= :north (direction-between (make-cell 0 0)
                                     (make-cell 0 1))))
    (is (= :south (direction-between (make-cell 0 1)
                                     (make-cell 0 0))))
    (is (= :east (direction-between (make-cell 0 0)
                                    (make-cell 1 0))))
    (is (= :west (direction-between (make-cell 1 0)
                                    (make-cell 0 0))))))

(deftest link-cells-test
  (testing "Cells can be linked to each other"
    (let [grid (init 2 2)]
      (is (= (link-cells grid (make-cell 0 0) (make-cell 0 1))
             {:rows 2
              :cols 2
              :cells {"0,0" {:column 0 :row 0 :links #{:north}}
                      "0,1" {:column 0 :row 1 :links #{:south}}
                      "1,0" {:column 1 :row 0 :links #{}}
                      "1,1" {:column 1 :row 1 :links #{}}}}))
      (is (= (link-cells grid (make-cell 0 0) (make-cell 1 1))
             {:rows 2
              :cols 2
              :cells {"0,0" {:column 0 :row 0 :links #{}}
                      "0,1" {:column 0 :row 1 :links #{}}
                      "1,0" {:column 1 :row 0 :links #{}}
                      "1,1" {:column 1 :row 1 :links #{}}}}))
      (is (= (link-cells grid (make-cell 0 0) (make-cell 0 0))
             {:rows 2
              :cols 2
              :cells {"0,0" {:column 0 :row 0 :links #{}}
                      "0,1" {:column 0 :row 1 :links #{}}
                      "1,0" {:column 1 :row 0 :links #{}}
                      "1,1" {:column 1 :row 1 :links #{}}}})))))
