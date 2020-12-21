(ns mazes.grid-test
  (:require [clojure.test :refer :all]
            [mazes.grid :refer :all]))

(deftest grid-key-test
  (testing "Creating a key from a grid location"
    (is (= (grid-key (make-cell [3 4])) [3 4]))))

(deftest init-test
  (testing "Creating a grid"
    (is (= (init 2 2)
           {:rows 2
            :cols 2
            :cells {[0 0] #{}
                    [0 1] #{}
                    [1 0] #{}
                    [1 1] #{}}}))
    (is (= (init 1 2) {:rows 1, :cols 2, :cells {[0 0] #{}, [1 0] #{}}}))))

(deftest get-links-test
  (testing "Testing getting a cell out of a grid"
    (let [grid {:rows 2
                :cols 2
                :cells {[0 0] #{:a}
                        [0 1] #{:b}
                        [1 0] #{:c}
                        [1 1] #{:d}}}]
      (is (= {:coords [0 0] :links #{:a}} (get-cell grid [0 0])))
      (is (= {:coords [0 1] :links #{:b}} (get-cell grid [0 1])))
      (is (= {:coords [1 0] :links #{:c}} (get-cell grid [1 0])))
      (is (= {:coords [1 1] :links #{:d}} (get-cell grid [1 1])))
      (is (= nil (get-cell grid [5 1]))))))

(deftest iter-cols-cells-test
  (testing "Iterating through a grid by column"
    (let [grid (init 2 3)]
      (is (= (iter-cells grid)
             '({:coords [0 0] :links #{}}
               {:coords [0 1] :links #{}}
               {:coords [1 0] :links #{}}
               {:coords [1 1] :links #{}}
               {:coords [2 0] :links #{}}
               {:coords [2 1] :links #{}}))))))

(deftest iter-rows-coords-test
  (testing "Iterating through a grid row by row"
    (let [grid (init 2 3)]
      (is (= (iter-rows-coords grid)
             [[[0 0] [1 0] [2 0]] [[0 1] [1 1] [2 1]]])))))

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

(deftest cell-neighbour-at-test
  (testing "Get neighbouring cell if it exists"
    (let [grid (init 2 2)]
      (is (nil? (cell-neighbour-at grid [0 0] :south)))
      (is (nil? (cell-neighbour-at grid [0 0] :west)))
      (is (= (cell-neighbour-at grid [0 0] :north) [0 1]))
      (is (= (cell-neighbour-at grid [0 0] :east) [1 0])))))

(deftest direction-between-test
  (testing "Find direction between two cells if neighbouring"
    (is (nil? (direction-between [0 0] [5 5])))
    (is (= :north (direction-between [0 0] [0 1])))
    (is (= :south (direction-between [0 1] [0 0])))
    (is (= :east (direction-between [0 0] [1 0])))
    (is (= :west (direction-between [1 0] [0 0])))))

(deftest link-cells-test
  (testing "Cells can be linked to each other"
    (let [grid (init 2 2)]
      (is (= (link-cells grid [0 0] [0 1])
             {:rows 2
              :cols 2
              :cells {[0 0] #{:north}
                      [0 1] #{:south}
                      [1 0] #{}
                      [1 1] #{}}}))
      (is (= (link-cells grid [0 0] [1 1])
             {:rows 2
              :cols 2
              :cells {[0 0] #{}
                      [0 1] #{}
                      [1 0] #{}
                      [1 1] #{}}}))
      (is (= (link-cells grid [0 0] [0 0])
             {:rows 2
              :cols 2
              :cells {[0 0] #{}
                      [0 1] #{}
                      [1 0] #{}
                      [1 1] #{}}})))))


(deftest get-cell-neighbours-test
  (testing "Find coords of cells adjacent to a given cell"
    (let [grid (init 2 2)]
      (is (= (get-cell-neighbours grid [0 0] '(:north :south :east :west))
             [[0 1] [1 0]]))
      (is (= (get-cell-neighbours grid [0 0] '()) [])))))

(deftest get-cell-links-test
  (testing "Creating a grid"
    (let [grid {:rows 2
                :cols 2
                :cells {[0 0] #{:north}
                        [0 1] #{:south :east}
                        [1 0] #{}
                        [1 1] #{:west}}}]
      (is (= (get-cell-links grid [0 1]) [[0 0] [1 1]])))))
