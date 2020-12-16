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

(deftest iter-rows-test
  (testing "Iterating through a grid row by row"
    (let [grid (init 2 3)]
      (is (= (iter-rows grid)
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
    (let [grid (init 2 2)
          cell (get-cell grid [0 0])]
      (is (nil? (cell-neighbour-at grid cell :south)))
      (is (nil? (cell-neighbour-at grid cell :west)))
      (is (= (cell-neighbour-at grid cell :north)
             (make-cell [0 1])))
      (is (= (cell-neighbour-at grid cell :east)
             (make-cell [1 0]))))))

(deftest direction-between-test
  (testing "Find direction between two cells if neighbouring"
    (is (nil? (direction-between (make-cell [0 0])
                                 (make-cell [5 5]))))
    (is (= :north (direction-between (make-cell [0 0])
                                     (make-cell [0 1]))))
    (is (= :south (direction-between (make-cell [0 1])
                                     (make-cell [0 0]))))
    (is (= :east (direction-between (make-cell [0 0])
                                    (make-cell [1 0]))))
    (is (= :west (direction-between (make-cell [1 0])
                                    (make-cell [0 0]))))))

(deftest link-cells-test
  (testing "Cells can be linked to each other"
    (let [grid (init 2 2)]
      (is (= (link-cells grid (make-cell [0 0]) (make-cell [0 1]))
             {:rows 2
              :cols 2
              :cells {[0 0] #{:north}
                      [0 1] #{:south}
                      [1 0] #{}
                      [1 1] #{}}}))
      (is (= (link-cells grid (make-cell [0 0]) (make-cell [1 1]))
             {:rows 2
              :cols 2
              :cells {[0 0] #{}
                      [0 1] #{}
                      [1 0] #{}
                      [1 1] #{}}}))
      (is (= (link-cells grid (make-cell [0 0]) (make-cell [0 0]))
             {:rows 2
              :cols 2
              :cells {[0 0] #{}
                      [0 1] #{}
                      [1 0] #{}
                      [1 1] #{}}})))))


(deftest get-cell-neighbours-test
  (testing "Find cells adjacent to a given cell"
    (let [grid (init 2 2)
          cell (make-cell [0 0])]
      (is (= (get-cell-neighbours grid cell '(:north :south :east :west))
             [{:coords [0 1] :links #{}}
              {:coords [1 0] :links #{}}]))
      (is (= (get-cell-neighbours grid cell '()) [])))))

(deftest get-cell-links
  (testing "TODO"))
