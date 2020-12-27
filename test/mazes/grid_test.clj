(ns mazes.grid-test
  (:require [clojure.test :refer :all]
            [mazes.grid :refer :all]))

(deftest new-simple-grid-test
  (testing "Creating a grid"
    (is (= (new-simple-grid 2 2)
           (->SimpleGrid 2 2 {[0 0] #{}
                                    [0 1] #{}
                                    [1 0] #{}
                                    [1 1] #{}})))
    (is (= (new-simple-grid 1 2)
           (->SimpleGrid 1 2 {[0 0] #{} [1 0] #{}})))))

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
    (let [grid (new-simple-grid 2 3)]
      (is (= (iter-cells grid)
             '({:coords [0 0] :links #{}}
               {:coords [0 1] :links #{}}
               {:coords [1 0] :links #{}}
               {:coords [1 1] :links #{}}
               {:coords [2 0] :links #{}}
               {:coords [2 1] :links #{}}))))))

(deftest iter-rows-coords-test
  (testing "Iterating through a grid row by row"
    (let [grid (new-simple-grid 2 3)]
      (is (= (iter-rows-coords grid)
             [[[0 0] [1 0] [2 0]] [[0 1] [1 1] [2 1]]])))))

(deftest visible-neighbour-coords-test
  (testing "Get neighbouring cell if it exists"
    (let [grid (new-simple-grid 2 2)]
      (is (nil? (visible-neighbour-coords grid [0 0] :south)))
      (is (nil? (visible-neighbour-coords grid [0 0] :west)))
      (is (= (visible-neighbour-coords grid [0 0] :north) [0 1]))
      (is (= (visible-neighbour-coords grid [0 0] :east) [1 0])))))

(deftest direction-between-test
  (testing "Find direction between two cells if neighbouring"
    (is (nil? (direction-between [0 0] [5 5])))
    (is (= :north (direction-between [0 0] [0 1])))
    (is (= :south (direction-between [0 1] [0 0])))
    (is (= :east (direction-between [0 0] [1 0])))
    (is (= :west (direction-between [1 0] [0 0])))))

(deftest link-cells-test
  (testing "Cells can be linked to each other"
    (let [grid (new-simple-grid 2 2)]
      (is (= (link-cells grid [0 0] [0 1])
             (->SimpleGrid 2 2 {[0 0] #{:north}
                                 [0 1] #{:south}
                                 [1 0] #{}
                                 [1 1] #{}})))
      (is (= (link-cells grid [0 0] [1 1])
             (->SimpleGrid 2 2 {[0 0] #{}
                                [0 1] #{}
                                [1 0] #{}
                                [1 1] #{}})))
      (is (= (link-cells grid [0 0] [0 0])
             (->SimpleGrid 2 2 {[0 0] #{}
                                [0 1] #{}
                                [1 0] #{}
                                [1 1] #{}}))))))


(deftest get-neighbouring-coords-test
  (testing "Find coords of cells adjacent to a given cell"
    (let [grid (new-simple-grid 2 2)]
      (is (= (get-neighbouring-coords grid [0 0] '(:north :south :east :west))
             [[0 1] [1 0]]))
      (is (= (get-neighbouring-coords grid [0 0] '()) [])))))

(deftest get-cell-links-test
  (testing "Creating a grid"
    (let [grid (->SimpleGrid 2
                             2
                             {[0 0] #{:north}
                              [0 1] #{:south :east}
                              [1 0] #{}
                              [1 1] #{:west}})]
      (is (= (get-cell-links grid [0 1]) [[0 0] [1 1]])))))
