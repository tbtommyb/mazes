(ns mazes.grid.grid-test
  (:require [clojure.test :refer :all]
            [mazes.cell.cell :as cell]
            [mazes.grid.grid :refer :all]))

(deftest new-grid-test
  (testing "Creating a grid"
    (is (= (new-grid 2 2)
           {:type :cartesian
            :rows 2
            :cols 2
            :cells {[0 0] {}
                    [0 1] {}
                    [1 0] {}
                    [1 1] {}}}))
    (is (= (new-grid 1 2)
           {:type :cartesian
            :rows 1
            :cols 2
            :cells {[0 0] {}
                    [1 0] {}}}))))

(deftest size-test
  (testing "Finding the size of a grid"
    (is (= 4 (size (new-grid 2 2))))))

(deftest get-links-test
  (testing "Testing getting a cell out of a grid"
    (let [grid {:type :cartesian
                :rows 2
                :cols 2
                :cells {[0 0] {:north '(:a)}
                        [0 1] {:south '(:b)}
                        [1 0] {:west '(:c)}
                        [1 1] {:east '(:d)}}}]
      (is (= {:coords [0 0] :links {:north '(:a)}} (get-cell grid [0 0])))
      (is (= {:coords [0 1] :links {:south '(:b)}} (get-cell grid [0 1])))
      (is (= {:coords [1 0] :links {:west '(:c)}} (get-cell grid [1 0])))
      (is (= {:coords [1 1] :links {:east '(:d)}} (get-cell grid [1 1])))
      (is (= nil (get-cell grid [5 1]))))))

(deftest iter-grid-test
  (testing "Iterating through a grid"
    (let [grid (new-grid 2 3)]
      (is (= (iter-grid grid)
             '({:coords [0 0] :links {}}
               {:coords [0 1] :links {}}
               {:coords [1 0] :links {}}
               {:coords [1 1] :links {}}
               {:coords [2 0] :links {}}
               {:coords [2 1] :links {}}))))))

(deftest iter-rows-test
  (testing "Iterating through a grid row by row"
    (let [grid (new-grid 2 3)]
      (is (= (iter-rows grid)
             [[{:coords [0 0] :links {}}
               {:coords [1 0] :links {}}
               {:coords [2 0] :links {}}]
              [{:coords [0 1] :links {}}
               {:coords [1 1] :links {}}
               {:coords [2 1] :links {}}]])))))

;; TODO gross
(deftest direction-between-cells-test
  (testing "Find direction between two cells if neighbouring"
    (let [grid (new-grid 2 2)]
      (is (nil? (direction-between-cells grid (cell/make [0 0]) (cell/make [5 5]))))
      (is (= :north (direction-between-cells grid (cell/make [0 0]) (cell/make [0 1]))))
      (is (= :south (direction-between-cells grid (cell/make [0 1]) (cell/make [0 0]))))
      (is (= :east (direction-between-cells grid (cell/make [0 0]) (cell/make [1 0]))))
      (is (= :west (direction-between-cells grid (cell/make [1 0]) (cell/make [0 0])))))))

(deftest link-coords-test
  (testing "Cells can be linked to each other"
    (let [grid (new-grid 2 2)]
      (is (= (link-cells grid (cell/make [0 0]) (cell/make [0 1]))
             {:type :cartesian
              :rows 2
              :cols 2
              :cells {[0 0] {:north '([0 1])}
                      [0 1] {:south '([0 0])}
                      [1 0] {}
                      [1 1] {}}}))
      (is (= (link-cells grid (cell/make [0 0]) (cell/make [1 1]))
             {:type :cartesian
              :rows 2
              :cols 2
              :cells {[0 0] {}
                      [0 1] {}
                      [1 0] {}
                      [1 1] {}}}))
      (is (= (link-cells grid (cell/make [0 0]) (cell/make [0 0]))
             {:type :cartesian
              :rows 2
              :cols 2
              :cells {[0 0] {}
                      [0 1] {}
                      [1 0] {}
                      [1 1] {}}})))))

(deftest get-neighbouring-cells-test
  (testing "Find cells neighbouring a given cell"
    (let [grid (new-grid 2 2)]
      (is (= (get-neighbouring-cells grid (cell/make [0 0]))
             [{:coords [0 1] :links {}}
              {:coords [1 0] :links {}}]))
      (is (= (get-neighbouring-cells grid (cell/make [0 0]) '()) [])))))
