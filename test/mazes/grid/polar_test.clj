(ns mazes.grid.polar-test
  (:require [clojure.test :refer :all]
            [mazes.cell.cell :as cell]
            [mazes.grid.grid :as grid]
            [mazes.grid.polar :refer :all]))

(deftest new-grid-test
  (testing "Creating a polar grid"
    (is (= (new-grid 3)
           {:type :polar
            :weighting :unweighted
            :rows 3
            :cells {[11 2] {:links {}}
                    [2 2] {:links {}}
                    [0 0] {:links {}}
                    [7 2] {:links {}}
                    [1 1] {:links {}}
                    [4 2] {:links {}}
                    [4 1] {:links {}}
                    [5 2] {:links {}}
                    [10 2] {:links {}}
                    [8 2] {:links {}}
                    [5 1] {:links {}}
                    [9 2] {:links {}}
                    [0 2] {:links {}}
                    [3 1] {:links {}}
                    [2 1] {:links {}}
                    [6 2] {:links {}}
                    [1 2] {:links {}}
                    [3 2] {:links {}}
                    [0 1] {:links {}}}}))))

;; TODO handle wrap around
(deftest get-neighbouring-cells-test
  (testing "Finding neighbours of a polar grid"
    (let [grid (new-grid 3)
          mid-row (grid/get-cell grid [1 1])
          top-row (grid/get-cell grid [1 2])]
      (is (= (grid/get-neighbouring-cells grid mid-row '(:cw))
             '({:coords [2 1] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:ccw))
             '({:coords [0 1] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:inner))
             '({:coords [0 0] :links {}})))
      (is (= (grid/get-neighbouring-cells grid top-row '(:inner))
             '({:coords [0 1] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:outer))
             '({:coords [2 2] :links {}}
               {:coords [3 2] :links {}})))
      (is (= (grid/get-neighbouring-cells grid (grid/get-cell grid [0 0]))
             '({:coords [0 1] :links {}}
               {:coords [1 1] :links {}}
               {:coords [2 1] :links {}}
               {:coords [3 1] :links {}}
               {:coords [4 1] :links {}}
               {:coords [5 1] :links {}})))))
  (testing "Finding neighbours of a polar grid with links"
    (let [grid {:type :polar
                :weighting :unweighted
                :rows 3
                :cells {[11 2] {:links {}}
                        [2 2] {:links {}}
                        [0 0] {:links {}}
                        [7 2] {:links {}}
                        [1 1] {:links {}}
                        [4 2] {:links {}}
                        [4 1] {:links {}}
                        [5 2] {:links {}}
                        [10 2] {:links {}}
                        [8 2] {:links {}}
                        [5 1] {:links {}}
                        [9 2] {:links {}}
                        [0 2] {:links {}}
                        [3 1] {:links {:outer '([6 2])}}
                        [2 1] {:links {}}
                        [6 2] {:links {:inner '([3 1])}}
                        [1 2] {:links {}}
                        [3 2] {:links {}}
                        [0 1] {:links {}}}}
          mid-row (grid/get-cell grid [3 1])]
      (is (= (grid/get-neighbouring-cells grid mid-row '(:cw))
             '({:coords [4 1] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:ccw))
             '({:coords [2 1] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:inner))
             '({:coords [0 0] :links {}})))
      (is (= (grid/get-neighbouring-cells grid mid-row '(:outer))
             '({:coords [6 2] :links {:inner ([3 1])}}
               {:coords [7 2] :links {}}))))))

(deftest get-neighbouring-cells-test
  (testing "Finding neighbours of a polar grid"
    (let [grid {:type :polar
                :weighting :unweighted
                :rows 3
                :cells {[11 2] {:links {}}
                        [2 2] {:links {}}
                        [0 0] {:links {}}
                        [7 2] {:links {}}
                        [1 1] {:links {}}
                        [4 2] {:links {}}
                        [4 1] {:links {}}
                        [5 2] {:links {}}
                        [10 2] {:links {}}
                        [8 2] {:links {}}
                        [5 1] {:links {}}
                        [9 2] {:links {}}
                        [0 2] {:links {}}
                        [3 1] {:links {:outer '([6 2])}}
                        [2 1] {:links {}}
                        [6 2] {:links {:inner '([3 1])}}
                        [1 2] {:links {}}
                        [3 2] {:links {}}
                        [0 1] {:links {}}}}
          mid-row (grid/get-cell grid [3 1])]
      (is (= (grid/direction-between-cells grid (cell/make [3 1]) (cell/make [4 1]))
             :cw))
      (is (= (grid/direction-between-cells grid (cell/make [4 1]) (cell/make [3 1]))
             :ccw)))))
