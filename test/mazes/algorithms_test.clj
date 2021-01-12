(ns mazes.algorithms-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid.grid :as grid]
            [mazes.printer :as pr]
            [mazes.grid.masked :as masked]
            [mazes.algorithms :refer :all]))

(deftest binary-tree-test
  (testing "Applying the binary tree algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (grid/new-grid 2 2))
             {:type :cartesian
              :rows 2
              :cols 2
              :cells {[0 0] {:links {:east '([1 0])}}
                      [0 1] {:links {:east '([1 1])}}
                      [1 0] {:links {:west '([0 0]) :north '([1 1])}}
                      [1 1] {:links {:west '([0 1]) :south '([1 0])}}}})))))

(deftest masked-binary-tree-test
  (testing "Applying binary tree to a masked grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (masked/new-grid "test/mazes/test-mask.txt"))
             {:type :cartesian
              :rows 3
              :cols 5
              :cells {[2 2] {:links {:west '([1 2]), :east '([3 2])}}
                      [0 0] {:links {:east '([1 0])}}
                      [1 0] {:links {:west '([0 0]) :north '([1 1])}}
                      [1 1] {:links {:west '([0 1]), :south '([1 0]), :north '([1 2])}}
                      [4 2] {:links {}}
                      [3 0] {:links {:west '([2 0]), :north '([3 1])}}
                      [4 1] {:links {}}
                      [0 2] {:links {}}
                      [2 0] {:links {:east '([3 0])}}
                      [3 1] {:links {:south '([3 0]), :north '([3 2])}}
                      [2 1] {:links {}}
                      [1 2] {:links {:south '([1 1]), :east '([2 2])}}
                      [3 2] {:links {:west '([2 2]), :south '([3 1])}}
                      [0 1] {:links {:east '([1 1])}}
                      [4 0] {:links {}}}
              :mask {[2 2] true
                     [0 0] true
                     [1 0] true
                     [1 1] true
                     [4 2] false
                     [3 0] true
                     [4 1] true
                     [0 2] false
                     [2 0] true
                     [3 1] true
                     [2 1] false
                     [1 2] true
                     [3 2] true
                     [0 1] true
                     [4 0] false}})))))

(deftest sidewinder-test
  (testing "Applying the sidewinder algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (sidewinder (grid/new-grid 2 2))
             {:type :cartesian
              :rows 2
              :cols 2
              :cells {[0 0] {:links {:north '([0 1])}}
                      [0 1] {:links {:south '([0 0]) :east '([1 1])}}
                      [1 0] {:links {:north '([1 1])}}
                      [1 1] {:links {:west '([0 1]) :south '([1 0])}}}})))))

(deftest aldous-broder-test
  (testing "Applying the Aldous-Broder algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (aldous-broder (grid/new-grid 3 3))
             {:type :cartesian
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:west '([1 2])}}
                      [0 0] {:links {:east '([1 0])}}
                      [1 0] {:links {:east '([2 0]) :north '([1 1]) :west '([0 0])}}
                      [1 1] {:links {:south '([1 0]) :west '([0 1])}}
                      [0 2] {:links {:south '([0 1]) :east '([1 2])}}
                      [2 0] {:links {:north '([2 1]) :west '([1 0])}}
                      [2 1] {:links {:south '([2 0])}}
                      [1 2] {:links {:west '([0 2]) :east '([2 2])}}
                      [0 1] {:links {:east '([1 1]) :north '([0 2])}}}})))))

(deftest wilson-test
  (testing "Applying the Wilson algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (wilson (grid/new-grid 3 3))
             {:type :cartesian
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1])}}
                      [0 0] {:links {:north '([0 1])}}
                      [1 0] {:links {:east '([2 0])}}
                      [1 1] {:links {:east '([2 1]) :west '([0 1]) :north '([1 2])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1]) :west '([1 0])}}
                      [2 1] {:links {:south '([2 0]) :west '([1 1]) :north '([2 2])}}
                      [1 2] {:links {:south '([1 1])}}
                      [0 1] {:links {:east '([1 1]) :south '([0 0]) :north '([0 2])}}}})))))

(deftest hunt-and-kill-test
  (testing "Applying the hunt and kill algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (hunt-and-kill (grid/new-grid 3 3))
             {:type :cartesian
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1]) :west '([1 2])}}
                      [0 0] {:links {:north '([0 1]) :east '([1 0])}}
                      [1 0] {:links {:west '([0 0])}}
                      [1 1] {:links {:north '([1 2]) :west '([0 1])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1])}}
                      [2 1] {:links {:south '([2 0]) :north '([2 2])}}
                      [1 2] {:links {:east '([2 2]) :south '([1 1])}}
                      [0 1] {:links {:east '([1 1]) :north '([0 2]) :south '([0 0])}}}})))))

(deftest recursive-backtracker-test
  (testing "Applying the hunt and kill algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (recursive-backtracker (grid/new-grid 3 3))
             {:type :cartesian
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1]), :west '([1 2])}}
                      [0 0] {:links {:north '([0 1])}}
                      [1 0] {:links {:east '([2 0])}}
                      [1 1] {:links {:west '([0 1])}}
                      [0 2] {:links {:east '([1 2]) :south '([0 1])}}
                      [2 0] {:links {:north '([2 1]) :west '([1 0])}}
                      [2 1] {:links {:south '([2 0]) :north '([2 2])}}
                      [1 2] {:links {:east '([2 2]) :west '([0 2])}}
                      [0 1] {:links {:north '([0 2]) :east '([1 1]) :south '([0 0])}}}})))))
