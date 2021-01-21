(ns mazes.algorithms-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid.grid :as grid]
            [mazes.printer :as pr]
            [mazes.grid.masked :as masked]
            [mazes.grid.weave :as weave]
            [mazes.algorithms :refer :all]))

(deftest binary-tree-test
  (testing "Applying the binary tree algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (grid/new-grid 2 2))
             {:type :cartesian
              :weighting :unweighted
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
              :weighting :unweighted
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
              :weighting :unweighted
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
              :weighting :unweighted
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
              :weighting :unweighted
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
              :weighting :unweighted
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
              :weighting :unweighted
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

(deftest simple-kruskal-test
  (testing "Applying the simple Kruskal algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (simple-kruskal (grid/new-grid 3 3))
             {:type :cartesian
              :weighting :unweighted
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1])}}
                      [0 0] {:links {:east '([1 0]), :north '([0 1])}}
                      [1 0] {:links {:west '([0 0])}}
                      [1 1] {:links {:east '([2 1]) :west '([0 1]) :north '([1 2])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1])}}
                      [2 1] {:links {:north '([2 2]) :west '([1 1]), :south '([2 0])}}
                      [1 2] {:links {:south '([1 1])}}
                      [0 1] {:links {:north '([0 2]), :east '([1 1]), :south '([0 0])}}}})))))

(deftest randomised-kruskal-test
  (testing "Applying the randomised Kruskal algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (randomised-kruskal (weave/new-grid 5 5))
             {:type :cartesian
              :weave :weave
              :weighting :unweighted
              :rows 5
              :cols 5
              :cells {[4 3] {:links {:north '([4 4]) :south '([4 2])}}
                      [2 2] {:links {:west '([1 2]) :east '([3 2])}}
                      [0 0] {:links {:east '([1 0]) :north '([0 1])}}
                      [1 0] {:links {:north-north '([1 2]) :west '([0 0])}}
                      [2 3] {:links {:south-south '([2 1]) :east '([3 3]) :north '([2 4])}}
                      [3 3] {:links {:north '([3 4]) :west '([2 3])}}
                      [1 1] {:links {:west '([0 1]) :east '([2 1])}}
                      [3 4] {:links {:south '([3 3]) :east '([4 4])}}
                      [4 2] {:links {:north '([4 3])}}
                      [3 0] {:links {:north '([3 1]) :west '([2 0]) :east '([4 0])}}
                      [4 1] {:links {:west '([3 1])}}
                      [1 4] {:links {:east '([2 4])}}
                      [1 3] {:links {:west '([0 3]) :south '([1 2])}}
                      [0 3] {:links {:east '([1 3]) :north '([0 4])}}
                      [2 4] {:links {:south '([2 3]) :west '([1 4])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1]) :east '([3 0])}}
                      [0 4] {:links {:south '([0 3])}}
                      [3 1] {:links {:south '([3 0]) :east '([4 1])}}
                      [2 1] {:links {:west '([1 1]) :north-north '([2 3]) :south '([2 0])}}
                      [4 4] {:links {:south '([4 3]) :west '([3 4])}}
                      [1 2] {:links {:south-south '([1 0]) :east '([2 2]) :north '([1 3])}}
                      [3 2] {:links {:west '([2 2])}}
                      [0 1] {:links {:east '([1 1]) :north '([0 2]) :south '([0 0])}}
                      [4 0] {:links {:west '([3 0])}}}})))))

(deftest simplified-prims-test
  (testing "Applying the simplified Prim's algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (simplified-prims (grid/new-grid 3 3))
             {:type :cartesian
              :weighting :unweighted
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1]), :west '([1 2])}}
                      [0 0] {:links {:east '([1 0]), :north '([0 1])}}
                      [1 0] {:links {:east '([2 0]), :west '([0 0])}}
                      [1 1] {:links {:east '([2 1])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1]), :west '([1 0])}}
                      [2 1] {:links {:south '([2 0]), :north '([2 2]), :west '([1 1])}}
                      [1 2] {:links {:east '([2 2])}}
                      [0 1] {:links {:south '([0 0]), :north '([0 2])}}}})))))

(deftest true-prims-test
  (testing "Applying the true Prim's algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (true-prims (grid/new-grid 3 3))
             {:type :cartesian
              :weighting :unweighted
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:south '([2 1])}}
                      [0 0] {:links {:north '([0 1]), :east '([1 0])}}
                      [1 0] {:links {:west '([0 0])}}
                      [1 1] {:links {:east '([2 1]), :north '([1 2])}}
                      [0 2] {:links {:east '([1 2]), :south '([0 1])}}
                      [2 0] {:links {:north '([2 1])}}
                      [2 1] {:links {:south '([2 0]), :west '([1 1]), :north '([2 2])}}
                      [1 2] {:links {:south '([1 1]), :west '([0 2])}}
                      [0 1] {:links {:north '([0 2]), :south '([0 0])}}}})))))

(deftest growing-tree-test
  (testing "Applying the growing tree algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (growing-tree (grid/new-grid 4 4) first)
             {:type :cartesian
              :weighting :unweighted
              :rows 4
              :cols 4
              :cells {[2 2] {:links {:north '([2 3]), :east '([3 2])}}
                      [0 0] {:links {:north '([0 1])}}
                      [1 0] {:links {:east '([2 0]), :north '([1 1])}}
                      [2 3] {:links {:south '([2 2])}}
                      [3 3] {:links {:south '([3 2])}}
                      [1 1] {:links {:south '([1 0]), :west '([0 1])}}
                      [3 0] {:links {:west '([2 0])}}
                      [1 3] {:links {:south '([1 2]), :west '([0 3])}}
                      [0 3] {:links {:east '([1 3])}}
                      [0 2] {:links {:south '([0 1]), :east '([1 2])}}
                      [2 0] {:links {:north '([2 1]), :west '([1 0]), :east '([3 0])}}
                      [3 1] {:links {:north '([3 2]), :west '([2 1])}}
                      [2 1] {:links {:east '([3 1]), :south '([2 0])}}
                      [1 2] {:links {:west '([0 2]), :north '([1 3])}}
                      [3 2] {:links {:west '([2 2]), :north '([3 3]), :south '([3 1])}}
                      [0 1] {:links {:east '([1 1]), :south '([0 0]), :north '([0 2])}}}})))))

(deftest eller-test
  (testing "Applying Eller's algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (eller (grid/new-grid 3 3))
             {:type :cartesian
              :weighting :unweighted
              :rows 3
              :cols 3
              :cells {[2 2] {:links {:west '([1 2]), :south '([2 1])}}
                      [0 0] {:links {:east '([1 0])}}
                      [1 0] {:links {:north '([1 1]), :west '([0 0]), :east '([2 0])}}
                      [1 1] {:links {:west '([0 1]), :south '([1 0])}}
                      [0 2] {:links {:south '([0 1])}}
                      [2 0] {:links {:north '([2 1]), :west '([1 0])}}
                      [2 1] {:links {:north '([2 2]), :south '([2 0])}}
                      [1 2] {:links {:east '([2 2])}}
                      [0 1] {:links {:north '([0 2]), :east '([1 1])}}}})))))

(deftest recursive-division-test
  (testing "Applying recursive division algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (recursive-division (grid/new-grid 6 6))
             {:type :cartesian
              :weighting :unweighted
              :rows 6
              :cols 6
              :cells {[4 3] {:links {:north '([4 4])}}
                      [2 2] {:links {:west '([1 2]) :south '([2 1])}}
                      [0 0] {:links {:north '([0 1])}}
                      [1 0] {:links {:east '([2 0])}}
                      [2 3] {:links {:west '([1 3]) :east '([3 3]) :north '([2 4])}}
                      [2 5] {:links {:west '([1 5])}}
                      [3 3] {:links {:west '([2 3]) :north '([3 4])}}
                      [5 4] {:links {:north '([5 5]) :south '([5 3]) :west '([4 4])}}
                      [1 1] {:links {:north '([1 2])}}
                      [0 5] {:links {:south '([0 4])}}
                      [3 4] {:links {:south '([3 3]) :north '([3 5])}}
                      [4 2] {:links {:east '([5 2])}}
                      [3 0] {:links {:north '([3 1]) :east '([4 0]) :west '([2 0])}}
                      [5 3] {:links {:south '([5 2]) :north '([5 4])}}
                      [4 1] {:links {:south '([4 0])}}
                      [5 2] {:links {:north '([5 3]) :south '([5 1]) :west '([4 2])}}
                      [1 4] {:links {:south '([1 3]) :north '([1 5])}}
                      [1 3] {:links {:north '([1 4]) :south '([1 2]) :east '([2 3]) :west '([0 3])}}
                      [1 5] {:links {:south '([1 4]) :east '([2 5])}}
                      [0 3] {:links {:east '([1 3]) :north '([0 4])}}
                      [5 1] {:links {:south '([5 0]) :north '([5 2])}}
                      [5 5] {:links {:south '([5 4]) :west '([4 5])}}
                      [2 4] {:links {:south '([2 3])}}
                      [4 5] {:links {:east '([5 5])}}
                      [0 2] {:links {:south '([0 1]) :east '([1 2])}}
                      [2 0] {:links {:north '([2 1]) :east '([3 0]) :west '([1 0])}}
                      [0 4] {:links {:south '([0 3]) :north '([0 5])}}
                      [3 1] {:links {:south '([3 0]) :north '([3 2])}}
                      [2 1] {:links {:south '([2 0]) :north '([2 2])}}
                      [4 4] {:links {:south '([4 3]) :east '([5 4])}}
                      [5 0] {:links {:north '([5 1]) :west '([4 0])}}
                      [1 2] {:links {:north '([1 3]) :south '([1 1]) :east '([2 2]) :west '([0 2])}}
                      [3 5] {:links {:south '([3 4])}}
                      [3 2] {:links {:south '([3 1])}}
                      [0 1] {:links {:south '([0 0]) :north '([0 2])}}
                      [4 0] {:links {:north '([4 1]) :east '([5 0]) :west '([3 0])}}}})))))

(deftest recursive-division-test
  (testing "Applying recursive division algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (recursive-division (grid/new-grid 6 6) {:room-p 0.6})
             {:type :cartesian
              :weighting :unweighted
              :rows 6
              :cols 6
              :cells {[4 3] {:links {:south '([4 2]), :east '([5 3]), :north '([4 4])}}
                      [2 2] {:links {:west '([1 2]) :south '([2 1]) :east '([3 2])}}
                      [0 0] {:links {:north '([0 1]) :east '([1 0])}}
                      [1 0] {:links {:north '([1 1]) :east '([2 0]) :west '([0 0])}}
                      [2 3] {:links {:west '([1 3]) :east '([3 3]) :north '([2 4])}}
                      [2 5] {:links {:south '([2 4]) :east '([3 5]) :west '([1 5])}}
                      [3 3] {:links {:west '([2 3]) :north '([3 4])}}
                      [5 4] {:links {:north '([5 5]) :south '([5 3]) :west '([4 4])}}
                      [1 1] {:links {:north '([1 2]) :south '([1 0]) :east '([2 1]) :west '([0 1])}}
                      [0 5] {:links {:south '([0 4]) :east '([1 5])}}
                      [3 4] {:links {:west '([2 4]) :south '([3 3]) :north '([3 5])}}
                      [4 2] {:links {:south '([4 1]) :east '([5 2]) :north '([4 3])}}
                      [3 0] {:links {:north '([3 1]) :east '([4 0]) :west '([2 0])}}
                      [5 3] {:links {:north '([5 4]) :south '([5 2]) :west '([4 3])}}
                      [4 1] {:links {:east '([5 1]) :north '([4 2])}}
                      [5 2] {:links {:north '([5 3]) :south '([5 1]) :west '([4 2])}}
                      [1 4] {:links {:south '([1 3]) :east '([2 4]) :north '([1 5])}}
                      [1 3] {:links {:south '([1 2]) :east '([2 3]) :north '([1 4])}}
                      [1 5] {:links {:south '([1 4]) :east '([2 5]) :west '([0 5])}}
                      [0 3] {:links {:north '([0 4])}}
                      [5 1] {:links {:north '([5 2]) :south '([5 0]) :west '([4 1])}}
                      [5 5] {:links {:south '([5 4]) :west '([4 5])}}
                      [2 4] {:links {:north '([2 5]) :south '([2 3]) :east '([3 4]) :west '([1 4])}}
                      [4 5] {:links {:east '([5 5])}}
                      [0 2] {:links {:south '([0 1]) :east '([1 2])}}
                      [2 0] {:links {:north '([2 1]) :east '([3 0]) :west '([1 0])}}
                      [0 4] {:links {:south '([0 3]) :north '([0 5])}}
                      [3 1] {:links {:west '([2 1]) :south '([3 0]) :north '([3 2])}}
                      [2 1] {:links {:north '([2 2]) :south '([2 0]) :east '([3 1]) :west '([1 1])}}
                      [4 4] {:links {:south '([4 3]) :east '([5 4])}}
                      [5 0] {:links {:north '([5 1]) :west '([4 0])}}
                      [1 2] {:links {:north '([1 3]) :south '([1 1]) :east '([2 2]) :west '([0 2])}}
                      [3 5] {:links {:west '([2 5]) :south '([3 4])}}
                      [3 2] {:links {:west '([2 2]) :south '([3 1])}}
                      [0 1] {:links {:north '([0 2]) :south '([0 0]) :east '([1 1])}}
                      [4 0] {:links {:west '([3 0]) :east '([5 0])}}}})))))
