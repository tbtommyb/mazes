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
             {:mask-type :unmasked
              :rows 2
              :cols 2
              :cells {[0 0] {:east '([1 0])}
                      [0 1] {:east '([1 1])}
                      [1 0] {:west '([0 0]) :north '([1 1])}
                      [1 1] {:west '([0 1]) :south '([1 0])}}})))))

(deftest masked-binary-tree-test
  (testing "Applying binary tree to a masked grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (masked/new-grid "test/mazes/test-mask.txt"))
             {:mask-type :masked
              :rows 3
              :cols 5
              :cells {[2 2] {:west '([1 2]) :east '([3 2])}
                      [0 0] {}
                      [1 0] {:north '([1 1])}
                      [1 1] {:west '([0 1]) :south '([1 0]) :north '([1 2])}
                      [4 2] {}
                      [3 0] {:west '([2 0]) :north '([3 1])}
                      [4 1] {:west '([3 1])}
                      [0 2] {}
                      [2 0] {:east '([3 0])}
                      [3 1] {:south '([3 0]) :east '([4 1])}
                      [2 1] {}
                      [1 2] {:south '([1 1]) :east '([2 2])}
                      [3 2] {:west '([2 2])}
                      [0 1] {:east '([1 1])}
                      [4 0] {}}
              :mask {[2 2] true
                     [0 0] false
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

;; (deftest sidewinder-test
;;   (testing "Applying the sidewinder algorithm to a grid"
;;     (binding [gen/*rnd* (java.util.Random. 5)]
;;       (is (= (sidewinder (gr/new-simple-grid 2 2))
;;              (gr/->SimpleGrid 2 2 {[0 0] #{:north}
;;                                    [0 1] #{:south :east}
;;                                    [1 0] #{:north}
;;                                    [1 1] #{:west :south}}))))))
