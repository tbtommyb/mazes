(ns mazes.algorithms-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid :as gr]
            [mazes.algorithms :refer :all]))

(deftest binary-tree-test
  (testing "Applying the binary tree algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (gr/new-simple-grid 2 2))
             (gr/->SimpleGrid 2 2 {[0 0] #{:east}
                                   [0 1] #{:east}
                                   [1 0] #{:west :north}
                                   [1 1] #{:west :south}}))))))

(deftest sidewinder-test
  (testing "Applying the sidewinder algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (sidewinder (gr/new-simple-grid 2 2))
             (gr/->SimpleGrid 2 2 {[0 0] #{:north}
                                   [0 1] #{:south :east}
                                   [1 0] #{:north}
                                   [1 1] #{:west :south}}))))))
