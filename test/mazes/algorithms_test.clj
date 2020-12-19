(ns mazes.algorithms-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid :as gr]
            [mazes.algorithms :refer :all]))

(deftest binary-tree-test
  (testing "Applying the binary tree algorithm to a grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (is (= (binary-tree (gr/init 2 2))
             {:rows 2
              :cols 2
              :cells {[0 0] #{:east}
                      [0 1] #{:east}
                      [1 0] #{:west :north}
                      [1 1] #{:west :south}}})))))
