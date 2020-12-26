(ns mazes.distances-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid :as gr]
            [mazes.algorithms :as algo]
            [mazes.printer :as pr]
            [mazes.distances :as dist]
            [mazes.core :refer :all]))

(deftest simple-distances-test
  (testing "A grid with no links has no distances"
    (let [grid (gr/new-simple-grid 3 3)]
      (is (= (dist/dijkstra grid [0 0])
             {[0 0] 0
              [0 1] 2147483647
              [1 0] 2147483647
              [1 1] 2147483647
              [0 2] 2147483647
              [2 0] 2147483647
              [2 1] 2147483647
              [1 2] 2147483647
              [2 2] 2147483647})))))

(deftest maze-distances-test
  (testing "A simple maze has the correct distances"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/new-simple-grid 3 3))]
        (is (= (dist/dijkstra maze [0 0])
               {[0 0] 0
                [0 1] 1
                [0 2] 4
                [1 0] 3
                [1 1] 2
                [1 2] 3
                [2 0] 6
                [2 1] 5
                [2 2] 4}))))))

(deftest shortest-path-test
  (testing "Find the correct shortest path from start to goal"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/new-simple-grid 3 3))]
        (is (= (dist/shortest-path maze [0 0] [2 2])
               {[2 2] 4
                [0 0] 0
                [1 0] 2147483647
                [1 1] 2
                [0 2] 2147483647
                [2 0] 2147483647
                [2 1] 2147483647
                [1 2] 3
                [0 1] 1}))))))

(deftest longest-path-test
  (testing "Find the correct longest path in a given maze"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/new-simple-grid 4 4))]
        (is (= (dist/longest-path maze)
               {[0 0] 0
                [0 1] 1
                [0 2] 2
                [0 3] 2147483647
                [1 0] 2147483647
                [1 1] 2147483647
                [1 2] 3
                [1 3] 4
                [2 0] 10
                [2 1] 9
                [2 2] 2147483647
                [2 3] 5
                [3 0] 2147483647
                [3 1] 8
                [3 2] 7
                [3 3] 6}))))))
