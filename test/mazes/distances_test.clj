(ns mazes.distances-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid.grid :as grid]
            [mazes.grid.weighted :as weighted]
            [mazes.algorithms :as algo]
            [mazes.printer :as pr]
            [mazes.distances :as dist]
            [mazes.core :refer :all]))

(deftest simple-distances-test
  (testing "A grid with no links has no distances"
    (let [grid (grid/new-grid 3 3)]
      (is (= (dist/dijkstra grid [0 0])
             {[0 0] 0
              [0 1] nil
              [1 0] nil
              [1 1] nil
              [0 2] nil
              [2 0] nil
              [2 1] nil
              [1 2] nil
              [2 2] nil})))))

(deftest maze-distances-test
  (testing "A simple maze has the correct distances"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (grid/new-grid 3 3))]
        (is (= (dist/dijkstra maze [0 0])
               {[2 2] 4
                [0 0] 0
                [1 0] 3
                [1 1] 2
                [0 2] 2
                [2 0] 6
                [2 1] 5
                [1 2] 3
                [0 1] 1}))))))

(deftest shortest-path-test
  (testing "Find the correct shortest path from start to goal"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (grid/new-grid 3 3))]
        (is (= (dist/shortest-path maze [0 0] [2 2])
               {[2 2] 4
                [0 0] 0
                [1 0] nil
                [1 1] nil
                [0 2] 2
                [2 0] nil
                [2 1] nil
                [1 2] 3
                [0 1] 1}))))))

(deftest longest-path-test
  (testing "Find the correct longest path in a given maze"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (grid/new-grid 4 4))]
        (is (= (dist/longest-path maze)
               {[2 2] 7
                [0 0] nil
                [1 0] 0
                [2 3] 6
                [3 3] nil
                [1 1] 1
                [3 0] 10
                [1 3] 5
                [0 3] 4
                [0 2] 3
                [2 0] nil
                [3 1] 9
                [2 1] 8
                [1 2] 2
                [3 2] nil
                [0 1] nil}))))))

(deftest weighted-maze-distances-test
  (testing "A simple maze has the correct distances"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (weighted/new-grid 3 3))]
        (is (= (dist/dijkstra maze [0 0])
               {[2 2] 4
                [0 0] 0
                [1 0] 3
                [1 1] 2
                [0 2] 2
                [2 0] 6
                [2 1] 5
                [1 2] 3
                [0 1] 1}))))))

(deftest modified-path-test
  (testing "Adding a high weight node on the path changes the shortest path"
    (binding [gen/*rnd* (java.util.Random. 4)]
      (let [square (-> (weighted/new-grid 3 3)
                      (algo/recursive-backtracker)
                      (grid/braid))
            with-lava (assoc-in square [:cells [2 0] :weight] 50)]
        (is (= (dist/shortest-path square [0 0] [2 2])
               {[2 2] 4,
                [0 0] 0,
                [1 0] 1,
                [1 1] nil,
                [0 2] nil,
                [2 0] 2,
                [2 1] 3,
                [1 2] nil,
                [0 1] nil}))
        (is (= (dist/shortest-path with-lava [0 0] [2 2])
               {[2 2] 4,
                [0 0] 0,
                [1 0] nil,
                [1 1] nil,
                [0 2] 2,
                [2 0] nil,
                [2 1] nil,
                [1 2] 3,
                [0 1] 1}))))))
