(ns mazes.distances-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid :as gr]
            [mazes.algorithms :as algo]
            [mazes.printer :as pr]
            [mazes.distances :as dist]
            [mazes.core :refer :all]))

(deftest ascii-grid-distances-smoke-test
  (testing "A simple grid with distances renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/init 3 3))
            distances (dist/dijkstra maze 0 0)]
        (is (= (pr/str-grid-distances maze distances)
               (str "+---+---+---+\n"
                    "| 4   3   4 |\n"
                    "+---+   +   +\n"
                    "| 1   2 | 5 |\n"
                    "+   +   +   +\n"
                    "| 0 | 3 | 6 |\n"
                    "+---+---+---+\n")))))))

(deftest simple-distances-test
  (testing "A grid with no links has no distances"
    (let [grid (gr/init 3 3)]
      (is (= (dist/dijkstra grid 0 0)
             {"0,0" 0
              "0,1" 2147483647
              "1,0" 2147483647
              "1,1" 2147483647
              "0,2" 2147483647
              "2,0" 2147483647
              "2,1" 2147483647
              "1,2" 2147483647
              "2,2" 2147483647})))))

;; TODO: x and y confusion again
(deftest maze-distances-test
  (testing "A simple maze has the correct distances"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/init 3 3))]
        (is (= (dist/dijkstra maze 0 0)
               {"2,1" 5,
                "2,2" 4,
                "0,2" 4,
                "1,0" 3,
                "0,1" 1,
                "2,0" 6,
                "1,1" 2,
                "0,0" 0,
                "1,2" 3}))))))
