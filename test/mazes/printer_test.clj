(ns mazes.printer-test
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

;; TODO start user-input x and y from 1
(deftest ascii-path-smoke-test
  (testing "A simple shortest path renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/init 3 4))
            distances (dist/dijkstra maze 0 0)
            path (dist/shortest-path distances maze (gr/get-cell maze 3 2) (gr/get-cell maze 0 0))]
        (is (= (pr/str-distances-path maze distances path)
               (str "+---+---+---+---+\n"
                    "| 2   3   4   5 |\n"
                    "+   +---+---+---+\n"
                    "| 1             |\n"
                    "+   +   +   +   +\n"
                    "| 0 |   |   |   |\n"
                    "+---+---+---+---+\n")))))))
