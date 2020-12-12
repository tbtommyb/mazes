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
