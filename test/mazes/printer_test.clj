(ns mazes.printer-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid :as gr]
            [mazes.distances :as dist]
            [mazes.algorithms :as algo]
            [mazes.printer :refer :all]))

(deftest ascii-grid-test
  (testing "A grid is correctly rendered in ASCII"
    (let [grid {:rows 2
                :cols 2
                :cells {[0 0] #{:east}
                        [0 1] #{:east}
                        [1 0] #{:west :north}
                        [1 1] #{:west :south}}}]
      (is (= (ascii-grid grid)
             '("+" "---+" "---+" "\n"
               "|" "    " "   |" "\n"
               "+" "---+" "   +" "\n"
               "|" "    " "   |" "\n"
               "+" "---+" "---+" "\n"))))))

(deftest ascii-distances-test
  (testing "A simple grid with distances renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/new-simple-grid 3 3))
            distances (dist/dijkstra maze [0 0])]
        (is (= (ascii-grid maze {:distances distances})
               '("+" "---+" "---+" "---+" "\n"
                 "|" " 4  " " 3  " " 4 |" "\n"
                 "+" "---+" "   +" "   +" "\n"
                 "|" " 1  " " 2 |" " 5 |" "\n"
                 "+" "   +" "   +" "   +" "\n"
                 "|" " 0 |" " 3 |" " 6 |" "\n"
                 "+" "---+" "---+" "---+" "\n")))))))

(deftest ascii-path-test
  (testing "A simple path renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (gr/new-simple-grid 4 4))]
        (is (= (ascii-grid maze {:distances (dist/longest-path maze)})
               '("+" "---+" "---+" "---+" "---+" "\n"
                 "|" "    " " 4  " " 5  " " 6 |" "\n"
                 "+" "---+" "   +" "   +" "   +" "\n"
                 "|" " 2  " " 3 |" "   |" " 7 |" "\n"
                 "+" "   +" "   +" "---+" "   +" "\n"
                 "|" " 1 |" "   |" " 9  " " 8 |" "\n"
                 "+" "   +" "   +" "   +" "   +" "\n"
                 "|" " 0 |" "   |" " a |" "   |" "\n"
                 "+" "---+" "---+" "---+" "---+" "\n")))))))
