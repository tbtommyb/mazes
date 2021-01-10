(ns mazes.printer-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid.grid :as grid]
            [mazes.grid.masked :as masked]
            [mazes.distances :as dist]
            [mazes.algorithms :as algo]
            [mazes.printer :refer :all]))

(deftest ascii-grid-test
  (testing "A grid is correctly rendered in ASCII"
    (let [grid {:mask-type :unmasked
                :rows 2
                :cols 2
                :cells {[0 0] {:east '([1 0])}
                        [0 1] {:east '([1 1])}
                        [1 0] {:west '([0 0]) :north '([1 1])}
                        [1 1] {:west '([0 1]) :south '([1 0])}}}]
      (is (= (ascii-grid grid)
             '("+" "---+" "---+" "\n"
               "|" "    " "   |" "\n"
               "+" "---+" "   +" "\n"
               "|" "    " "   |" "\n"
               "+" "---+" "---+" "\n"))))))

(deftest ascii-masked-test
  (testing "Printing a masked grid"
    (binding [gen/*rnd* (java.util.Random. 5)]
      (let [maze (algo/binary-tree (masked/new-grid "test/mazes/test-mask.txt"))]
        (is (= (ascii-grid maze)
               '("+" "---+" "---+" "---+" "---+" "---+" "\n"
                 "|" "   |" "    " "    " "   |" "   |" "\n"
                 "+" "---+" "   +" "---+" "   +" "---+" "\n"
                 "|" "    " "   |" "   |" "   |" "   |" "\n"
                 "+" "---+" "   +" "---+" "   +" "---+" "\n"
                 "|" "    " "   |" "    " "   |" "   |" "\n"
                 "+" "---+" "---+" "---+" "---+" "---+" "\n")))))))

(deftest ascii-distances-test
  (testing "A simple grid with distances renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (grid/new-grid 3 3))
            distances (dist/dijkstra maze [0 0])]
        (is (= (ascii-grid maze {:distances distances})
               '("+" "---+" "---+" "---+" "\n"
                 "|" " 2  " " 3  " " 4 |" "\n"
                 "+" "   +" "---+" "   +" "\n"
                 "|" " 1  " " 2 |" " 5 |" "\n"
                 "+" "   +" "   +" "   +" "\n"
                 "|" " 0 |" " 3 |" " 6 |" "\n"
                 "+" "---+" "---+" "---+" "\n")))))))

(deftest ascii-path-test
  (testing "A simple path renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/sidewinder (grid/new-grid 4 4))]
        (is (= (ascii-grid maze {:distances (dist/longest-path maze)})
               '("+" "---+" "---+" "---+" "---+" "\n"
                 "|" " 4  " " 5  " " 6  " "   |" "\n"
                 "+" "   +" "---+" "   +" "   +" "\n"
                 "|" " 3  " " 2 |" " 7 |" "   |" "\n"
                 "+" "   +" "   +" "   +" "---+" "\n"
                 "|" "   |" " 1 |" " 8  " " 9 |" "\n"
                 "+" "   +" "   +" "   +" "   +" "\n"
                 "|" "   |" " 0 |" "   |" " a |" "\n"
                 "+" "---+" "---+" "---+" "---+" "\n")))))))

(deftest ascii-masked-distances-test
  (testing "Distances in a masked grid render correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/aldous-broder (masked/new-grid "test/mazes/test-mask.txt"))]
        (is (= (ascii-grid maze {:distances (dist/dijkstra maze [0 0])})
               '("+" "---+" "---+" "---+" "---+" "---+" "\n"
                 "|" "   |" " 3 |" " 6  " " 5 |" "   |" "\n"
                 "+" "---+" "   +" "---+" "   +" "---+" "\n"
                 "|" " 3  " " 2 |" "   |" " 4  " " 5 |" "\n"
                 "+" "---+" "   +" "---+" "   +" "---+" "\n"
                 "|" " 0  " " 1  " " 2  " " 3 |" "   |" "\n"
                 "+" "---+" "---+" "---+" "---+" "---+" "\n")))))))

(deftest svg-distances-test
  (testing "SVG maze with coloured distances renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/aldous-broder (grid/new-grid 3 3))]
        (is (= (to-svg maze (dist/dijkstra maze [0 0]))
               [:dali/page {:width 150, :height 150}
                [:rect {:fill :white} [0 0] [150 150]]
                '([:rect {:stroke "rgb(255,255,255)", :fill "rgb(255,255,255)"} [0 100] [50 50]]
                  [:rect {:stroke "rgb(0,128,0)", :fill "rgb(0,128,0)"} [0 50] [50 50]]
                  [:rect {:stroke "rgb(36,146,36)", :fill "rgb(36,146,36)"} [0 0] [50 50]]
                  [:rect {:stroke "rgb(218,236,218)", :fill "rgb(218,236,218)"} [50 100] [50 50]]
                  [:rect {:stroke "rgb(36,146,36)", :fill "rgb(36,146,36)"} [50 50] [50 50]]
                  [:rect {:stroke "rgb(72,164,72)", :fill "rgb(72,164,72)"} [50 0] [50 50]]
                  [:rect {:stroke "rgb(182,218,182)", :fill "rgb(182,218,182)"} [100 100] [50 50]]
                  [:rect {:stroke "rgb(145,200,145)", :fill "rgb(145,200,145)"} [100 50] [50 50]]
                  [:rect {:stroke "rgb(109,182,109)", :fill "rgb(109,182,109)"} [100 0] [50 50]])
                '([:dali/page
                   [:line {:stroke :black} [0 100] [50 100]]
                   [:line {:stroke "rgb(255,255,255)"} [50 150] [50 100]]
                   [:line {:stroke :black} [0 150] [50 150]]
                   [:line {:stroke :black} [0 150] [0 100]]]
                  [:dali/page
                   [:line {:stroke :black} [0 50] [50 50]]
                   [:line {:stroke "rgb(0,128,0)"} [50 100] [50 50]]
                   [:line {:stroke :black} [0 100] [50 100]]
                   [:line {:stroke :black} [0 100] [0 50]]]
                  [:dali/page
                   [:line {:stroke :black} [0 0] [50 0]]
                   [:line {:stroke "rgb(36,146,36)"} [50 50] [50 0]]
                   [:line {:stroke :black} [0 50] [50 50]]
                   [:line {:stroke :black} [0 50] [0 0]]]
                  [:dali/page
                   [:line {:stroke :black} [50 100] [100 100]]
                   [:line {:stroke "rgb(218,236,218)"} [100 150] [100 100]]
                   [:line {:stroke :black} [50 150] [100 150]]
                   [:line {:stroke "rgb(218,236,218)"} [50 150] [50 100]]]
                  [:dali/page
                   [:line {:stroke "rgb(36,146,36)"} [50 50] [100 50]]
                   [:line {:stroke :black} [100 100] [100 50]]
                   [:line {:stroke :black} [50 100] [100 100]]
                   [:line {:stroke "rgb(36,146,36)"} [50 100] [50 50]]]
                  [:dali/page
                   [:line {:stroke :black} [50 0] [100 0]]
                   [:line {:stroke "rgb(72,164,72)"} [100 50] [100 0]]
                   [:line {:stroke "rgb(72,164,72)"} [50 50] [100 50]]
                   [:line {:stroke "rgb(72,164,72)"} [50 50] [50 0]]]
                  [:dali/page
                   [:line {:stroke "rgb(182,218,182)"} [100 100] [150 100]]
                   [:line {:stroke :black} [150 150] [150 100]]
                   [:line {:stroke :black} [100 150] [150 150]]
                   [:line {:stroke "rgb(182,218,182)"} [100 150] [100 100]]]
                  [:dali/page
                   [:line {:stroke "rgb(145,200,145)"} [100 50] [150 50]]
                   [:line {:stroke :black} [150 100] [150 50]]
                   [:line {:stroke "rgb(145,200,145)"} [100 100] [150 100]]
                   [:line {:stroke :black} [100 100] [100 50]]]
                  [:dali/page
                   [:line {:stroke :black} [100 0] [150 0]]
                   [:line {:stroke :black} [150 50] [150 0]]
                   [:line {:stroke "rgb(109,182,109)"} [100 50] [150 50]]
                   [:line {:stroke "rgb(109,182,109)"} [100 50] [100 0]]])]))))))

(deftest svg-masked-distances-test
  (testing "SVG masked maze with coloured distances renders correctly"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/aldous-broder (masked/new-grid "test/mazes/test-mask.txt"))]
        (is (= (to-svg maze (dist/dijkstra maze [0 0]))
               [:dali/page {:width 250, :height 150}
                [:rect {:fill :white} [0 0] [250 150]]
                '([:rect {:stroke "rgb(255,255,255)", :fill "rgb(255,255,255)"} [0 100] [50 50]]
                 [:rect {:stroke "rgb(127,191,127)", :fill "rgb(127,191,127)"} [0 50] [50 50]]
                 [:rect {:stroke "rgb(212,233,212)", :fill "rgb(212,233,212)"} [50 100] [50 50]]
                 [:rect {:stroke "rgb(170,212,170)", :fill "rgb(170,212,170)"} [50 50] [50 50]]
                 [:rect {:stroke "rgb(127,191,127)", :fill "rgb(127,191,127)"} [50 0] [50 50]]
                 [:rect {:stroke "rgb(170,212,170)", :fill "rgb(170,212,170)"} [100 100] [50 50]]
                 [:rect {:stroke "rgb(0,128,0)", :fill "rgb(0,128,0)"} [100 0] [50 50]]
                 [:rect {:stroke "rgb(127,191,127)", :fill "rgb(127,191,127)"} [150 100] [50 50]]
                 [:rect {:stroke "rgb(85,170,85)", :fill "rgb(85,170,85)"} [150 50] [50 50]]
                 [:rect {:stroke "rgb(42,149,42)", :fill "rgb(42,149,42)"} [150 0] [50 50]]
                 [:rect {:stroke "rgb(42,149,42)", :fill "rgb(42,149,42)"} [200 50] [50 50]])
                '([:dali/page
                  [:line {:stroke :black} [0 100] [50 100]]
                  [:line {:stroke "rgb(255,255,255)"} [50 150] [50 100]]
                  [:line {:stroke :black} [0 150] [50 150]]
                  [:line {:stroke :black} [0 150] [0 100]]]
                 [:dali/page
                  [:line {:stroke :black} [0 50] [50 50]]
                  [:line {:stroke "rgb(127,191,127)"} [50 100] [50 50]]
                  [:line {:stroke :black} [0 100] [50 100]]
                  [:line {:stroke :black} [0 100] [0 50]]]
                 [:dali/page
                  [:line {:stroke "rgb(212,233,212)"} [50 100] [100 100]]
                  [:line {:stroke "rgb(212,233,212)"} [100 150] [100 100]]
                  [:line {:stroke :black} [50 150] [100 150]]
                  [:line {:stroke "rgb(212,233,212)"} [50 150] [50 100]]]
                 [:dali/page
                  [:line {:stroke "rgb(170,212,170)"} [50 50] [100 50]]
                  [:line {:stroke :black} [100 100] [100 50]]
                  [:line {:stroke "rgb(170,212,170)"} [50 100] [100 100]]
                  [:line {:stroke "rgb(170,212,170)"} [50 100] [50 50]]]
                 [:dali/page
                  [:line {:stroke :black} [50 0] [100 0]]
                  [:line {:stroke :black} [100 50] [100 0]]
                  [:line {:stroke "rgb(127,191,127)"} [50 50] [100 50]]
                  [:line {:stroke :black} [50 50] [50 0]]]
                 [:dali/page
                  [:line {:stroke :black} [100 100] [150 100]]
                  [:line {:stroke "rgb(170,212,170)"} [150 150] [150 100]]
                  [:line {:stroke :black} [100 150] [150 150]]
                  [:line {:stroke "rgb(170,212,170)"} [100 150] [100 100]]]
                 [:dali/page
                  [:line {:stroke :black} [100 0] [150 0]]
                  [:line {:stroke "rgb(0,128,0)"} [150 50] [150 0]]
                  [:line {:stroke :black} [100 50] [150 50]]
                  [:line {:stroke :black} [100 50] [100 0]]]
                 [:dali/page
                  [:line {:stroke "rgb(127,191,127)"} [150 100] [200 100]]
                  [:line {:stroke :black} [200 150] [200 100]]
                  [:line {:stroke :black} [150 150] [200 150]]
                  [:line {:stroke "rgb(127,191,127)"} [150 150] [150 100]]]
                 [:dali/page
                  [:line {:stroke "rgb(85,170,85)"} [150 50] [200 50]]
                  [:line {:stroke "rgb(85,170,85)"} [200 100] [200 50]]
                  [:line {:stroke "rgb(85,170,85)"} [150 100] [200 100]]
                  [:line {:stroke :black} [150 100] [150 50]]]
                 [:dali/page
                  [:line {:stroke :black} [150 0] [200 0]]
                  [:line {:stroke :black} [200 50] [200 0]]
                  [:line {:stroke "rgb(42,149,42)"} [150 50] [200 50]]
                  [:line {:stroke "rgb(42,149,42)"} [150 50] [150 0]]]
                 [:dali/page
                  [:line {:stroke :black} [200 50] [250 50]]
                  [:line {:stroke :black} [250 100] [250 50]]
                  [:line {:stroke :black} [200 100] [250 100]]
                  [:line {:stroke "rgb(42,149,42)"} [200 100] [200 50]]])]))))))
