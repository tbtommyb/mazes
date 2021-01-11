(ns mazes.printer-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as gen]
            [mazes.grid.grid :as grid]
            [mazes.grid.masked :as masked]
            [mazes.grid.polar :as polar]
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

(deftest svg-polar-test
  (testing "SVG polar test prints successfully"
    (binding [gen/*rnd* (java.util.Random. 3)]
      (let [maze (algo/recursive-backtracker (polar/new-grid 3))]
        (is (= (to-svg maze nil)
               [:dali/page {:width 301, :height 301} [:rect {:fill :white} [0 0] [301 301]] '([:dali/page {:fill :none} [:circle {:cx 150, :cy 150, :r 50}]] [:dali/page [:path {:fill :none, :stroke :none} :M [200 150] :L [250 150] :A [50 50] 0.0 false true [200 236] :L [175 193] :A [50 50] 0.0 false false [200 150] :z] [:path {:stroke :black, :fill :none} :M [200 150] :A [50 50] 0.0 false true [175 193]] [:line {:stroke :black, :fill :none} [175 193] [200 236]]] [:dali/page [:path {:fill :none, :stroke :none} :M [175 193] :L [200 236] :A [50 50] 1.0471975511965976 false true [101 236] :L [126 193] :A [50 50] 1.0471975511965976 false false [175 193] :z] [:path {:stroke :black, :fill :none} :M [175 193] :A [50 50] 1.0471975511965976 false true [126 193]] [:line {:stroke :none, :fill :none} [126 193] [101 236]]] [:dali/page [:path {:fill :none, :stroke :none} :M [126 193] :L [101 236] :A [50 50] 2.0943951023931953 false true [50 150] :L [100 150] :A [50 50] 2.0943951023931953 false false [126 193] :z] [:path {:stroke :none, :fill :none} :M [126 193] :A [50 50] 2.0943951023931953 false true [100 150]] [:line {:stroke :black, :fill :none} [100 150] [50 150]]] [:dali/page [:path {:fill :none, :stroke :none} :M [100 150] :L [50 150] :A [50 50] 3.141592653589793 false true [100 64] :L [125 107] :A [50 50] 3.141592653589793 false false [100 150] :z] [:path {:stroke :black, :fill :none} :M [100 150] :A [50 50] 3.141592653589793 false true [125 107]] [:line {:stroke :black, :fill :none} [125 107] [100 64]]] [:dali/page [:path {:fill :none, :stroke :none} :M [125 107] :L [100 64] :A [50 50] 4.1887902047863905 false true [199 64] :L [174 107] :A [50 50] 4.1887902047863905 false false [125 107] :z] [:path {:stroke :black, :fill :none} :M [125 107] :A [50 50] 4.1887902047863905 false true [174 107]] [:line {:stroke :black, :fill :none} [174 107] [199 64]]] [:dali/page [:path {:fill :none, :stroke :none} :M [174 107] :L [199 64] :A [50 50] 5.235987755982988 false true [250 150] :L [200 150] :A [50 50] 5.235987755982988 false false [174 107] :z] [:path {:stroke :black, :fill :none} :M [174 107] :A [50 50] 5.235987755982988 false true [200 150]] [:line {:stroke :black, :fill :none} [200 150] [250 150]]] [:dali/page [:path {:fill :none, :stroke :none} :M [250 150] :L [300 150] :A [100 100] 0.0 false true [279 224] :L [236 199] :A [100 100] 0.0 false false [250 150] :z] [:path {:stroke :none, :fill :none} :M [250 150] :A [100 100] 0.0 false true [236 199]] [:line {:stroke :black, :fill :none} [236 199] [279 224]]] [:dali/page [:path {:fill :none, :stroke :none} :M [236 199] :L [279 224] :A [100 100] 0.5235987755982988 false true [225 279] :L [200 236] :A [100 100] 0.5235987755982988 false false [236 199] :z] [:path {:stroke :none, :fill :none} :M [236 199] :A [100 100] 0.5235987755982988 false true [200 236]] [:line {:stroke :none, :fill :none} [200 236] [225 279]]] [:dali/page [:path {:fill :none, :stroke :none} :M [200 236] :L [225 279] :A [100 100] 1.0471975511965976 false true [150 300] :L [150 250] :A [100 100] 1.0471975511965976 false false [200 236] :z] [:path {:stroke :none, :fill :none} :M [200 236] :A [100 100] 1.0471975511965976 false true [150 250]] [:line {:stroke :black, :fill :none} [150 250] [150 300]]] [:dali/page [:path {:fill :none, :stroke :none} :M [150 250] :L [150 300] :A [100 100] 1.5707963267948966 false true [76 279] :L [101 236] :A [100 100] 1.5707963267948966 false false [150 250] :z] [:path {:stroke :black, :fill :none} :M [150 250] :A [100 100] 1.5707963267948966 false true [101 236]] [:line {:stroke :none, :fill :none} [101 236] [76 279]]] [:dali/page [:path {:fill :none, :stroke :none} :M [101 236] :L [76 279] :A [100 100] 2.0943951023931953 false true [21 225] :L [64 200] :A [100 100] 2.0943951023931953 false false [101 236] :z] [:path {:stroke :none, :fill :none} :M [101 236] :A [100 100] 2.0943951023931953 false true [64 200]] [:line {:stroke :black, :fill :none} [64 200] [21 225]]] [:dali/page [:path {:fill :none, :stroke :none} :M [64 200] :L [21 225] :A [100 100] 2.617993877991494 false true [0 150] :L [50 150] :A [100 100] 2.617993877991494 false false [64 200] :z] [:path {:stroke :black, :fill :none} :M [64 200] :A [100 100] 2.617993877991494 false true [50 150]] [:line {:stroke :none, :fill :none} [50 150] [0 150]]] [:dali/page [:path {:fill :none, :stroke :none} :M [50 150] :L [0 150] :A [100 100] 3.141592653589793 false true [21 76] :L [64 101] :A [100 100] 3.141592653589793 false false [50 150] :z] [:path {:stroke :none, :fill :none} :M [50 150] :A [100 100] 3.141592653589793 false true [64 101]] [:line {:stroke :none, :fill :none} [64 101] [21 76]]] [:dali/page [:path {:fill :none, :stroke :none} :M [64 101] :L [21 76] :A [100 100] 3.665191429188092 false true [75 21] :L [100 64] :A [100 100] 3.665191429188092 false false [64 101] :z] [:path {:stroke :black, :fill :none} :M [64 101] :A [100 100] 3.665191429188092 false true [100 64]] [:line {:stroke :none, :fill :none} [100 64] [75 21]]] [:dali/page [:path {:fill :none, :stroke :none} :M [100 64] :L [75 21] :A [100 100] 4.1887902047863905 false true [150 0] :L [150 50] :A [100 100] 4.1887902047863905 false false [100 64] :z] [:path {:stroke :none, :fill :none} :M [100 64] :A [100 100] 4.1887902047863905 false true [150 50]] [:line {:stroke :black, :fill :none} [150 50] [150 0]]] [:dali/page [:path {:fill :none, :stroke :none} :M [150 50] :L [150 0] :A [100 100] 4.71238898038469 false true [224 21] :L [199 64] :A [100 100] 4.71238898038469 false false [150 50] :z] [:path {:stroke :none, :fill :none} :M [150 50] :A [100 100] 4.71238898038469 false true [199 64]] [:line {:stroke :none, :fill :none} [199 64] [224 21]]] [:dali/page [:path {:fill :none, :stroke :none} :M [199 64] :L [224 21] :A [100 100] 5.235987755982988 false true [279 75] :L [236 100] :A [100 100] 5.235987755982988 false false [199 64] :z] [:path {:stroke :none, :fill :none} :M [199 64] :A [100 100] 5.235987755982988 false true [236 100]] [:line {:stroke :black, :fill :none} [236 100] [279 75]]] [:dali/page [:path {:fill :none, :stroke :none} :M [236 100] :L [279 75] :A [100 100] 5.759586531581287 false true [300 150] :L [250 150] :A [100 100] 5.759586531581287 false false [236 100] :z] [:path {:stroke :none, :fill :none} :M [236 100] :A [100 100] 5.759586531581287 false true [250 150]] [:line {:stroke :none, :fill :none} [250 150] [300 150]]]) [:circle {:fill :none, :stroke :black} [150 150] 150]]))))))
