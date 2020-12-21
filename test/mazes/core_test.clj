(ns mazes.core-test
  (:require [clojure.test :refer :all]
            [mazes.grid :as gr]
            [mazes.printer :as pr]
            [mazes.core :refer :all]))

;; (deftest ascii-grid-smoke-test
;;   (testing "A simple grid renders correctly"
;;     (is (= (pr/str-grid (gr/init 3 3))
;;            (str "+---+---+---+\n"
;;                 "|   |   |   |\n"
;;                 "+---+---+---+\n"
;;                 "|   |   |   |\n"
;;                 "+---+---+---+\n"
;;                 "|   |   |   |\n"
;;                 "+---+---+---+\n")))))
