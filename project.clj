(defproject mazes "0.1.0-SNAPSHOT"
  :description "Mazes: create mazes in clojure"
  :url "https://github.com/tbtommyb/mazes"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.generators "1.0.0"]
                 [dali "1.0.2"]]
  :repl-options {:init-ns mazes.core})
