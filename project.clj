(defproject advent2019 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [criterium "0.4.5"]
                 [org.flatland/ordered "1.5.7"]
                 [quil "2.5.0"]
                 [org.clojure/math.combinatorics "0.1.5"]]
  :resource-paths ["resources"
                   "lib/jna.jar"
                   "lib/jna-platform.jar"
                   "lib/VideoExport.jar"]

  :main ^:skip-aot advent2019.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
