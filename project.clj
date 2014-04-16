(defproject jamesmacaulay/async-tools "0.1.0-SNAPSHOT"
  :description "Tools for core.async"
  :url "http://github.com/jamesmacaulay/async-tools"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :source-paths ["src/clj" "src/cljx" "target/classes"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :jar-exclusions [#"\.cljx"]
  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2156"]]
                   ; :hooks [cljx.hooks]
                   :plugins [[lein-cljsbuild "1.0.2"]
                             [com.keminglabs/cljx "0.3.2"]
                             [com.cemerick/clojurescript.test "0.3.0"]
                             [com.cemerick/austin "0.1.4"]]
                   :cljx {:builds [{:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :clj}
                                   {:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :cljs}
                                   {:source-paths ["test/cljx"]
                                    :output-path "target/test-classes"
                                    :rules :clj}
                                   {:source-paths ["test/cljx"]
                                    :output-path "target/test-classes"
                                    :rules :cljs}]}
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test," "cljsbuild" "test"]}
                   :cljsbuild {:test-commands {"phantom" ["phantomjs" :runner "target/testable.js"]
                                               "node" ["node" :node-runner "target/testable.js"]}
                               :builds [{:source-paths ["target/classes" "target/test-classes"]
                                         :compiler {:output-to "target/testable.js"
                                                    :libs [""]
                                                    ; node doesn't like source maps I guess?
                                                    ;:source-map "target/testable.js.map"
                                                    :optimizations :simple
                                                    :pretty-print true}}]}}})
