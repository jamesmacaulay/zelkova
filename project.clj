(defproject jamesmacaulay/async-tools "0.1.0-SNAPSHOT"
  :description "Tools for core.async"
  :url "http://github.com/jamesmacaulay/async-tools"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]]
  :plugins [[com.keminglabs/cljx "0.3.2"]]
  :jar-exclusions [#"\.cljx"]
  :aliases {"repl" ["with-profile" "clj" "repl"]
            "clj-test" ["with-profile" "clj" "test"]
            "clj-clean-test" ["do" "clean," "clj-test"]
            "cljs-test" ["with-profile" "cljs" "cljsbuild" "test"]
            "cljs-clean-test" ["do" "clean," "cljs-test"]
            "all-tests" ["do" "clean," "clj-test," "cljs-test"]}
  :profiles {:clj {:dependencies [[com.cemerick/clojurescript.test "0.3.0"]]
                   :source-paths ["src/clj" "target/generated/src/clj"]
                   :test-paths ["test/clj" "target/generated/test/clj"]}
             :cljs {:dependencies [[org.clojure/clojurescript "0.0-2156"]]
                    :plugins [[lein-cljsbuild "1.0.2"]
                              [com.cemerick/clojurescript.test "0.3.0"]
                              [com.cemerick/austin "0.1.4"]]
                    :source-paths ["src/clj"]
                    :cljsbuild {:test-commands {"phantom" ["phantomjs" :runner "target/testable.js"]
                                                "node" ["node" :node-runner "target/testable.js"]}
                                :builds [{:source-paths ["src/cljs" "target/generated/src/cljs" "target/generated/test/cljs"]
                                          :compiler {:output-to "target/testable.js"
                                                     :libs [""]
                                                     ; node doesn't like source maps I guess?
                                                     ;:source-map "target/testable.js.map"
                                                     :optimizations :simple
                                                     :pretty-print true}}]}}}
  :hooks [cljx.hooks]
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/clj"
                   :rules :clj}
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/cljs"
                   :rules :cljs}]})
