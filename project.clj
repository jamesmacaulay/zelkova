(defproject jamesmacaulay/async-tools "0.1.0-SNAPSHOT"
  :description "Tools for core.async"
  :url "http://github.com/jamesmacaulay/async-tools"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.keminglabs/cljx "0.3.2"]
            [com.cemerick/clojurescript.test "0.3.0"]
            [com.cemerick/austin "0.1.4"]]
  :jar-exclusions [#"\.cljx"]
  :aliases {"repl" ["with-profile" "repl" "repl"]
            "clj-test" ["with-profile" "clj" "test"]
            "clj-clean-test" ["do" "clean," "clj-test"]
            "cljs-test" ["with-profile" "cljs" "cljsbuild" "test"]
            "cljs-clean-test" ["do" "clean," "cljs-test"]
            "cljs-autotest" ["with-profile" "cljs" "cljsbuild" "auto" "test"]
            "cljs-clean-autotest" ["do" "clean" ["cljs-autotest"]]
            "all-tests" ["do" "clean," "clj-test," "cljs-test"]}
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/tools.nrepl "0.2.3"]
                                  [org.clojure/clojurescript "0.0-2156"]
                                  [com.cemerick/clojurescript.test "0.3.0"]]}
             :repl [:dev {:source-paths ["repl" "target/generated/src/clj" "target/generated/src/cljs" "target/generated/test/clj" "target/generated/test/cljs"]
                          :test-paths ["target/generated/test/clj" "target/generated/test/cljs"]}]
             :clj [:dev {:source-paths ["target/generated/src/clj"]
                         :test-paths ["target/generated/test/clj"]}]
             :cljs [:dev {:dependencies [[org.clojure/clojurescript "0.0-2156"]]}]}
  :hooks [cljx.hooks]
  :cljsbuild {:test-commands {"phantom" ["phantomjs" :runner "target/testable.js"]
                              "node" ["node" :node-runner "target/testable.js"]}
              :builds [{:id "test"
                        :source-paths ["target/generated/src/cljs" "target/generated/test/cljs"]
                        :notify-command ["phantomjs" :cljs.test/runner "target/testable.js"]
                        :compiler {:output-to "target/testable.js"
                                   :libs [""]
                                   ; node doesn't like source maps I guess?
                                   ;:source-map "target/testable.js.map"
                                   :optimizations :simple
                                   :pretty-print true}}]}
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
