(defproject jamesmacaulay/zelkova "0.4.0"
  :description "Elm-style FRP for Clojure and ClojureScript"
  :url "http://github.com/jamesmacaulay/zelkova"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :plugins [[lein-cljsbuild "1.0.5"]
            [com.cemerick/clojurescript.test "0.3.3"]
            [funcool/codeina "0.3.0-SNAPSHOT"]]

  :codeina {:source ["src"]
            :reader :clojurescript
            :src-uri "http://github.com/jamesmacaulay/zelkova/blob/master/"
            :src-uri-prefix "#L"}

  :aliases {"repl" ["with-profile" "repl" "repl"]
            "cljs-test" ["cljsbuild" "test"]
            "cljs-autotest" ["cljsbuild" "auto" "test"]
            "all-tests" ["do" "clean" ["test"] ["cljs-test"]]}
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]
                                  [org.clojure/tools.nrepl "0.2.10"]
                                  [com.cemerick/piggieback "0.2.0"]
                                  [net.cgrand/parsley "0.9.3" :exclusions [org.clojure/clojure]]]
                   :source-paths ["src" "test"]}
             :repl [:dev {:source-paths ["src" "test" "repl"]
                          :test-paths ["test"]}]}
  :prep-tasks ["javac" "compile"]
  :cljsbuild {:test-commands {"phantom" ["phantomjs" :runner "target/testable.js"]
                              "node" ["node" :node-runner "target/testable.js"]}
              :builds [{:id "test"
                        :source-paths ["src" "test"]
                        :notify-command ["phantomjs" :cljs.test/runner "target/testable.js"]
                        :compiler {:output-to "target/testable.js"
                                   :libs [""]
                                        ; node doesn't like source maps I guess?
                                        ;:source-map "target/testable.js.map"
                                   :optimizations :simple
                                   :pretty-print true}}]})
