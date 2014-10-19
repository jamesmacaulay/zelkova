(defproject mario "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                 [org.clojure/clojurescript "0.0-2356"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [jamesmacaulay/zelkova "0.2.0"]
                 [om "0.7.3"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "mario"
              :source-paths ["src"]
              :compiler {
                :output-to "mario.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
