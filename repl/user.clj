(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test]))

(defn run-tests
  []
  (clojure.test/run-all-tests #"jamesmacaulay\..*"))
