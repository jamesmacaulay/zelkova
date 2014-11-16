(ns user
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.async-tools.core-test :as tools-test]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.signal-test :as signals-test]
            [clojure.core.async :as async :refer [go go-loop <! <!! >! >!! chan]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test]
            [cemerick.austin.repls]))

(defn run-tests
  []
  (clojure.test/run-all-tests #"jamesmacaulay\..*"))
