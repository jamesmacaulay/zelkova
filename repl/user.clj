(ns user
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.async-tools.signals :as signals]
            [jamesmacaulay.async-tools.signals-test :as signals-test]
            [clojure.core.async :as async :refer [go <! <!!]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test]
            [cemerick.austin.repls]))

(defn run-tests
  []
  (clojure.test/run-all-tests #"jamesmacaulay\..*"))
