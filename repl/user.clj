(ns user
  (:require [jamesmacaulay.async-tools.core :as tools]
            [clojure.core.async :as async :refer [go <! <!!]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test]))

(defn run-tests
  []
  (clojure.test/run-all-tests #"jamesmacaulay\..*"))
