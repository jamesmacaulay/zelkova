#+clj
(ns jamesmacaulay.zelkova.impl.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is are testing)])
  (:import [java.util.Date]))

#+cljs
(ns jamesmacaulay.zelkova.impl.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is are testing)])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(deftest-async test-msg-fn-takes-event-and-previous-message-and-sequence-of-source-messages-and-returns-a-message
  (go
    (let [in1 (z/input 0 :in1)
          in2 (z/input 0 :in2)
          sig (impl/make-signal {:init-fn (constantly :test-init)
                                 :sources [in1 in2]
                                 :msg-fn  (fn [& args] (impl/fresh args))})
          live-graph (z/spawn sig)
          output (async/tap live-graph
                            (chan 1 impl/fresh-values))
          event1 (impl/->Event :in1 1 101)
          event2 (impl/->Event :in2 2 102)
          expected1 [event1
                     (impl/fresh :test-init)
                     [(impl/fresh 1) (impl/cached 0)]]
          expected2 [event2
                     (impl/fresh expected1)
                     [(impl/cached 1) (impl/fresh 2)]]]
      (async/onto-chan live-graph [event1 event2])
      (is (= [expected1 expected2]
             (<! (async/into [] output)))))))
