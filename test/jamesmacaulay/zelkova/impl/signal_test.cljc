(ns jamesmacaulay.zelkova.impl.signal-test
  #?(:cljs (:require [jamesmacaulay.async-tools.core :as tools]
                     [jamesmacaulay.zelkova.signal :as z]
                     [jamesmacaulay.zelkova.impl.signal :as impl]
                     [cljs.core.async :as async :refer [chan to-chan <! >!]]
                     [cljs.core.async.impl.protocols :as async-impl]
                     [cemerick.cljs.test :refer-macros (deftest is are testing)])

     :clj (:require [jamesmacaulay.async-tools.core :as tools]
                    [jamesmacaulay.zelkova.signal :as z]
                    [jamesmacaulay.zelkova.impl.signal :as impl]
                    [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
                    [clojure.core.async.impl.protocols :as async-impl]
                    [jamesmacaulay.async-tools.test :refer (deftest-async)]
                    [clojure.test :refer (deftest is are testing)]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [jamesmacaulay.async-tools.test :refer (deftest-async)])
     :clj (:import [java.util.Date])))

(deftest-async test-msg-fn-takes-event-and-previous-value-and-sequence-of-source-messages-and-returns-a-message
  (go
    (let [in1 (z/input 0 :in1)
          in2 (z/input 0 :in2)
          sig (impl/make-signal {:init-fn   (constantly :test-init)
                                 :sources   [in1 in2]
                                 :msg-xform (map (fn [payload] (impl/fresh payload)))})
          live-graph (z/spawn sig)
          output (async/tap live-graph (chan))
          event1 (impl/->Event :in1 1 101)
          event2 (impl/->Event :in2 2 102)
          expected1 [event1
                     :test-init
                     [(impl/fresh 1) (impl/cached 0)]]
          expected2 [event2
                     expected1
                     [(impl/cached 1) (impl/fresh 2)]]]
      (async/onto-chan live-graph [event1 event2])
      (is (= [expected1 expected2]
             (<! (async/into [] output)))))))
