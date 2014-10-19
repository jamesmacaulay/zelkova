#+clj
(ns jamesmacaulay.async-tools.core-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [clojure.core.async :as async :refer [go chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is testing)]))
#+cljs
(ns jamesmacaulay.async-tools.core-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is testing)])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(deftest-async test-concat
  (go
    (is (= [1 2 3 4 5 6]
           (->> [[1 2] [3] [4 5] [6]]
                (map async/to-chan)
                (apply tools/concat)
                (async/into [])
                <!)))))

(deftest-async test-do-effects
  (go
    (let [box (atom 0)
          in (chan)
          out (chan)
          ret (tools/do-effects (partial async/put! out) in)]
      (is (= in ret))
      (>! in 1)
      (is (= 1 (<! out)))
      (>! in 2)
      (is (= 2 (<! out))))))

(deftest readport?-test
  (is (every? tools/readport? [(chan)
                               (async/map< inc (chan))]))
  (is (not-any? tools/readport? [1
                                 "another"
                                 {}])))



(deftest-async test-constant
  (go
    (let [c (tools/constant 1)]
      (is (= [1 1 1]
             [(<! c) (<! c) (<! c)])))))

(deftest-async test-cast-as-readport
  (go
    (let [channel (chan)
          not-a-channel {}]
      (is (identical? channel
                      (tools/cast-as-readport channel)))
      (is (identical? not-a-channel
                      (<! (tools/cast-as-readport not-a-channel)))))))
