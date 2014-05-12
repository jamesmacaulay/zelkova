#+clj
(ns jamesmacaulay.async-tools.core-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [clojure.core.async :as async :refer [go chan to-chan <!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.test :refer (is testing deftest)]
            [cemerick.cljs.test :refer (block-or-done)]))
#+cljs
(ns jamesmacaulay.async-tools.core-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [cljs.core.async :as async :refer [chan to-chan <!]]
            [cljs.core.async.impl.protocols :as impl]
            [cemerick.cljs.test :refer-macros (block-or-done is testing deftest)])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(deftest readport?-test
  (is (every? tools/readport? [(chan)
                               (async/map< inc (chan))]))
  (is (not-any? tools/readport? [1
                                 "another"
                                 {}])))

(deftest ^:async constant-test
  (block-or-done
    (go
      (let [c (tools/constant 1)]
        (is (= [1 1 1]
               [(<! c) (<! c) (<! c)]))))))

(deftest ^:async cast-as-readport-test
  (block-or-done
    (go
      (let [channel (chan)
            not-a-channel {}]
        (is (identical? channel
                        (tools/cast-as-readport channel)))
        (is (identical? not-a-channel
                        (<! (tools/cast-as-readport not-a-channel))))))))

(deftest ^:async future*-test
  (block-or-done
    (go
      (let [fut (tools/future* (fn [resolve]
                                 (go (resolve (<! (to-chan [1]))))))]
        (is (= [1 1 1]
               [(<! fut) (<! fut) (<! fut)]))))))

(deftest ^:async future<!-test
  (block-or-done
    (go
      (let [fut (tools/future<! (to-chan [1]))]
        (is (= [1 1 1]
               [(<! fut) (<! fut) (<! fut)]))))))
