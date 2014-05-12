(ns jamesmacaulay.async-tools.test-test
  #+cljs (:require-macros [cljs.core.async.macros :refer [go]]
                          [cemerick.cljs.test :refer (block-or-done is testing deftest)])
  (:require [jamesmacaulay.async-tools.test :as t]
            [jamesmacaulay.async-tools.test.platform :as platform]
            [#+clj clojure.core.async
             #+cljs cljs.core.async :as async :refer [#+clj go to-chan <!]]
            [#+clj clojure.core.async.impl.protocols
             #+cljs cljs.core.async.impl.protocols :as impl]
            #+clj [clojure.test :refer (is testing deftest)]
            #+clj [cemerick.cljs.test :refer (block-or-done)]))

(deftest ^:async an-async-test
  (block-or-done
    (go
      (is (= 1 (<! (to-chan [1])))))))
