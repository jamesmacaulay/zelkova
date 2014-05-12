(ns jamesmacaulay.async-tools.test.platform
  (:require #+clj [clojure.core.async :as async]
            #+clj [clojure.core.async.impl.protocols :as impl]
            #+clj [clojure.test :refer (is testing deftest)]
            #+clj [cemerick.cljs.test :refer (block-or-done)]
            #+cljs [cljs.core.async :as async]
            #+cljs [cljs.core.async.impl.protocols :as impl]
            #+cljs [cemerick.cljs.test :refer-macros (is testing deftest block-or-done)]))

(def ReadPort impl/ReadPort)
