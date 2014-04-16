(ns jamesmacaulay.async-tools.constant-test
  (:require [jamesmacaulay.async-tools.constant :as c]
            #+clj [clojure.test :refer (is testing deftest)]
            #+cljs [cemerick.cljs.test :refer-macros (is testing deftest)]))

(deftest foo-does-something
  (is (= 4 (c/foo 1))))
