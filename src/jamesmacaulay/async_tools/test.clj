(ns jamesmacaulay.async-tools.test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [cemerick.cljs.test]
            [clojure.test]))

(defmacro deftest-async
  [name & body]
  (let [deftest-sym (if (:ns &env)
                      'cemerick.cljs.test/deftest
                      'clojure.test/deftest)]
    `(~deftest-sym
       ~(with-meta name {:async true})
       (-> (do ~@body)
           tools/cast-as-readport
           cemerick.cljs.test/block-or-done))))
