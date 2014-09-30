#+clj
(ns jamesmacaulay.async-tools.test
  (:require [clojure.core.async :as async :refer [go go-loop >! <! chan]]
            [jamesmacaulay.async-tools.core :as tools]
            [cemerick.cljs.test]
            [clojure.test]))

#+cljs
(ns jamesmacaulay.async-tools.test
  (:require [cljs.core.async :as async :refer [>! <! chan]]
            [jamesmacaulay.async-tools.core :as tools]
            [cemerick.cljs.test])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


#+clj
(defmacro deftest-async
  [name & body]
  (let [deftest-sym (if (:ns &env)
                      'cemerick.cljs.test/deftest
                      'clojure.test/deftest)]
    `(~deftest-sym
       ~(with-meta name {:async true})
       (-> (do ~@body)
           (as-> ch#
                 (if (tools/readport? ch#)
                   (go
                     (let [t# (async/timeout 2000)]
                       (async/alt!
                         ch# ([v#] v#)
                         t# ([_#] (throw (ex-info (str "timeout in " ~name) {}))))))
                   ch#))
           tools/cast-as-readport
           cemerick.cljs.test/block-or-done))))