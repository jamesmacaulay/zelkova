(ns jamesmacaulay.async-tools.test
  #+cljs (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [jamesmacaulay.async-tools.test.platform :as platform]
            #+clj [clojure.core.async :as async :refer [go]]
            #+clj [clojure.core.async.impl.protocols :as impl]
            #+clj [clojure.test :refer (is testing deftest)]
            #+clj [cemerick.cljs.test :refer (block-or-done)]
            #+cljs [cljs.core.async :as async]
            #+cljs [cljs.core.async.impl.protocols :as impl]
            #+cljs [cemerick.cljs.test :refer-macros (is testing deftest block-or-done)]))

(defn cast-as-readport
  [x]
  (if (instance? platform/ReadPort x)
    x
    (go x)))

(comment
  
#+clj
(defmacro deftest
  [name & body]
  (let [deftest-symbol (if (:ns &env)
                         'cemerick.cljs.test/deftest
                         'clojure.test/deftest)
        name-with-async-metadata (with-meta name {:async true})]
    `(~deftest-symbol
       ~name-with-async-metadata
       )))

)
