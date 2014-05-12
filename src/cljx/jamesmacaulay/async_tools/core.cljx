#+clj
(ns jamesmacaulay.async-tools.core
  (:require [clojure.core.async :as async :refer [go]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]))

#+cljs
(ns jamesmacaulay.async-tools.core
  (:require [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn readport?
  [x]
  (satisfies? impl/ReadPort x))

(defn constant
  [x]
  (reify
    impl/ReadPort
    (take! [_ _] (channels/box x))))

(defn cast-as-readport
  [x]
  (if (readport? x) x (constant x)))

(defn future*
  [f]
  (let [handlers (atom [])
        container (atom nil)
        resolve (fn [value]
                  (reset! container
                          (channels/box value))
                  (doseq [handler @handlers]
                    ((impl/commit handler) value)))]
    (f resolve)
    (reify
      impl/ReadPort
      (take! [_ handler]
        (let [boxed @container]
          (when (nil? boxed)
            (swap! handlers conj handler))
          boxed)))))

(defn future<!
  [ch]
  (future* (partial async/take! ch)))
