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

(defrecord ConstantReadPort [boxed-value]
  impl/ReadPort
  (take! [_ _] boxed-value))

(defn constant
  [x]
  (ConstantReadPort. (channels/box x)))

(defn cast-as-readport
  [x]
  (if (readport? x) x (constant x)))

(defn async-future*
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

(defn async-future<
  [ch]
  (async-future* (partial async/take! ch)))

(defn then<
  [f ch]
  (async-future* (fn [cb]
             (async/take! ch (comp cb f)))))

(defn all<
  [xs]
  (async-future< (async/map list
                      (map cast-as-readport
                           xs))))

(defn race<
  [xs]
  (let [non-readports (remove readport? xs)]
    (if (empty? non-readports)
      (async-future<
        (go (first (async/alts! xs))))
      (->> non-readports first constant))))
