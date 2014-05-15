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

(defn- async-future-state-atom
  [f]
  (let [state-atom (atom {:boxed-value nil
                          :handlers []})]
    (f (fn [value]
         (let [state (swap! state-atom assoc :boxed-value (channels/box value))]
           (doseq [handler (:handlers state)]
             ((impl/commit handler) value))
           (swap! state-atom assoc :handlers nil))))
    state-atom))

(defrecord AsyncFuture [state-atom]
  impl/ReadPort
  (take! [_ handler]
    (-> state-atom
        (swap! (fn [state]
                 (if (nil? (:boxed-value state))
                   (update-in state [:handlers] conj handler)
                   state)))
        :boxed-value)))

(defn async-future*
  [f]
  (AsyncFuture. (async-future-state-atom f)))

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
