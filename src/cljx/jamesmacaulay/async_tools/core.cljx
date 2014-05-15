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

(deftype ConstantReadPort [boxed-value]
  impl/ReadPort
  (take! [_ _] boxed-value))

(defn constant
  [x]
  (ConstantReadPort. (channels/box x)))

(deftype AsyncFuture [state-atom]
  impl/ReadPort
  (take! [_ handler]
    (-> state-atom
        (swap! (fn [state]
                 (if (nil? (:boxed-value state))
                   (update-in state [:handlers] conj handler)
                   state)))
        :boxed-value)))

(defn- async-future-state-atom
  [f]
  (let [state-atom (atom {:boxed-value nil
                          :handlers []})
        resolve! (fn [value]
                   (let [state (swap! state-atom
                                      (fn [state]
                                        (when-not (nil? (:boxed-value state))
                                          (throw (ex-info "Can't call an async-future's `resolve!` function more than once." {})))
                                        (assoc state :boxed-value (channels/box value))))]
                     (doseq [handler (:handlers state)]
                       ((impl/commit handler) value))
                     (swap! state-atom dissoc :handlers)
                     nil))]
    (f resolve!)
    state-atom))

(defn async-future*
  [f]
  (AsyncFuture. (async-future-state-atom f)))

(defn async-future<
  [ch]
  (async-future* (partial async/take! ch)))

(defn readport?
  [x]
  (satisfies? impl/ReadPort x))

(defn cast-as-readport
  [x]
  (if (readport? x) x (constant x)))

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
    (if (seq non-readports)
      (-> non-readports first constant)
      (async-future< (go (-> xs async/alts! first))))))
