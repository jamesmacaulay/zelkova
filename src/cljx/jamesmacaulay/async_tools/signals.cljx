#+clj
(ns jamesmacaulay.async-tools.signals
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools]))

#+cljs
(ns jamesmacaulay.async-tools.signals
  (:refer-clojure :exclude [merge])
  (:require [cljs.core.async :as async :refer [chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defprotocol Message
  (change? [msg])
  (body [msg]))

(deftype Change
  [body]
  Message
  (change? [_] true)
  (body [_] body))

(deftype NoChange
  [body]
  Message
  (change? [_] false)
  (body [_] body))

(defn change
  [val]
  (Change. val))

(defn no-change
  [val]
  (NoChange. val))

(def ^:private gen-id (partial swap! (atom 0) inc))

(def ^:private new-event (chan 1000))

(def ^:private event-notify (async/mult new-event))

(defn port
  [m]
  (async/tap m (chan)))

(defprotocol Sproutable
  (sprout [this]))

(defrecord Sprout
  [value channel])

(deftype Node
  [value mult]
  Sproutable
  (sprout
    [_]
    (Sprout. value (port mult))))

(defn input
  [id init]
  (let [c-in (port event-notify)
        c-out (chan)]
    (go-loop [prev init]
      (let [[eid v] (<! c-in)
            msg (if (= id eid)
                  (change v)
                  (no-change prev))]
        (>! c-out msg)
        (recur (body msg))))
    (Node. init (async/mult c-out))))

(defn lift-msgs
  [f & sigs]
  (let [sprouts (mapv sprout sigs)
        init-msgs (mapv (comp change :value) sprouts)
        channels (mapv :channel sprouts)
        c-in (async/map vector channels)
        init-msg (apply f init-msgs)
        v (body init-msg)
        c-out (chan)]
    (go-loop [prev v]
      (let [input-msgs (<! c-in)
            msg (if (some change? input-msgs)
                  (let [result (apply f input-msgs)]
                    (if (and (satisfies? Message result)
                             (change? result))
                      result
                      (no-change prev)))
                  (no-change prev))]
        (>! c-out msg)
        (recur (body msg))))
    (Node. v (async/mult c-out))))

(defn lift
  [f & sigs]
  (apply lift-msgs
         (fn [& args]
           (change (apply f (map body args))))
         sigs))

;(defn lift
;  [f & sigs]
;  (let [sprouts (mapv sprout sigs)
;        values (mapv :value sprouts)
;        channels (mapv :channel sprouts)
;        c-in (async/map vector channels)
;        v (apply f values)
;        c-out (chan)]
;    (go-loop [prev v]
;      (let [input-msgs (<! c-in)
;            msg (if (some change? input-msgs)
;                  (change (->> input-msgs (map body) (apply f)))
;                  (no-change prev))]
;        (>! c-out msg)
;        (recur (body msg))))
;    (Node. v (async/mult c-out))))

(defn foldp-msgs
  [f v sig]
  (let [c-in (:channel (sprout sig))
        c-out (chan)]
    (go-loop [acc (change v)]
      (let [input-msg (<! c-in)
            msg (if (change? input-msg)
                  (let [result (f input-msg acc)]
                    (if (and (satisfies? Message result)
                             (change? result))
                      result
                      (no-change acc)))
                  (no-change acc))]
        (>! c-out msg)
        (recur msg)))
    (Node. v (async/mult c-out))))

(defn foldp
  [f v sig]
  (foldp-msgs (fn [msg acc-msg]
                (change (f (body msg) (body acc-msg))))
              v
              sig))

;(defn foldp
;  [f v sig]
;  (let [c-in (:channel (sprout sig))
;        c-out (chan)]
;    (go-loop [acc v]
;      (let [input-msg (<! c-in)
;            msg (if (change? input-msg)
;                  (change (f (body input-msg) acc))
;                  (no-change acc))]
;        (>! c-out msg)
;        (recur (body msg))))
;    (Node. v (async/mult c-out))))


(defn- channel-input
  [ch v]
  (let [id (gen-id)
        events (async/map (partial vector id) [ch])]
    (async/pipe events new-event)
    (input id v)))

(defn read-port
  [sig]
  (let [c-in (:channel (sprout sig))
        c-out (chan 1 (comp (filter change?)
                            (map body)))]
    (async/pipe c-in c-out)
    c-out))

(defn write-port
  [init]
  (let [ch (chan 1)
        sig (channel-input ch init)]
    (reify
      impl/Channel
      (close! [_] (impl/close! ch))
      (closed? [_] (impl/closed? ch))
      impl/WritePort
      (put! [_ val fn1] (impl/put! ch val fn1))
      Sproutable
      (sprout [_] (sprout sig)))))

(defn async
  [sig]
  (let [s (sprout sig)
        c-in (:channel s)
        c-out (chan 1 (comp (filter change?)
                            (map body)))]
    (channel-input (async/pipe c-in c-out)
                   (:value s))))

(defn constant
  [value]
  (let [msg (no-change value)
        out (async/tap event-notify (chan 1 (map (constantly msg))))]
    (Node. value (async/mult out))))

;(defn merge
;  [& sigs]
;  (let [sprouts (mapv sprout sigs)
;        first-value (:value (first sprouts))
;        msg-vectors (->> sprouts
;                         (mapv :channel)
;                         (async/map vector))
;        c-out (chan)]
;    (go-loop [prev first-value]
;      (let [msgs (<! msg-vectors)
;            first-change (first (filter change? msgs))
;            msg (or first-change (no-change prev))]
;        (>! c-out msg)
;        (recur (body msg))))
;    (Node. first-value (async/mult c-out))))

(defn merge
  [& sigs]
  (apply lift-msgs
         (fn [& msgs]
           (->> msgs (filter change?) first))
         sigs))

;(defn sample-on
;  [sampler-sig value-sig]
;  (let [sample-channel (:channel (sprout sampler-sig))
;        {init :value
;         value-channel :channel} (sprout value-sig)
;        c-in (async/map vector [sample-channel value-channel])
;        c-out (chan)]
;    (go-loop []
;      (let [[sample-msg value-msg] (<! c-in)
;            value (body value-msg)
;            msg (if (change? sample-msg)
;                  (change value)
;                  (no-change value))]
;        (>! c-out msg)
;        (recur)))
;    (Node. init (async/mult c-out))))

;(defn sample-on
;  [sampler-sig value-sig]
;  (lift-msgs (fn [sample-msg value-msg]
;               (when (change? sample-msg)
;                 (change (body value-msg))))
;             sampler-sig
;             value-sig))