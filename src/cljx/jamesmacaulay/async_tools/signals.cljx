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

(defrecord Message
  [topic body])

(defn change
  [val]
  (Message. :change val))

(defn no-change
  [val]
  (Message. :no-change val))

(defn change?
  [msg]
  (= :change (:topic msg)))

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
  [id mc-in v]
  (let [c-in (port mc-in)
        c-out (chan)
        eid (port event-notify)]
    (go-loop [prev v]
      (let [msg (if (= id (<! eid))
                  (change (<! c-in))
                  (no-change prev))]
        (>! c-out msg)
        (recur (:body msg))))
    (Node. v (async/mult c-out))))

(defn lift
  [f & sigs]
  (let [sprouts (mapv sprout sigs)
        values (mapv :value sprouts)
        channels (mapv :channel sprouts)
        c-in (async/map vector channels)
        v (apply f values)
        c-out (chan)]
    (go-loop [prev v]
      (let [input-msgs (<! c-in)
            msg (if (some change? input-msgs)
                  (change (->> input-msgs (map :body) (apply f)))
                  (no-change prev))]
        (>! c-out msg)
        (recur (:body msg))))
    (Node. v (async/mult c-out))))

(defn foldp
  [f v sig]
  (let [c-in (:channel (sprout sig))
        c-out (chan)]
    (go-loop [acc v]
      (let [input-msg (<! c-in)
            msg (if (change? input-msg)
                  (change (f (:body input-msg) acc))
                  (no-change acc))]
        (>! c-out msg)
        (recur (:body msg))))
    (Node. v (async/mult c-out))))


(defn- channel-input
  [ch v]
  (let [id (gen-id)
        mc-out (async/mult ch)
        ids (async/map (constantly id)
                       [(port mc-out)])]
    (async/pipe ids new-event)
    (input id mc-out v)))

(defn read-port
  [sig]
  (let [c-in (:channel (sprout sig))
        c-out (chan 1 (comp (filter change?)
                            (map :body)))]
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
                            (map :body)))]
    (channel-input (async/pipe c-in c-out)
                   (:value s))))

(defn constant
  [value]
  (let [msg (no-change value)
        out (async/tap event-notify (chan 1 (map (constantly msg))))]
    (Node. value (async/mult out))))

(defn merge
  [& sigs]
  (let [sprouts (mapv sprout sigs)
        first-value (:value (first sprouts))
        msg-vectors (->> sprouts
                         (mapv :channel)
                         (async/map vector))
        c-out (chan)]
    (go-loop [prev first-value]
      (let [msgs (<! msg-vectors)
            first-change (first (filter change? msgs))
            msg (or first-change (no-change prev))]
        (>! c-out msg)
        (recur (:body msg))))
    (Node. first-value (async/mult c-out))))
