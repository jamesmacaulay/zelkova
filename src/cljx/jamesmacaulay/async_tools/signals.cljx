#+clj
(ns jamesmacaulay.async-tools.signals
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels])
  (:refer-clojure :exclude [merge]))

#+cljs
(ns jamesmacaulay.async-tools.signals
  (:require [cljs.core.async :as async :refer [chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:refer-clojure :exclude [merge]))


(defprotocol Sproutable
  (sprout [this]))

(defrecord Sprout
  [value channel])

(defn node
  [value-atom ch]
  (let [msg-mult (async/mult ch)]
    (reify
      Sproutable
      (sprout
        [_]
        (let [out (async/tap msg-mult (chan))]
          (Sprout. @value-atom out))))))

(defrecord Message
  [topic body])

(defn change-message
  [val]
  (Message. :change val))

(defn no-change-message
  [val]
  (Message. :no-change val))

(defn change?
  [msg]
  (= :change (:topic msg)))

(def ^:private gen-id (partial swap! (atom 0) inc))

(def ^:private new-event (chan 1000))

(def ^:private event-notify (async/mult new-event))

(defn- input-node
  [id value-mult first-value]
  (let [in (async/tap value-mult (chan))
        event-ids (async/tap event-notify (chan))
        out (chan)
        value-atom (atom first-value)
        ret (node value-atom out)]
    (go-loop []
      (let [msg (if (= id (<! event-ids))
                  (change-message (reset! value-atom (<! in)))
                  (no-change-message @value-atom))]
        (>! out msg)
        (recur)))
    ret))

(defn- channel-input
  [ch v]
  (let [id (gen-id)
        p (async/mult ch)
        ids (async/map (constantly id)
                       [(async/tap p (chan))])]
    (async/pipe ids new-event)
    (input-node id p v)))

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

(defn lift
  [f & sigs]
  (let [sprouts (map sprout sigs)
        first-value (apply f (map :value sprouts))
        in (async/map vector (map :channel sprouts))
        value-atom (atom first-value)
        out (chan)
        ret (node value-atom out)]
    (go-loop []
      (let [input-msgs (<! in)
            msg (if (some change? input-msgs)
                  (change-message (->> input-msgs
                                       (map :body)
                                       (apply f)
                                       (reset! value-atom)))
                  (no-change-message @value-atom))]
        (>! out msg)
        (recur)))
    ret))

(defn foldp
  [f v source-node]
  (let [in (:channel (sprout source-node))
        out (chan)
        value-atom (atom v)
        ret (node value-atom out)]
    (go-loop []
      (let [input-msg (<! in)
            msg (if (change? input-msg)
                  (change-message (swap! value-atom f (:body input-msg)))
                  (no-change-message @value-atom))]
        (>! out msg)
        (recur)))
    ret))

(defn async
  [source-node]
  (let [s (sprout source-node)
        current-value (:value s)
        in (:channel s)
        out (chan 1 (comp (filter change?)
                          (map :body)))]
    (channel-input out current-value)))

(defn read-port
  [source-node]
  (let [in (:channel (sprout source-node))
        out (chan 1 (comp (filter change?)
                          (map :body)))]
    (async/pipe in out)
    (reify
      impl/ReadPort
      (take! [_ handler] (impl/take! out handler)))))

; helpers

(defn constant
  [value]
  (let [msg (no-change-message value)
        out (async/tap event-notify (chan 1 (map (constantly msg))))]
    (node (atom value) out)))

;(defn merge
;  [& sigs]
;  (let [sprouts (map sprout sigs)
;        value-atom (atom (:value (first sprouts)))
;        msg-channels (map :channel sprouts)
;        no-changes (-> msg-channels
;                       first
;                       (async/pipe (chan 1 (filter (complement change?)))))
;        changes-mult (-> msg-channels
;                         async/merge
;                         (async/pipe (chan 1 (filter change?)))
;                         async/mult)
;        changes-for-atom (async/tap changes-mult (chan))
;        out (async/merge [no-changes (async/tap changes-mult (chan))])]
;    (go-loop []
;      (reset! value-atom
;              (<! changes-for-atom))
;      (recur))
;    (node value-atom out)))

(defn combine
  [list-of-signals]
  (apply lift vector list-of-signals))
