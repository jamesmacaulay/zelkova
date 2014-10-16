#+clj
(ns jamesmacaulay.async-tools.signals
  (:refer-clojure :exclude [merge count])
  (:require [clojure.core :as core]
            [clojure.zip :as zip]
            [clojure.core.async :as async :refer [go go-loop chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools]
            [alandipert.kahn :as kahn]))

#+cljs
(ns jamesmacaulay.async-tools.signals
  (:refer-clojure :exclude [merge])
  (:require [cljs.core :as core]
            [clojure.zip :as zip]
            [cljs.core.async :as async :refer [chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools]
            [alandipert.kahn :as kahn])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn gen-topic
  []
  (keyword (gensym)))

(defrecord Event
  [topic value])

(defprotocol Message
  (fresh? [msg]))

(defrecord Fresh
  [value]
  Message
  (fresh? [_] true))

(defrecord Cached
  [value]
  Message
  (fresh? [_] false))

(defprotocol SignalProtocol
  (sources [s])
  (message-emitter [s]))

(defn signal?
  [x]
  (satisfies? SignalProtocol x))

(defn- subscribed-topics-message-emitter
  [topics]
  (let [topics (if (set? topics) topics (set topics))]
    (fn [prev event]
      (if (contains? topics (:topic event))
        (->Fresh (:value event))
        (->Cached (:value prev))))))

(defrecord Signal
  [init message-emitter subscribed-topics event-emitter external-event-source]
  SignalProtocol
  (sources [_]
    (into #{}
          (comp (filter identity)
                (mapcat (comp flatten vector first))
                (filter signal?))
          [message-emitter event-emitter]))
  (message-emitter [_]
    (cond
      message-emitter message-emitter
      subscribed-topics [:events (subscribed-topics-message-emitter subscribed-topics)])))

(defn- messages-from-events
  [topic]
  (fn [prev event]
    (if (= topic (:topic event))
      (->Fresh (:value event))
      (->Cached (:value prev)))))

(defn input
  ([init] (input init (gen-topic)))
  ([init topic]
   (map->Signal {:init init
                 :subscribed-topics #{topic}})))

(defn constant
  [x]
  (let [cached (->Cached x)]
    (map->Signal {:init x
                  :message-emitter [:events (constantly cached)]})))

(defn liftseq
  [f sources]
  (if (empty? sources)
    (constant (f))
    (let [emit-message (fn [prev messages]
                         (if (some fresh? messages)
                           (->Fresh (apply f (mapv :value messages)))
                           (->Cached (:value prev))))]
      (map->Signal {:init (->> sources
                               (mapv (comp ->Fresh :init))
                               (emit-message nil)
                               :value)
                    :message-emitter [sources emit-message]}))))

(defn lift
  [f & sources]
  (liftseq f sources))

(defn foldp
  [f init source]
  (map->Signal {:init init
                :message-emitter [source (fn [acc message]
                                           (if (fresh? message)
                                             (->Fresh (f (:value message) (:value acc)))
                                             (->Cached (:value acc))))]}))

(defn drop-repeats
  [sig]
  (map->Signal {:init (:init sig)
                :message-emitter [sig (fn [prev msg]
                                        (if (and (fresh? msg)
                                                 (not= (:value msg) (:value prev)))
                                          msg
                                          (->Cached (:value prev))))]}))


(defn reducep
  ([f source] (reducep f (f) source))
  ([f init source]
   (->> source
        (foldp (fn [val acc] (f acc val)) init)
        drop-repeats)))

(defn transducep
  ([xform f source] (reducep (xform f) (f) source))
  ([xform f init source]
   (reducep (xform f) init source)))

(defn async
  [source]
  (let [topic source]
    (map->Signal {:init (:init source)
                  :event-emitter [source (fn [message]
                                           (when (fresh? message)
                                             (:value message)))]
                  :subscribed-topics #{topic}})))

(defn mergeseq
  [sigs]
  (map->Signal {:init (:init (first sigs))
                :message-emitter [sigs (fn [prev messages]
                                         (or (first (filter fresh? messages))
                                             (->Cached (:value prev))))]}))

(defn merge
  [& sigs]
  (mergeseq sigs))

(defn combine
  [sigs]
  (liftseq vector sigs))

(defn sample-on
  [sampler-sig value-sig]
  (map->Signal {:init (:init value-sig)
                :message-emitter [[sampler-sig value-sig]
                                  (fn [prev [sampler-msg value-msg]]
                                    (if (fresh? sampler-msg)
                                      (->Fresh (:value value-msg))
                                      (->Cached (:value prev))))]}))

(defn count
  [sig]
  (foldp #(inc %2) 0 sig))

(defn count-if
  [pred sig]
  (foldp (fn [v c]
           (if (pred v) (inc c) c))
         0
         sig))

(defn keep-if
  [pred base sig]
  (map->Signal {:init (if (pred (:init sig))
                        (:init sig)
                        base)
                :message-emitter [sig (fn [prev msg]
                                        (if (and (fresh? msg)
                                                 (pred (:value msg)))
                                          (->Fresh (:value msg))
                                          (->Cached (:value prev))))]}))

(defn drop-if
  [pred base sig]
  (keep-if (complement pred) base sig))

(defn keep-when
  [switch-sig base value-sig]
  (->> value-sig
       (lift vector (sample-on value-sig switch-sig))
       (keep-if first [false base])
       (lift second)))

(defn drop-when
  [switch-sig base value-sig]
  (keep-when (lift not switch-sig) base value-sig))

; helpers:

(def fresh-values (comp (filter fresh?)
                        (map :value)))

; compiling graphs:

(defn node-graph-zipper
  [output-node]
  (zip/zipper (constantly true)
              (comp seq sources)
              nil
              output-node))

(defn skip-subtree
  [loc]
  (or (zip/right loc)
      (loop [p loc]
        (if (zip/up p)
          (or (zip/right (zip/up p))
              (recur (zip/up p)))
          [(zip/node p) :end]))))

(defn output-node->dependency-map
  [output-node]
  (loop [deps {}
         loc (node-graph-zipper output-node)]
    (cond
      (zip/end? loc)
      deps
      (contains? deps (zip/node loc))
      (recur deps
             (skip-subtree loc))
      :else
      (let [n (zip/node loc)]
        (recur (if (sequential? n)
                 deps
                 (assoc deps n (sources n)))
               (zip/next loc))))))

(defn topsort
  [output]
  (-> output
      output-node->dependency-map
      kahn/kahn-sort
      reverse))


(defrecord CompiledGraph
  [output-signal sorted-signals])

(defn compile-graph
  [output-signal]
  (let [sorted-signals (topsort output-signal)]
    (->CompiledGraph output-signal sorted-signals)))


; wiring up channels:

(defn- tap-template
  [tmpl mult-map]
  (if (sequential? tmpl)
    (->> tmpl
         (mapv #(async/tap (get mult-map %) (chan)))
         (async/map vector))
    (async/tap (get mult-map tmpl) (chan))))

(defn- spawn-message-loop!
  [init msg-fn c-in c-out]
  (go-loop [prev (->Fresh init)]
    (let [in-val (<! c-in)]
      (if (nil? in-val)
        (async/close! c-out)
        (let [out-val (msg-fn prev in-val)]
          (>! c-out out-val)
          (recur out-val))))))

(defn- build-message-mult
  [signal mult-map]
  (if-let [[tmpl msg-fn] (message-emitter signal)]
    (let [c-in (tap-template tmpl mult-map)
          c-out (chan)]
      (spawn-message-loop! (:init signal) msg-fn c-in c-out)
      (async/mult c-out))))

(defn build-message-mult-map
  [sorted-signals events-mult]
  (reduce (fn [mult-map signal]
            (assoc mult-map
              signal (build-message-mult signal mult-map)))
          {:events events-mult}
          sorted-signals))

(defprotocol LiveChannelGraphProtocol
  (output-mult [g])
  (init [g]))

(defrecord LiveChannelGraph
  [compiled-graph events-channel mult-map]
  LiveChannelGraphProtocol
  (output-mult [_] (get mult-map (:output-signal compiled-graph)))
  impl/Channel
  (close! [_] (impl/close! events-channel))
  (closed? [_] (impl/closed? events-channel))
  impl/WritePort
  (put! [_ val fn1] (impl/put! events-channel val fn1))
  async/Mult
  (tap* [g ch close?] (async/tap* (output-mult g) ch close?))
  (untap* [g ch] (async/untap* (output-mult g) ch))
  (untap-all* [g] (async/untap-all* (output-mult g))))

(defprotocol Spawnable
  (spawn [x]))

(extend-protocol Spawnable
  CompiledGraph
  (spawn [g]
    (let [events-channel (chan)
          events-mult (async/mult events-channel)
          mult-map (build-message-mult-map (:sorted-signals g) events-mult)]
      (->LiveChannelGraph g events-channel mult-map)))
  Signal
  (spawn [s] (spawn (compile-graph s))))
