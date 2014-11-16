#+clj
(ns jamesmacaulay.zelkova.signal
  (:refer-clojure :exclude [map merge count])
  (:require [clojure.core :as core]
            [clojure.zip :as zip]
            [clojure.core.async :as async :refer [go go-loop chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools]
            [alandipert.kahn :as kahn]
            [clojure.pprint :refer [pprint]]))

#+cljs
(ns jamesmacaulay.zelkova.signal
  (:refer-clojure :exclude [map merge count])
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
  (signal-deps [s])
  (message-emitter [s]))

(defn signal?
  [x]
  (satisfies? SignalProtocol x))

(defn- event-relay
  [topics]
  (let [topics (if (set? topics) topics (set topics))]
    {:sources [:events]
     :relayed-event-topics topics
     :msg-fn (fn [prev [event]]
               (if (contains? topics (:topic event))
                 (->Fresh (:value event))
                 (->Cached (:value prev))))}))

(defrecord Signal
  [init message-emitter deps event-sources]
  SignalProtocol
  (signal-deps [_]
    (into #{}
          (filter signal?)
          (or deps (:sources message-emitter))))
  (message-emitter [_] message-emitter))

(defmulti value-source->events-fn
  (fn [src topic]
    (cond
      (satisfies? async/Mult src) :mult
      (satisfies? impl/ReadPort src) :readport
      (ifn? src) :ifn)))

(defmethod value-source->events-fn :ifn
  [src-fn topic]
  (fn [graph opts]
    (let [ch (src-fn graph opts)]
      (async/pipe ch (chan 1 (core/map (partial ->Event topic)))))))

(defmethod value-source->events-fn :mult
  [src-mult topic]
  (value-source->events-fn (fn [_ _] (async/tap src-mult (chan)))
                           topic))

(defmethod value-source->events-fn :readport
  [src-chan topic]
  (value-source->events-fn (async/mult src-chan)
                           topic))

(defn input
  ([init] (input init (gen-topic)))
  ([init topic]
   (map->Signal {:init init
                 :message-emitter (event-relay #{topic})}))
  ([init topic value-channel-or-fn-or-mult]
   (let [event-channel-fn (value-source->events-fn value-channel-or-fn-or-mult
                                                   topic)]
     (map->Signal {:init init
                   :message-emitter (event-relay #{topic})
                   :event-sources {topic event-channel-fn}}))))

(defn constant
  [x]
  (let [cached (->Cached x)]
    (map->Signal {:init x
                  :message-emitter {:sources [:events]
                                    :msg-fn (constantly cached)}})))

(defn pipeline
  [xform base sig]
  (let [reducer ((comp (core/map :value)
                       xform
                       (core/map ->Fresh))
                 conj)
        msg-fn (fn [prev [msg :as msg-in-seq]]
                 (if (fresh? msg)
                   (reduce reducer [] msg-in-seq)
                   (->Cached (:value prev))))
        pipelined-init-msg (->> (:init sig) ->Fresh vector (msg-fn nil) last)
        init-val (if (nil? pipelined-init-msg)
                   base
                   (:value pipelined-init-msg))]
    (map->Signal {:init init-val
                  :message-emitter {:sources [sig]
                                    :msg-fn msg-fn}})))

(defn mapseq
  [f sources]
  (if (empty? sources)
    (constant (f))
    (let [sources (vec sources)
          emit-message (fn [prev messages]
                         (if (some fresh? messages)
                           (->Fresh (apply f (mapv :value messages)))
                           (->Cached (:value prev))))]
      (map->Signal {:init (->> sources
                               (mapv (comp ->Fresh :init))
                               (emit-message nil)
                               :value)
                    :message-emitter {:sources sources
                                      :msg-fn emit-message}}))))

(defn map
  [f & sources]
  (mapseq f sources))

(defn template
  [signal-map]
  (let [ks (keys signal-map)]
    (mapseq (fn [& values]
               (zipmap ks values))
             (vals signal-map))))

(defn foldp
  [f init source]
  (map->Signal {:init init
                :message-emitter {:sources [source]
                                  :msg-fn (fn [acc [message]]
                                            (if (fresh? message)
                                              (->Fresh (f (:value message) (:value acc)))
                                              (->Cached (:value acc))))}}))

(defn drop-repeats
  [sig]
  (map->Signal {:init (:init sig)
                :message-emitter {:sources [sig]
                                  :msg-fn (fn [prev [msg]]
                                            (if (and (fresh? msg)
                                                     (not= (:value msg) (:value prev)))
                                              msg
                                              (->Cached (:value prev))))}}))


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
  (let [topic source
        msgs->events (comp cat
                           (filter fresh?)
                           (core/map (fn [msg]
                                       (->Event topic (:value msg)))))
        events-channel-fn (fn [live-graph _]
                            (async/tap (get (:mult-map live-graph) source)
                                       (chan 1 msgs->events)))]
    (map->Signal {:init (:init source)
                  :deps [source]
                  :event-sources {topic events-channel-fn}
                  :message-emitter (event-relay #{topic})})))

(defn mergeseq
  [sigs]
  (map->Signal {:init (:init (first sigs))
                :message-emitter {:sources sigs
                                  :msg-fn (fn [prev messages]
                                            (or (first (filter fresh? messages))
                                                (->Cached (:value prev))))}}))

(defn merge
  [& sigs]
  (mergeseq sigs))

(defn combine
  [sigs]
  (mapseq vector sigs))

(defn sample-on
  [sampler-sig value-sig]
  (map->Signal {:init (:init value-sig)
                :message-emitter {:sources [sampler-sig value-sig]
                                  :msg-fn (fn [prev [sampler-msg value-msg]]
                                            (if (fresh? sampler-msg)
                                              (->Fresh (:value value-msg))
                                              (->Cached (:value prev))))}}))

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
                :message-emitter {:sources [sig]
                                  :msg-fn (fn [prev [msg]]
                                            (if (and (fresh? msg)
                                                     (pred (:value msg)))
                                              (->Fresh (:value msg))
                                              (->Cached (:value prev))))}}))

(defn drop-if
  [pred base sig]
  (keep-if (complement pred) base sig))

(defn keep-when
  [switch-sig base value-sig]
  (->> value-sig
       (map vector (sample-on value-sig switch-sig))
       (keep-if first [false base])
       (map second)))

(defn drop-when
  [switch-sig base value-sig]
  (keep-when (map not switch-sig) base value-sig))

(defn log
  [sig]
  (map (fn [x] (pr x) x) sig))

; helpers:

(def fresh-values (comp cat
                        (filter fresh?)
                        (core/map :value)))

; compiling graphs:

(defn node-graph-zipper
  [output-node]
  (zip/zipper (constantly true)
              (comp seq signal-deps)
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
                 (assoc deps n (signal-deps n)))
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

; dealing with multiple outputs:

(defn- ensure-sequential
  [x-or-xs]
  (if (sequential? x-or-xs) x-or-xs [x-or-xs]))

(defn pad
  [msg-lists]
  (if (>= 1 (core/count msg-lists))
    msg-lists
    (let [max-count (reduce max (core/map core/count msg-lists))
          pad (fn [msgs]
                (->> [msgs (-> msgs last :value ->Cached repeat)]
                     (into [] (comp cat (take max-count)))))]
      (core/map pad msg-lists))))

(defn transpose
  [msg-lists]
  (apply core/map vector msg-lists))

(defn wrap-msg-fn
  [msg-fn]
  (let [msg-fn (comp ensure-sequential msg-fn)]
    (fn [prev msg-payloads]
      (let [input-series (-> msg-payloads pad transpose)
            output-series (->> input-series
                               (reductions msg-fn prev)
                               (into [] (comp cat (drop 1))))]
        (if (empty? output-series)
          [(->Cached (:value prev))]
          output-series)))))

; wiring up channels:

(defn- tap-signal
  [mult-map source]
  (let [mult (get mult-map source)]
    (async/tap mult (chan))))

(defn- tap-signals
  [mult-map sources]
  (->> sources
       (mapv (partial tap-signal mult-map))
       (async/map vector)))

(defn- spawn-message-loop!
  [init msg-fn c-in c-out]
  (let [wrapped-msg-fn (wrap-msg-fn msg-fn)]
    (go-loop [prev (->Fresh init)]
      (let [in-val (<! c-in)]
        (if (nil? in-val)
          (async/close! c-out)
          (let [out-val (wrapped-msg-fn prev in-val)]
            (>! c-out out-val)
            (recur (last out-val))))))))

(defn- build-message-mult
  [mult-map signal]
  (if-let [{:keys [sources msg-fn]} (message-emitter signal)]
    (let [c-in (tap-signals mult-map sources)
          c-out (chan)]
      (spawn-message-loop! (:init signal) msg-fn c-in c-out)
      (async/mult c-out))))

(defn build-message-mult-map
  [sorted-signals events-mult]
  (reduce (fn [mult-map signal]
            (assoc mult-map
              signal (build-message-mult mult-map signal)))
          {:events events-mult}
          sorted-signals))

(defn gather-event-sources
  [sorted-signals]
  (into {} (core/map :event-sources) sorted-signals))

(defprotocol LiveChannelGraphProtocol
  (output-mult [g])
  (connect-to-world [g opts]))

(defrecord LiveChannelGraph
  [compiled-graph events-channel mult-map]
  LiveChannelGraphProtocol
  (output-mult [_] (get mult-map (:output-signal compiled-graph)))
  (connect-to-world [g opts]
    (let [world (gather-event-sources (:sorted-signals compiled-graph))]
      (doseq [channel-fn (vals world)]
        (async/pipe (channel-fn g opts)
                    events-channel)))
    g)
  impl/Channel
  (close! [_] (impl/close! events-channel))
  (closed? [_] (impl/closed? events-channel))
  impl/WritePort
  (put! [_ val fn1] (impl/put! events-channel val fn1))
  async/Mult
  (tap* [g ch close?] (async/tap* (output-mult g) ch close?))
  (untap* [g ch] (async/untap* (output-mult g) ch))
  (untap-all* [g] (async/untap-all* (output-mult g))))

(defprotocol SignalLike
  (init [x])
  (spawn* [x opts])
  (pipe-to-atom* [x a ks]))

(extend-protocol SignalLike
  LiveChannelGraph
  (init [g] (init (:compiled-graph g)))
  (spawn* [g opts] (spawn* (:compiled-graph g) opts))
  (pipe-to-atom* [g atm ks]
    (tools/do-effects (if (seq ks)
                        (partial swap! atm assoc-in ks)
                        (partial reset! atm))
                      (async/tap g (async/chan 1 fresh-values)))
    atm)
  CompiledGraph
  (init [g] (:init (:output-signal g)))
  (spawn* [g opts]
    (let [events-channel (chan 1 (core/map ensure-sequential))
          events-mult (async/mult events-channel)
          mult-map (build-message-mult-map (:sorted-signals g) events-mult)]
      (-> g
          (->LiveChannelGraph events-channel mult-map)
          (connect-to-world opts))))
  (pipe-to-atom* [g atm ks]
    (pipe-to-atom* (spawn* g nil) atm ks))
  Signal
  (init [s] (:init s))
  (spawn* [s opts] (spawn* (compile-graph s) opts))
  (pipe-to-atom* [s atm ks]
    (pipe-to-atom* (spawn* s nil) atm ks)))

(defn spawn
  ([g] (spawn* g nil))
  ([g opts] (spawn* g opts)))

(defn pipe-to-atom
  ([x]
   (let [live-graph (spawn x)]
     (pipe-to-atom live-graph
                   (atom (init live-graph)
                         :meta {::source live-graph}))))
  ([x atm] (pipe-to-atom* x atm nil))
  ([x atm ks] (pipe-to-atom* x atm ks)))
