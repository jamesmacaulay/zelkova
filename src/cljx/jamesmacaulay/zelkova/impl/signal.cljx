#+clj
(ns jamesmacaulay.zelkova.impl.signal
  (:require [jamesmacaulay.async-tools.core :as tools]
            [clojure.zip :as zip]
            [alandipert.kahn :as kahn]
            [clojure.core.async :as async :refer [go go-loop <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]))

#+cljs
(ns jamesmacaulay.zelkova.impl.signal
  (:require [jamesmacaulay.async-tools.core :as tools]
            [clojure.zip :as zip]
            [alandipert.kahn :as kahn]
            [cljs.core.async :as async :refer [<! >!]]
            [cljs.core.async.impl.protocols :as async-impl])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defprotocol BoxedValueProtocol
  (value [boxed]))

(defprotocol EventProtocol
  "Events come in from \"the outside world\" and get transformed into Messages by input signal nodes"
  (topic [event]))

(defprotocol MessageProtocol
  "Messages are propagated through the signal graph, and can either be \"fresh\" or \"cached\"."
  (fresh? [msg] "returns `true` if the message represents a fresh value, `false` otherwise"))

; an external event
(defrecord Event
  [topic value]
  BoxedValueProtocol
  (value [_] value)
  EventProtocol
  (topic [_] topic))

; a message representing a "fresh" signal value
(defrecord Fresh
  [value]
  BoxedValueProtocol
  (value [_] value)
  MessageProtocol
  (fresh? [_] true))

; a message representing a "cached" signal value
(defrecord Cached
  [value]
  BoxedValueProtocol
  (value [_] value)
  MessageProtocol
  (fresh? [_] false))

(defprotocol SignalProtocol
  (signal-deps [s] "returns the set of \"parent\" signal which this signal depends on")
  (message-emitter [s]
    "returns a map with two entries:
     * `:sources` is a vector of signals from which to get messages
     * `:msg-fn` is a function which takes two arguments:
       - the previous message emitted by this signal
       - a sequence of new messages emitted by the corresponding signals listed in `:sources`"))

(defn signal?
  "returns `true` if the argument satisfies `SignalProtocol`, `false` otherwise"
  [x]
  (satisfies? SignalProtocol x))

(defrecord Signal
  [init message-emitter deps event-sources]
  SignalProtocol
  (signal-deps [_]
    (into #{}
          (filter signal?)
          (or deps (:sources message-emitter))))
  (message-emitter [_] message-emitter))

(def ^{:doc "A transducer which takes in batches of signal graph messages and pipes out fresh values."}
  fresh-values
  (comp cat
        (filter fresh?)
        (map value)))

; compiling graphs:

(defn- node-graph-zipper
  "Takes a signal and returns a zipper which can be used to traverse the signal graph."
  [output-node]
  (zip/zipper (constantly true)
              (comp seq signal-deps)
              nil
              output-node))

(defn- skip-subtree
  "Returns a new zipper location that skips the whole subtree at `loc`."
  [loc]
  (or (zip/right loc)
      (loop [p loc]
        (if (zip/up p)
          (or (zip/right (zip/up p))
              (recur (zip/up p)))
          [(zip/node p) :end]))))

(defn output-node->dependency-map
  "Takes a signal and returns a map of signals to sets of signal dependencies."
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
  "Takes a signal and returns a topologically sorted sequence of all signals in its graph."
  [output]
  (-> output
      output-node->dependency-map
      kahn/kahn-sort
      reverse))

(defrecord CompiledGraph
  [output-signal sorted-signals])

(defn compile-graph
  "Calculate the topological sort of the given signal and return a `CompiledGraph`."
  [output-signal]
  (let [sorted-signals (topsort output-signal)]
    (->CompiledGraph output-signal sorted-signals)))

; dealing with multiple outputs:

(defn- ensure-sequential
  "Wraps `x` in a vector, if necessary, returning an empty vector if `x` is `nil`."
  [x]
  (cond
    (sequential? x) x
    (nil? x) []
    :else [x]))

(defn- pad
  "Takes a collection of message batches, presumably from some corresponding collection
  of signals. Pads each batch, when necessary, with cached versions of the last message
  in the batch, such that the returned batches are all the same size."
  [msg-batches]
  (if (>= 1 (count msg-batches))
    msg-batches
    (let [max-count (reduce max (map count msg-batches))
          pad (fn [msgs]
                (->> [msgs (-> msgs last value ->Cached repeat)]
                     (into [] (comp cat (take max-count)))))]
      (map pad msg-batches))))

(defn- transpose
  "Takes a collection of message batches and returns a sequence of vectors of corresponding
  messages from each batch."
  [msg-batches]
  (apply map vector msg-batches))

(defn- wrap-msg-fn
  "Takes a signal's `msg-fn` and wraps it to provide various behaviours:
    * return values are turned into sequences with `ensure-sequential`
    * message batches from each signal are padded and transposed, and `msg-fn` is called
      once for each vector of messages in the resulting series, as if each were the result
      of a separate event.
    * when `msg-fn` returns `nil` or an empty sequence, the previous value is returned as a
      cached value."
  [msg-fn]
  (let [msg-fn (comp ensure-sequential msg-fn)]
    (fn [prev msg-batches]
      (let [input-series (-> msg-batches pad transpose)
            output-series (->> input-series
                               (reductions msg-fn prev)
                               (into [] (comp cat (drop 1))))]
        (if (empty? output-series)
          [(->Cached (value prev))]
          output-series)))))

; wiring up channels:

(defn- tap-signal
  [mult-map source]
  (let [mult (get mult-map source)]
    (async/tap mult (async/chan))))

(defn- tap-signals
  [mult-map sources]
  (->> sources
       (mapv (partial tap-signal mult-map))
       (async/map vector)))

(defn- spawn-message-loop!
  [init msg-fn c-in c-out]
  (let [wrapped-msg-fn (wrap-msg-fn msg-fn)]
    (go-loop [prev (->Fresh init)]
      (let [in-val (async/<! c-in)]
        (if (nil? in-val)
          (async/close! c-out)
          (let [out-val (wrapped-msg-fn prev in-val)]
            (>! c-out out-val)
            (recur (last out-val))))))))

(defn- build-message-mult
  [mult-map signal]
  (if-let [{:keys [sources msg-fn]} (message-emitter signal)]
    (let [c-in (tap-signals mult-map sources)
          c-out (async/chan)]
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
  (into {} (map :event-sources) sorted-signals))

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
  async-impl/Channel
  (close! [_] (async-impl/close! events-channel))
  (closed? [_] (async-impl/closed? events-channel))
  async-impl/WritePort
  (put! [_ val fn1] (async-impl/put! events-channel val fn1))
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
    (let [events-channel (async/chan 1 (map ensure-sequential))
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
