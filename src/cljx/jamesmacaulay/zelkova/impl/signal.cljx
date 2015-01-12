#+clj
(ns jamesmacaulay.zelkova.impl.signal
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.platform.time :as time]
            [clojure.zip :as zip]
            [clojure.set]
            [alandipert.kahn :as kahn]
            [clojure.core.async :as async :refer [go go-loop <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]))

#+cljs
(ns jamesmacaulay.zelkova.impl.signal
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.platform.time :as time]
            [clojure.zip :as zip]
            [clojure.set]
            [alandipert.kahn :as kahn]
            [cljs.core.async :as async :refer [<! >!]]
            [cljs.core.async.impl.protocols :as async-impl])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defprotocol BoxedValueProtocol
  (value [boxed]))

(defprotocol HasOriginEvent
  (origin-event [x]))

(defprotocol EventProtocol
  "Events come in from \"the outside world\" and get transformed into Messages by input signal nodes"
  (topic [event])
  (timestamp [event])
  (record-timestamp [event timestamp]))

(defprotocol MessageProtocol
  "Messages are propagated through the signal graph, and can either be \"fresh\" or \"cached\"."
  (fresh? [msg] "returns `true` if the message represents a fresh value, `false` otherwise")
  (record-origin-event [msg e]))

; an external event
(defrecord Event
  [topic value timestamp]
  BoxedValueProtocol
  (value [_] value)
  HasOriginEvent
  (origin-event [e] e)
  EventProtocol
  (topic [_] topic)
  (timestamp [_] timestamp)
  (record-timestamp [e t] (assoc e :timestamp t)))

(defn make-event
  [topic value]
  (->Event topic value nil))

; a message representing a "fresh" signal value
(defrecord Fresh
  [value origin-event]
  BoxedValueProtocol
  (value [_] value)
  HasOriginEvent
  (origin-event [_] origin-event)
  MessageProtocol
  (fresh? [_] true)
  (record-origin-event [m e] (assoc m :origin-event e)))

; a message representing a "cached" signal value
(defrecord Cached
  [value origin-event]
  BoxedValueProtocol
  (value [_] value)
  HasOriginEvent
  (origin-event [_] origin-event)
  MessageProtocol
  (fresh? [_] false)
  (record-origin-event [m e] (assoc m :origin-event e)))

(defn fresh
  ([value] (->Fresh value nil))
  ([value event] (->Fresh value event)))

(defn cached
  ([value] (->Cached value nil))
  ([value event] (->Cached value event)))

(defprotocol SignalProtocol
  (signal-deps [s] "returns the set of \"parent\" signal which this signal depends on"))

(defn signal?
  "returns `true` if the argument satisfies `SignalProtocol`, `false` otherwise"
  [x]
  (satisfies? SignalProtocol x))

(defrecord Signal
  [init sources relayed-event-topic msg-fn deps event-sources]
  SignalProtocol
  (signal-deps [_]
    (into #{}
          (filter signal?)
          (or deps sources))))

(defn- setup-event-relay
  "Takes a topic, and returns an input signal which relays matching events as messages to its children"
  [opts]
  (if-let [relayed-topic (:relayed-event-topic opts)]
    (assoc opts
      :sources [:events]
      :msg-fn (fn [prev [event]]
                (when (= relayed-topic (topic event))
                  (fresh (value event)))))
    opts))

(defn make-signal
  "Takes a map of opts and returns a signal."
  [opts]
  (-> opts
      (setup-event-relay)
      (map->Signal)))

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

(defn signal->dependency-maps
  "Takes a signal and returns a map of two maps:
    :parents-map is a map of signals to their parents,
    :kids-map is a map of signals to their children."
  [signal]
  (loop [parents-map {}
         kids-map {signal #{}}
         loc (node-graph-zipper signal)]
    (cond
      (zip/end? loc)
      {:parents-map parents-map
       :kids-map kids-map}

      (contains? parents-map (zip/node loc))
      (recur parents-map kids-map (skip-subtree loc))

      :else
      (let [this-sig (zip/node loc)
            parents (signal-deps this-sig)
            next-sig (zip/next loc)]
        (recur
          (assoc parents-map this-sig parents)
          (merge-with clojure.set/union
                      kids-map
                      (zipmap parents (repeat #{this-sig})))
          next-sig)))))

(defn output-node->dependency-map
  "Takes a signal and returns a map of signals to sets of signal dependencies."
  [output-node]
  (-> output-node signal->dependency-maps :parents-map))

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
                (->> [msgs (-> msgs last value cached repeat)]
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
      (let [event (-> msg-batches ffirst origin-event)
            input-series (-> msg-batches pad transpose)
            output-series (->> input-series
                               (reductions msg-fn prev)
                               (into [] (comp (drop 1)
                                              cat
                                              (map #(record-origin-event % event)))))]
        (if (empty? output-series)
          [(cached (value prev) event)]
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
    (go-loop [prev (fresh init)]
      (let [in-val (async/<! c-in)]
        (if (nil? in-val)
          (async/close! c-out)
          (let [out-val (wrapped-msg-fn prev in-val)]
            (>! c-out out-val)
            (recur (last out-val))))))))

(defn- build-message-mult
  [mult-map {:keys [init sources msg-fn]}]
  (let [c-in (tap-signals mult-map sources)
        c-out (async/chan)]
    (spawn-message-loop! init msg-fn c-in c-out)
    (async/mult c-out)))

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

(def ^:private events-xform
  (map (comp (partial map
                      (fn [event]
                        (record-timestamp event (time/now))))
             ensure-sequential)))

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
    (let [events-channel (async/chan 1 events-xform)
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
