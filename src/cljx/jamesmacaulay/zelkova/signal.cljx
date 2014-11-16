#+clj
(ns jamesmacaulay.zelkova.signal
  (:refer-clojure :exclude [map merge count])
  (:require [clojure.core :as core]
            [clojure.zip :as zip]
            [clojure.core.async :as async :refer [go go-loop chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]
            [jamesmacaulay.async-tools.core :as tools]
            [alandipert.kahn :as kahn]))

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

; Events come in from "the outside world" and get transformed into Messages by input signal nodes
(defrecord Event
  [topic value])

(defprotocol Message
  "Messages are propagated through the signal graph, and can either be \"fresh\" or \"cached\"."
  (fresh? [msg] "returns `true` if the message represents a fresh value, `false` otherwise"))

; a box for a "fresh" value
(defrecord Fresh
  [value]
  Message
  (fresh? [_] true))

; a box for a "cached" value
(defrecord Cached
  [value]
  Message
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

(defn- event-relay
  "takes a sequence of topics, and returns an input signal which relays matching events as messages to its children"
  [topics]
  (let [topics (if (set? topics) topics (set topics))]
    {:sources [:events]
     :relayed-event-topics topics
     :msg-fn (fn [prev [event]]
               (when (contains? topics (:topic event))
                 (->Fresh (:value event))))}))

(defrecord Signal
  [init message-emitter deps event-sources]
  SignalProtocol
  (signal-deps [_]
    (into #{}
          (filter signal?)
          (or deps (:sources message-emitter))))
  (message-emitter [_] message-emitter))

(defmulti ^:private value-source->events-fn
  "Takes some asynchronous `source` of values, plus an event `topic`, and returns
  an event-source function. `source` may be one of the following:
    * a function taking a live graph and an options map, and returns a channel of values
    * a channel of values
    * a mult of some such value channel
  The returned event-source function has the same signature as the functions that can
  be supplied for the `source` argument, but the values are wrapped as Events."
  (fn [source topic]
    (cond
      (satisfies? async/Mult source) :mult
      (satisfies? impl/ReadPort source) :readport
      (ifn? source) :ifn)))

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
  "Returns an input signal with initial value `init`. The signal propagates values
  from events which match some `topic`. An asynchronous `value-source` may be provided,
  which will be used as the default value source for the given event `topic`. `value-source`
  may take the following forms:
    * a function taking a live graph and an options map, and returns a channel of values
    * a channel of values
    * a mult of some such value channel"
  ([init] (input init (keyword (gensym))))
  ([init topic]
   (map->Signal {:init init
                 :message-emitter (event-relay #{topic})}))
  ([init topic value-source]
   (let [event-channel-fn (value-source->events-fn value-source
                                                   topic)]
     (map->Signal {:init init
                   :message-emitter (event-relay #{topic})
                   :event-sources {topic event-channel-fn}}))))

(defn constant
  "Returns a constant signal of the given value."
  [x]
  (let [cached (->Cached x)]
    (map->Signal {:init x
                  :message-emitter {:sources [:events]
                                    :msg-fn (constantly cached)}})))

(defn pipeline
  "Takes a transducer `xform`, a fallback value `base`, and a signal `sig`.
  Returns a new signal which pipes values from `sig` through `xform`. Because
  transducers may filter out values, you must provide a `base` which will be
  used as the derived signal's initial value if the initial value of `sig` ends
  up being filtered. If multiple values are emitted from the transduction of the
  initial value of `sig`, then the initial value of the new signal will be the
  _last_ of those emitted. Stateful transducers carry state starting from the
  computation of the new signal's initial value. So for example a `(drop 3)`
  transducer will mean that the initial value will be filtered, `base` will
  be used as a fallback, and then two more values will be dropped before fresh
  values start being emitted."
  [xform base sig]
  (let [reducer ((comp (core/map :value)
                       xform
                       (core/map ->Fresh))
                 conj)
        msg-fn (fn [prev [msg :as msg-in-seq]]
                 (when (fresh? msg)
                   (reduce reducer [] msg-in-seq)))
        pipelined-init-msg (->> (:init sig) ->Fresh vector (msg-fn nil) last)
        init-val (if (nil? pipelined-init-msg)
                   base
                   (:value pipelined-init-msg))]
    (map->Signal {:init init-val
                  :message-emitter {:sources [sig]
                                    :msg-fn msg-fn}})))

(defn mapseq
  "Takes a mapping function `f` and a sequence of signal `sources`, and returns a
  signal of values obtained by applying `f` to the values from the source signals."
  [f sources]
  (if (empty? sources)
    (constant (f))
    (let [sources (vec sources)
          emit-message (fn [prev messages]
                         (when (some fresh? messages)
                           (->Fresh (apply f (mapv :value messages)))))]
      (map->Signal {:init (->> sources
                               (mapv (comp ->Fresh :init))
                               (emit-message nil)
                               :value)
                    :message-emitter {:sources sources
                                      :msg-fn emit-message}}))))

(defn map
  "Takes a mapping function `f` and any number of signal `sources`, and returns a
  signal of values obtained by applying `f` to the values from the source signals."
  [f & sources]
  (mapseq f sources))

(defn template
  "Takes a map whose values are signals, to be used as a template. Returns a new
  signal whose values are maps of the same form as `signal-map`, but with the current
  value of each signal in place of the signal itself."
  [signal-map]
  (let [ks (keys signal-map)]
    (mapseq (fn [& values]
               (zipmap ks values))
             (vals signal-map))))

(defn foldp
  "Create a past-dependent signal (\"fold into the past\"). The values of a `foldp`
  signal are obtained by calling `f` with two arguments: the current value of the
  `source` signal, and the previous value of the new `foldp` signal (acting as the
  \"accumulator\"). `init` provides the initial value of the new signal, and
  therefore acts as the seed accumulator."
  [f init source]
  (map->Signal {:init init
                :message-emitter {:sources [source]
                                  :msg-fn (fn [acc [message]]
                                            (when (fresh? message)
                                              (->Fresh (f (:value message)
                                                          (:value acc)))))}}))

(defn reducep
  "Create a past-dependent signal like `foldp`, but calls `f` with the arguments
  reversed to align with Clojure's `reduce`: the first argument is the accumulator,
  and the second is the current value of `source`. If `init` is omitted, the initial
  value of the new signal will be obtained by calling `f` with no arguments."
  ([f source] (reducep f (f) source))
  ([f init source]
   (->> source
        (foldp (fn [val acc] (f acc val)) init)
        drop-repeats)))

(defn transducep
  "Like `reducep`, but transforms the reducing function `f` with transducer `xform`."
  ([xform f source] (reducep (xform f) (f) source))
  ([xform f init source]
   (reducep (xform f) init source)))

(defn drop-repeats
  "Returns a signal which relays values of `sig`, but drops repeated equal values."
  [sig]
  (map->Signal {:init (:init sig)
                :message-emitter {:sources [sig]
                                  :msg-fn (fn [prev [msg]]
                                            (when (and (fresh? msg)
                                                       (not= (:value msg) (:value prev)))
                                              msg))}}))

(defn async
  "Returns an \"asynchronous\" version of `source`, splitting off a new subgraph which
  does not maintain consistent event ordering relative to the main graph. In exchange,
  signals which depend on an `async` signal don't have to wait for the `source` to finish
  computing new values. This function is mainly useful in multithreaded environments when
  you don't want a slow computation to block the whole graph."
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
  "Takes a sequence of signals `sigs`, and returns a new signal which relays fresh
  values from all of the source signals. When more than one source has fresh values
  at the same time, the first (leftmost) signal in `sigs` will take precedence and
  the other values will be discarded. The initial value of the returned signal is
  equal to the initial value of the first source signal."
  [sigs]
  (map->Signal {:init (:init (first sigs))
                :message-emitter {:sources sigs
                                  :msg-fn (fn [prev messages]
                                            (first (filter fresh? messages)))}}))

(defn merge
  "Takes any number of source signals `sigs`, and returns a new signal which relays
  fresh values from all of the source signals. When more than one source has fresh values
  at the same time, the first (leftmost) signal will take precedence and the other values
  will be discarded. The initial value of the returned signal is equal to the initial
  value of the first source signal."
  [& sigs]
  (mergeseq sigs))

(defn combine
  "Combines a sequence of signals into a signal of vectors. Equivalent to
  `(signal/map vector sig1, sig2, ...)`"
  [sigs]
  (mapseq vector sigs))

(defn sample-on
  "Sample the current value of `value-sig` every time `sampler-sig` updates with a
  fresh value. For example, `(sample-on mouse/clicks mouse/position)` returns a signal
  of click positions."
  [sampler-sig value-sig]
  (map->Signal {:init (:init value-sig)
                :message-emitter {:sources [sampler-sig value-sig]
                                  :msg-fn (fn [prev [sampler-msg value-msg]]
                                            (when (fresh? sampler-msg)
                                              (->Fresh (:value value-msg))))}}))

(defn count
  "Returns a signal whose values are the number of fresh values emitted so far from
  `sig`. Repeated equal values will be counted so long as they are fresh, so if you
  don't want to count repeats then you need to `(count (drop-repeats sig))` instead."
  [sig]
  (foldp #(inc %2) 0 sig))

(defn count-if
  "Like `count`, but only increments the counter if the fresh value emitted from `sig`
  satisfies the predicate funtion `pred`. For example, `(count-if odd? numbers)` returns
  a signal of how many times the `numbers` signal emitted an odd number."
  [pred sig]
  (foldp (fn [v c]
           (if (pred v) (inc c) c))
         0
         sig))

(defn keep-if
  "Returns a signal which relays values from `sig`, but discards any which don't match
  the given predicate function `pred`. A `base` value must be provided in case the initial
  value of `sig` does not match the predicate, in which case `base` is used as the initial
  value of the new signal."
  [pred base sig]
  (map->Signal {:init (if (pred (:init sig))
                        (:init sig)
                        base)
                :message-emitter {:sources [sig]
                                  :msg-fn (fn [prev [msg]]
                                            (when (and (fresh? msg)
                                                       (pred (:value msg)))
                                              (->Fresh (:value msg))))}}))

(defn drop-if
  "Like `keep-if`, but drops values which match the predicate."
  [pred base sig]
  (keep-if (complement pred) base sig))

(defn keep-when
  "Returns a new signal which relays values from `value-sig`, but only when the current
  value of `switch-sig` is truthy."
  [switch-sig base value-sig]
  (->> value-sig
       (map vector (sample-on value-sig switch-sig))
       (keep-if first [false base])
       (map second)))

(defn drop-when
  "Like `keep-when`, but only relays values when `switch-sig` is falsy."
  [switch-sig base value-sig]
  (keep-when (map not switch-sig) base value-sig))

(defn log
  "A little convenience helper which logs signal values with `pr` before propagating them unchanged."
  [sig]
  (map (fn [x] (pr x) x) sig))

; helpers:

(def ^{:doc "A transducer which takes in batches of signal graph messages and pipes out fresh values."}
  fresh-values
  (comp cat
        (filter fresh?)
        (core/map :value)))

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
  (if (>= 1 (core/count msg-batches))
    msg-batches
    (let [max-count (reduce max (core/map core/count msg-batches))
          pad (fn [msgs]
                (->> [msgs (-> msgs last :value ->Cached repeat)]
                     (into [] (comp cat (take max-count)))))]
      (core/map pad msg-batches))))

(defn- transpose
  "Takes a collection of message batches and returns a sequence of vectors of corresponding
  messages from each batch."
  [msg-batches]
  (apply core/map vector msg-batches))

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
  "Take an inert signal and produce a live, running graph."
  ([g] (spawn* g nil))
  ([g opts] (spawn* g opts)))

(defn pipe-to-atom
  "Pipes fresh values from a live graph into an atom. If `x` is a signal, it is `spawn`ed
  as a live graph first. If no atom is provided, then a new atom is created which takes its
  initial value from that of the given signal or graph. If an existing atom is provided along
  with a sequence of keys `ks`, then fresh values will be inserted into the atom's value using
  `swap!` with `assoc-in`. If `ks` is not present, then the whole atom value is replaced with
  `reset!`."
  ([x]
   (let [live-graph (spawn x)]
     (pipe-to-atom live-graph
                   (atom (init live-graph)
                         :meta {::source live-graph}))))
  ([x atm] (pipe-to-atom* x atm nil))
  ([x atm ks] (pipe-to-atom* x atm ks)))
