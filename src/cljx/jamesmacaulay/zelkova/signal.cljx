#+clj
(ns jamesmacaulay.zelkova.signal
  (:refer-clojure :exclude [map merge count])
  (:require [clojure.core :as core]
            [clojure.core.async :as async :refer [go go-loop <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.zelkova.impl.signal :as impl]))

#+cljs
(ns jamesmacaulay.zelkova.signal
  (:refer-clojure :exclude [map merge count])
  (:require [cljs.core :as core]
            [cljs.core.async :as async :refer [<! >!]]
            [cljs.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.zelkova.impl.signal :as impl])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

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
      (satisfies? async-impl/ReadPort source) :readport
      (ifn? source) :ifn)))

(defmethod value-source->events-fn :ifn
  [src-fn topic]
  (fn [graph opts]
    (let [ch (src-fn graph opts)]
      (async/pipe ch (async/chan 1 (core/map (partial impl/make-event topic)))))))

(defmethod value-source->events-fn :mult
  [src-mult topic]
  (value-source->events-fn (fn [_ _] (async/tap src-mult (async/chan)))
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
   (impl/make-signal {:init init
                      :relayed-event-topic topic}))
  ([init topic value-source]
   (let [event-channel-fn (value-source->events-fn value-source topic)]
     (impl/make-signal {:init init
                        :relayed-event-topic topic
                        :event-sources {topic event-channel-fn}}))))

(defn constant
  "Returns a constant signal of the given value."
  [x]
  (let [cached (impl/cached x)]
    (impl/make-signal {:init x
                       :sources [:events]
                       :msg-fn (constantly cached)})))

(defn pipeline
  "Takes a stateless transducer `xform`, a fallback value `base`, and a signal
  `sig`. Returns a new signal which pipes values from `sig` through `xform`.
  Because transducers may filter out values, you must provide a `base` which will
  be used as the derived signal's initial value if the initial value of `sig` ends
  up being filtered. If multiple values are emitted from the transduction of the
  initial value of `sig`, then the initial value of the new signal will be the
  _last_ of those emitted. Stateful transducers will give unexpected results and
  are not supported."
  [xform base sig]
  (let [xform' (comp (core/map impl/value) xform (core/map impl/fresh))
        msg-fn (fn [prev [msg :as msg-in-seq]]
                 (when (impl/fresh? msg)
                   (sequence xform' msg-in-seq)))
        pipelined-init-msg (->> (:init sig) impl/fresh vector (msg-fn nil) last)
        init-val (if (nil? pipelined-init-msg)
                   base
                   (impl/value pipelined-init-msg))]
    (impl/make-signal {:init init-val
                        :sources [sig]
                        :msg-fn msg-fn})))

(defn mapseq
  "Takes a mapping function `f` and a sequence of signal `sources`, and returns a
  signal of values obtained by applying `f` to the values from the source signals."
  [f sources]
  (if (empty? sources)
    (constant (f))
    (let [sources (vec sources)
          emit-message (fn [prev messages]
                         (when (some impl/fresh? messages)
                           (impl/fresh (apply f (mapv impl/value messages)))))]
      (impl/make-signal {:init (->> sources
                                    (mapv (comp impl/fresh :init))
                                    (emit-message nil)
                                    impl/value)
                         :sources sources
                         :msg-fn emit-message}))))

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
  (impl/make-signal {:init init
                     :sources [source]
                     :msg-fn (fn [acc [message]]
                               (when (impl/fresh? message)
                                 (impl/fresh (f (impl/value message)
                                                  (impl/value acc)))))}))

(defn drop-repeats
  "Returns a signal which relays values of `sig`, but drops repeated equal values."
  [sig]
  (impl/make-signal {:init (:init sig)
                     :sources [sig]
                     :msg-fn (fn [prev [msg]]
                               (when (and (impl/fresh? msg)
                                          (not= (impl/value msg) (impl/value prev)))
                                 msg))}))

(defn reducep
  "Create a past-dependent signal like `foldp`, with a few differences:
    * calls `f` with the arguments reversed to align with Clojure's `reduce`:
    the first argument is the accumulator, the second is the current value of `source`.
    * if `init` is omitted, the initial value of the new signal will be obtained by
    calling `f` with no arguments.
    * successive equal values of the returned signal are dropped with `drop-repeats`"
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

(defn async
  "Returns an \"asynchronous\" version of `source`, splitting off a new subgraph which
  does not maintain consistent event ordering relative to the main graph. In exchange,
  signals which depend on an `async` signal don't have to wait for the `source` to finish
  computing new values. This function is mainly useful in multithreaded environments when
  you don't want a slow computation to block the whole graph."
  [source]
  (let [topic [::async source]
        msgs->events (comp cat
                           (filter impl/fresh?)
                           (core/map (fn [msg]
                                       (impl/make-event topic (impl/value msg)))))
        events-channel-fn (fn [live-graph _]
                            (async/tap (impl/signal-mult live-graph source)
                                       (async/chan 1 msgs->events)))]
    (impl/make-signal {:init (:init source)
                       :deps [source]
                       :relayed-event-topic topic
                       :event-sources {topic events-channel-fn}})))

(defn splice
  "Splice into the signal graph on the level of core.async channels. Takes a
  `setup!` function which is called when the `source` signal gets wired up into
  a live graph. The `setup!` function is passed two arguments: a `to` channel
  and a `from` channel, in that order. The function is expected to be a consumer
  of the `from` channel and a producer on the `to` channel, and should close the
  `to` channel when the `from` channel is closed. There are no requirements for
  how many values should be put on the `to` channel or when they should be sent.
  `splice` returns a signal with an initial value of `init`, which asynchronously
  produces whichever values are put on the `to` channel in the `setup!` function."
  [setup! init source]
  (let [topic [::splice init setup! source]
        events-channel-fn (fn [live-graph _]
                            (let [from (async/tap (impl/signal-mult live-graph source)
                                                  (async/chan 1 impl/fresh-values))
                                  to (async/chan 1 (core/map (partial impl/make-event topic)))]
                              (setup! to from)
                              to))]
    (impl/make-signal {:init init
                       :deps [source]
                       :relayed-event-topic topic
                       :event-sources {topic events-channel-fn}})))

(defn mergeseq
  "Takes a sequence of signals `sigs`, and returns a new signal which relays fresh
  values from all of the source signals. When more than one source has fresh values
  at the same time, the first (leftmost) signal in `sigs` will take precedence and
  the other values will be discarded. The initial value of the returned signal is
  equal to the initial value of the first source signal."
  [sigs]
  (impl/make-signal {:init (:init (first sigs))
                     :sources sigs
                     :msg-fn (fn [prev messages]
                               (first (filter impl/fresh? messages)))}))

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
  (impl/make-signal {:init (:init value-sig)
                     :sources [sampler-sig value-sig]
                     :msg-fn (fn [prev [sampler-msg value-msg]]
                               (when (impl/fresh? sampler-msg)
                                 (impl/fresh (impl/value value-msg))))}))

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
  (impl/make-signal {:init (if (pred (:init sig))
                             (:init sig)
                             base)
                     :sources [sig]
                     :msg-fn (fn [prev [msg]]
                               (when (and (impl/fresh? msg)
                                          (pred (impl/value msg)))
                                 (impl/fresh (impl/value msg))))}))

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

(defn spawn
  "Take an inert signal and produce a live, running graph."
  ([s] (impl/spawn* s nil))
  ([s opts] (impl/spawn* s opts)))

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
                   (atom (impl/init live-graph)
                         :meta {::source live-graph}))))
  ([x atm] (impl/pipe-to-atom* x atm nil))
  ([x atm ks] (impl/pipe-to-atom* x atm ks)))
