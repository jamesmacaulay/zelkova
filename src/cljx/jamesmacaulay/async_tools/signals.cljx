#+clj
(ns jamesmacaulay.async-tools.signals
  (:refer-clojure :exclude [merge])
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

;(defrecord Event
;  [topic value])
;
;(defn event
;  [topic value]
;  (Event. topic value))
;
;(defprotocol Message
;  (change? [msg])
;  (body [msg]))
;
;(defrecord Change
;  [body]
;  Message
;  (change? [_] true)
;  (body [_] body))
;
;(defrecord NoChange
;  [body]
;  Message
;  (change? [_] false)
;  (body [_] body))
;
;(defn change
;  [val]
;  (->Change val))
;
;(defn no-change
;  [val]
;  (->NoChange val))
;
;(defprotocol NodeProtocol
;  (node->mult [n node-mults event-mult])
;  (init [n])
;  (sources [n]))
;
;(defprotocol InputNodeProtocol
;  (event-topic [n])
;  (event-channel-fn [n]))
;
;(defn- gen-id
;  []
;  (gensym "event-topic-"))
;
;(defn port
;  [m]
;  (async/tap m (chan)))
;
;(defprotocol SignalProtocol
;  (sprout [this]))
;
;(defprotocol InputSignalProtocol
;  (connect-events [this events-mult]))
;
;(defrecord Sprout
;  [value channel])
;
;(deftype Signal
;  [value mult]
;  SignalProtocol
;  (sprout
;    [_]
;    (->Sprout value (port mult))))
;
;(deftype InputSignal
;  [value mult events-channel]
;  SignalProtocol
;  (sprout
;    [_]
;    (->Sprout value (port mult)))
;  InputSignalProtocol
;  (connect-events [_ events-mult]
;    (async/tap events-mult events-channel)))
;
;(defn spawn-input
;  [input-node event-mult]
;  (let [init (init input-node)
;        id (event-topic input-node)
;        c-in (async/tap event-mult (chan))
;        c-out (chan)]
;    (go-loop [prev init]
;      (let [event (<! c-in)]
;        (if (nil? event)
;          (async/close! c-out)
;          (let [msg (if (= id (:topic event))
;                      (change (:value event))
;                      (no-change prev))]
;            (>! c-out msg)
;            (recur (body msg))))))
;    (async/mult c-out)))
;
;(defn spawn-lift
;  [f lift-node mults]
;  (let [channels (mapv #(async/tap % (chan)) mults)
;        c-in (async/map vector channels)
;        v (init lift-node)
;        c-out (chan)]
;    (go-loop [prev v]
;      (let [input-msgs (<! c-in)]
;        (if (nil? input-msgs)
;          (async/close! c-out)
;          (let [msg (if (some change? input-msgs)
;                      (let [result (apply f input-msgs)]
;                        (if (and (satisfies? Message result)
;                                 (change? result))
;                          result
;                          (no-change prev)))
;                      (no-change prev))]
;            (>! c-out msg)
;            (recur (body msg))))))
;    (async/mult c-out)))
;
;(defn spawn-foldp
;  [f v mult]
;  (let [c-in (async/tap mult (chan))
;        c-out (chan)]
;    (go-loop [acc (change v)]
;      (let [input-msg (<! c-in)]
;        (if (nil? input-msg)
;          (async/close! c-out)
;          (let [msg (if (change? input-msg)
;                      (let [result (f input-msg acc)]
;                        (if (and (satisfies? Message result)
;                                 (change? result))
;                          result
;                          (no-change acc)))
;                      (no-change acc))]
;            (>! c-out msg)
;            (recur msg)))))
;    (async/mult c-out)))
;
;(defn input?
;  [x]
;  (satisfies? InputNodeProtocol x))
;
;(defn input
;  ([init']
;   (input init' nil (gen-id)))
;  ([init' event-channel-fn']
;   (input init' event-channel-fn' (gen-id)))
;  ([init' event-channel-fn' topic]
;   (reify
;     NodeProtocol
;     (node->mult [n _ event-mult]
;       (spawn-input n event-mult))
;     (init [_] init')
;     (sources [_] [])
;     InputNodeProtocol
;     (event-topic [_] topic)
;     (event-channel-fn [_] event-channel-fn'))))
;
;(defn write-port
;  [init']
;  (let [ch (chan 1)
;        mult (async/mult ch)
;        node (input init' #(async/tap mult (chan)))]
;    (reify
;      impl/Channel
;      (close! [_] (impl/close! ch))
;      (closed? [_] (impl/closed? ch))
;      impl/WritePort
;      (put! [_ val fn1] (impl/put! ch val fn1))
;      NodeProtocol
;      (node->mult [_ _ event-mult]
;        (spawn-input node event-mult))
;      (init [_] init')
;      (sources [_] [])
;      InputNodeProtocol
;      (event-topic [_] (event-topic node))
;      (event-channel-fn [_] (event-channel-fn node)))))
;
;
;(defn lift-msgs
;  [f & source-nodes]
;  (reify
;    NodeProtocol
;    (node->mult [n node-mults _]
;      (spawn-lift f n (mapv node-mults source-nodes)))
;    (init [_] (body (apply f (map (comp change init) source-nodes))))
;    (sources [_] source-nodes)))
;
;(defn sample-on
;  [sampler-sig value-sig]
;  (lift-msgs (fn [sample-msg value-msg]
;               (when (change? sample-msg)
;                 (change (body value-msg))))
;             sampler-sig
;             value-sig))
;
;(defn lift
;  [f & sigs]
;  (apply lift-msgs
;         (fn [& args]
;           (change (apply f (map body args))))
;         sigs))
;
;(defn foldp-msgs
;  [f init' source-node]
;  (reify
;    NodeProtocol
;    (node->mult
;      [_ node-mults _]
;      (spawn-foldp f init' (node-mults source-node)))
;    (init [_] init')
;    (sources [_] [source-node])))
;
;(defn foldp
;  [f v sig]
;  (foldp-msgs (fn [msg acc-msg]
;                (change (f (body msg) (body acc-msg))))
;              v
;              sig))
;
;;(defn async
;;  [node]
;;  (let [topic (gen-id)]
;;    (reify
;;      NodeProtocol
;;      (node->mult
;;        [_ node-mults]
;;        (let [source-sprout (sprout (node-mults node))]
;;
;;          (input* topic (:value source-sprout))))
;;      (sources [_] [node])
;;      InputNodeProtocol
;;      (event-topic [_] topic)
;;      (event-channel-fn [_]
;;        ; can't write this function, need access to live source signal
;;        ))))
;
;(defn constant
;  [value]
;  (input value))
;
;(defn merge
;  [& sigs]
;  (apply lift-msgs
;         (fn [& msgs]
;           (->> msgs (filter change?) first))
;         sigs))
;
;
;(defn node-graph-zipper
;  [output-node]
;  (zip/zipper (constantly true)
;              (comp seq sources)
;              nil
;              output-node))
;
;(defn skip-subtree
;  [loc]
;  (or (zip/right loc)
;      (loop [p loc]
;        (if (zip/up p)
;          (or (zip/right (zip/up p))
;              (recur (zip/up p)))
;          [(zip/node p) :end]))))
;
;(defn output-node->dependency-map
;  [output-node]
;  (loop [deps {}
;         loc (node-graph-zipper output-node)]
;    (cond
;      (zip/end? loc)
;      deps
;      (contains? deps (zip/node loc))
;      (recur deps
;             (skip-subtree loc))
;      :else
;      (let [n (zip/node loc)]
;        (recur (if (sequential? n)
;                 deps
;                 (assoc deps n (set (sources n))))
;               (zip/next loc))))))
;
;(defn topsort
;  [output]
;  (-> output
;      output-node->dependency-map
;      kahn/kahn-sort
;      reverse))
;
;(defprotocol CompiledGraphProtocol
;  (default-world [g])
;  (spawn* [g world-overrides]))
;
;(defn spawn
;  ([g] (spawn* g {}))
;  ([g world-overrides] (spawn* g world-overrides)))
;
;(defn spawn-world
;  "Takes a `default-world` map, and another map of `world-overrides`. Both
;  are maps of event topics to 0-arity functions which return a channel of
;  values. A map is returned with the same keys as `default-world`, and values
;  obtained by calling the corresponding possibly-overridden functions and
;  creating a mult for each channel, with values tagged as events."
;  [default-world world-overrides]
;  (reduce (fn [m [topic fn]]
;            (let [overridden-fn (get world-overrides topic fn)
;                  channel (when-not (nil? overridden-fn)
;                            (async/map (partial ->Event topic)
;                                       [(overridden-fn)]))]
;              (assoc m topic channel)))
;          {}
;          default-world))
;
;(defn build-node-mults
;  "Takes a sequence of topologically sorted nodes and returns a map of nodes
;  to signals."
;  [sorted-nodes event-mult]
;  (reduce (fn [node-mults node]
;            (assoc node-mults
;              node (node->mult node node-mults event-mult)))
;          {}
;          sorted-nodes))
;
;(defrecord RunningGraph
;  [world events-input output-channel])
;
;(defn wire-up
;  [output-node signal-map]
;  (tools/concat (async/to-chan [(init output-node)])
;                (async/pipe (async/tap (get signal-map output-node)
;                                       (chan))
;                            (chan 1 (comp (filter change?)
;                                          (map body))))))
;
;
;(defrecord CompiledGraph
;  [input-nodes output-node sorted-nodes default-world]
;  CompiledGraphProtocol
;  (spawn* [_ world-overrides]
;    (let [spawned-world (spawn-world default-world world-overrides)
;          event-source-channels (->> input-nodes
;                                     (sequence (comp (map event-topic)
;                                                     (map spawned-world)
;                                                     (filter identity))))
;          events-input (if (seq event-source-channels)
;                         (async/merge event-source-channels)
;                         (chan))
;          event-mult (async/mult events-input)
;          signal-map (build-node-mults sorted-nodes event-mult)
;          output-channel (wire-up output-node signal-map)]
;      (->RunningGraph spawned-world events-input output-channel))))
;
;(defn remove-duplicate-inputs
;  [sorted-nodes]
;  ; how?
;  ; maybe better to remove duplicates when building live signals?
;  ; is it even necessary? maybe just an optimization, semantics should be testable
;  )
;
;(defn input-nodes->default-world
;  [input-nodes]
;  (reduce (fn [world node]
;            (assoc world (event-topic node) (event-channel-fn node)))
;          {}
;          input-nodes))
;
;(defn compile-graph
;  [output-node]
;  (let [sorted-nodes (topsort output-node)
;        input-nodes (filter input? sorted-nodes)
;        default-world (input-nodes->default-world input-nodes)]
;    (->CompiledGraph input-nodes
;                     output-node
;                     sorted-nodes
;                     default-world)))

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
  (sources [s]))

(defn signal?
  [x]
  (satisfies? SignalProtocol x))

(defrecord Signal
  [init message-emitter event-emitter]
  SignalProtocol
  (sources [_]
    (into #{}
          (comp (filter identity)
                (mapcat (comp flatten vector first))
                (filter signal?))
          [message-emitter event-emitter])))

(defn- event-relay
  [topic]
  (fn [prev event]
    (if (= topic (:topic event))
      (->Fresh (:value event))
      (->Cached (:value prev)))))

(defn input
  ([init] (input init (gen-topic)))
  ([init topic]
   (map->Signal {:init init
                 :message-emitter [:events (event-relay topic)]})))

(defn lift
  [f & sources]
  (let [emit-message (fn [prev messages]
                       (if (some fresh? messages)
                         (->Fresh (apply f (mapv :value messages)))
                         (->Cached (:value prev))))]
    (map->Signal {:init (->> sources
                             (mapv (comp ->Fresh :init))
                             (emit-message nil)
                             :value)
                  :message-emitter [sources emit-message]})))

(defn foldp
  [f init source]
  (map->Signal {:init init
                :message-emitter [source (fn [acc message]
                                           (if (fresh? message)
                                             (->Fresh (f (:value message) (:value acc)))
                                             (->Cached (:value acc))))]}))

(defn reducep
  ([f source] (reducep f (f) source))
  ([f init source]
   (foldp (fn [val acc] (f acc val)) init source)))

(defn transducep
  ([xform f source] (transducep))
  ([xform f init source]
   (reducep (xform f) init source)))

(defn async
  [source]
  (let [topic (gen-topic)]
    (map->Signal {:init (:init source)
                  :event-emitter [source (fn [message]
                                           (when (fresh? message)
                                             (->Event topic (:value message))))]
                  :message-emitter [:events (event-relay topic)]})))

(defn constant
  [x]
  (let [cached (->Cached x)]
    (map->Signal {:init x
                  :message-emitter [:events (constantly cached)]})))

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
  (if-let [[tmpl msg-fn] (:message-emitter signal)]
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
      (->LiveChannelGraph g events-channel mult-map))))

(extend-protocol Spawnable
  Signal
  (spawn [s] (spawn (compile-graph s))))
