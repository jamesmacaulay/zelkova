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

(defrecord Event
  [topic value])

(defn event
  [topic value]
  (Event. topic value))

(defprotocol Message
  (change? [msg])
  (body [msg]))

(defrecord Change
  [body]
  Message
  (change? [_] true)
  (body [_] body))

(defrecord NoChange
  [body]
  Message
  (change? [_] false)
  (body [_] body))

(defn change
  [val]
  (->Change val))

(defn no-change
  [val]
  (->NoChange val))

(defn- gen-id
  []
  (gensym "event-topic-"))

(defn port
  [m]
  (async/tap m (chan)))

(defprotocol SignalProtocol
  (sprout [this]))

(defprotocol InputSignalProtocol
  (connect-events [this events-mult]))

(defrecord Sprout
  [value channel])

(deftype Signal
  [value mult]
  SignalProtocol
  (sprout
    [_]
    (->Sprout value (port mult))))

(deftype InputSignal
  [value mult events-channel]
  SignalProtocol
  (sprout
    [_]
    (->Sprout value (port mult)))
  InputSignalProtocol
  (connect-events [_ events-mult]
    (async/tap events-mult events-channel)))

(defn input*
  [id init]
  (let [c-in (chan)
        c-out (chan)]
    (go-loop [prev init]
      (let [{eid :topic
             v :value} (<! c-in)
            msg (if (= id eid)
                  (change v)
                  (no-change prev))]
        (>! c-out msg)
        (recur (body msg))))
    (->InputSignal init (async/mult c-out) c-in)))

(defn lift-msgs*
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
    (->Signal v (async/mult c-out))))

(defn foldp-msgs*
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
    (->Signal v (async/mult c-out))))

(defprotocol NodeProtocol
  (node->signal [n source-node->signal])
  (sources [n]))

(defprotocol InputNodeProtocol
  (event-topic [n])
  (event-channel-fn [n]))

(defn input?
  [x]
  (satisfies? InputNodeProtocol x))

(defn input
  ([init]
   (input init nil (gen-id)))
  ([init event-channel-fn']
   (input init event-channel-fn' (gen-id)))
  ([init event-channel-fn' topic]
   (reify
     NodeProtocol
     (node->signal [_ _]
       (input* topic init))
     (sources [_] [])
     InputNodeProtocol
     (event-topic [_] topic)
     (event-channel-fn [_] event-channel-fn'))))

(defn write-port
  [init]
  (let [ch (chan 1)
        mult (async/mult ch)
        node (input init #(async/tap mult (chan)))]
    (reify
      impl/Channel
      (close! [_] (impl/close! ch))
      (closed? [_] (impl/closed? ch))
      impl/WritePort
      (put! [_ val fn1] (impl/put! ch val fn1))
      NodeProtocol
      (node->signal [_ _]
        (input* (event-topic node) init))
      (sources [_] [])
      InputNodeProtocol
      (event-topic [_] (event-topic node))
      (event-channel-fn [_] (event-channel-fn node)))))


(defn lift-msgs
  [f & source-nodes]
  (reify
    NodeProtocol
    (node->signal [_ source-node->signal]
      (apply lift-msgs* f (map source-node->signal source-nodes)))
    (sources [_] source-nodes)))

(defn sample-on
  [sampler-sig value-sig]
  (lift-msgs (fn [sample-msg value-msg]
               (when (change? sample-msg)
                 (change (body value-msg))))
             sampler-sig
             value-sig))

(defn lift
  [f & sigs]
  (apply lift-msgs
         (fn [& args]
           (change (apply f (map body args))))
         sigs))

(defn foldp-msgs
  [f init source-node]
  (reify
    NodeProtocol
    (node->signal
      [_ source-node->signal]
      (foldp-msgs* f init (source-node->signal source-node)))
    (sources [_] [source-node])))

(defn foldp
  [f v sig]
  (foldp-msgs (fn [msg acc-msg]
                (change (f (body msg) (body acc-msg))))
              v
              sig))

(defn constant
  [value]
  (input value))

(defn merge
  [& sigs]
  (apply lift-msgs
         (fn [& msgs]
           (->> msgs (filter change?) first))
         sigs))


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
                 (assoc deps n (set (sources n))))
               (zip/next loc))))))

(defn topsort
  [output]
  (-> output
      output-node->dependency-map
      kahn/kahn-sort
      reverse))

(defprotocol CompiledGraphProtocol
  (default-world [g])
  (spawn* [g world-overrides]))

(defn spawn
  ([g] (spawn* g {}))
  ([g world-overrides] (spawn* g world-overrides)))

(defn spawn-world
  "Takes a `default-world` map, and another map of `world-overrides`. Both
  are maps of event topics to 0-arity functions which return a channel of
  values. A map is returned with the same keys as `default-world`, and values
  obtained by calling the corresponding possibly-overridden functions and
  creating a mult for each channel, with values tagged as events."
  [default-world world-overrides]
  (reduce (fn [m [topic fn]]
            (let [overridden-fn (get world-overrides topic fn)
                  channel (when-not (nil? overridden-fn)
                            (async/map (partial ->Event topic)
                                       [(overridden-fn)]))]
              (assoc m topic channel)))
          {}
          default-world))

(defn build-signal-map
  "Takes a sequence of topologically sorted nodes and returns a map of nodes
  to signals."
  [sorted-nodes]
  (reduce (fn [signal-map node]
            (assoc signal-map
              node (node->signal node signal-map)))
          {}
          sorted-nodes))

(defrecord RunningGraph
  [world output-channel])

(defn wire-up
  [input-nodes output-node spawned-world signal-map]
  (let [output-sprout (sprout (get signal-map output-node))]
    (tools/concat (async/to-chan [(:value output-sprout)])
                  (async/pipe (:channel output-sprout)
                              (chan 1 (comp (filter change?)
                                            (map body)))))))


(defrecord CompiledGraph
  [input-nodes output-node sorted-nodes default-world]
  CompiledGraphProtocol
  (spawn* [_ world-overrides]
    (let [spawned-world (spawn-world default-world world-overrides)
          event-mult (->> input-nodes
                          (sequence (comp (map event-topic)
                                          (map spawned-world)
                                          (filter identity)))
                          async/merge
                          async/mult)
          signal-map (build-signal-map sorted-nodes)
          output-channel (wire-up input-nodes output-node spawned-world signal-map)]
      (doseq [n input-nodes]
        (connect-events (get signal-map n) event-mult))
      (->RunningGraph spawned-world output-channel))))

(defn remove-duplicate-inputs
  [sorted-nodes]
  ; how?
  ; maybe better to remove duplicates when building live signals?
  ; is it even necessary? maybe just an optimization, semantics should be testable
  )

(defn input-nodes->default-world
  [input-nodes]
  (reduce (fn [world node]
            (assoc world (event-topic node) (event-channel-fn node)))
          {}
          input-nodes))

(defn compile-graph
  [output-node]
  (let [sorted-nodes (topsort output-node)
        input-nodes (filter input? sorted-nodes)
        default-world (input-nodes->default-world input-nodes)]
    (->CompiledGraph input-nodes
                     output-node
                     sorted-nodes
                     default-world)))





(comment
  (def numbers-in (chan 1))
  (def letters-in (chan 1))
  (def numbers (signals/input-node :numbers 0 (constantly numbers-in)))
  (def letters (signals/input-node :letters :a (constantly letters-in)))
  (def combination (signals/lift-msgs-node (fn [nm lm] (signals/change [(signals/body nm) (signals/body lm)])) numbers letters))
  (def compiled (signals/compile-graph combination))
  (def running (signals/spawn compiled))
  (def out (:output-channel running))
  (>!! numbers-in 11)
  (>!! letters-in :z)
  (async/close! out)
  (<!! (async/into [] out))


  (def mouse-position
    (signals/input :mouse-position [0 0] #(...)))

  (def events-input (chan))

  )


