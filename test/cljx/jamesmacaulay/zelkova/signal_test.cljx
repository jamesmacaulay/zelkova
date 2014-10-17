#+clj
(ns jamesmacaulay.zelkova.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is are testing)])
  (:import [java.util.Date]))

#+cljs
(ns jamesmacaulay.zelkova.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is are testing)])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn event-constructor
  [topic]
  (partial z/->Event topic))

(deftest test-signal-sources
  (let [input (z/input 0)
        foldp (z/foldp + 0 input)
        lift (z/lift vector input foldp)
        async (z/async lift)]
    (are [sig sources] (= (z/sources sig) sources)
         input #{}
         foldp #{input}
         lift #{input foldp}
         async #{lift})))

(deftest test-output-node->dependency-map
  (let [input (z/input 0)
        foldp (z/foldp + 0 input)
        lift (z/lift vector input foldp)
        async (z/async lift)]
    (are [out deps] (= (z/output-node->dependency-map out) deps)
         input {input #{}}
         foldp {input #{}
                foldp #{input}}
         lift {input #{}
               foldp #{input}
               lift #{input foldp}}
         async {input #{}
                foldp #{input}
                lift #{input foldp}
                async #{lift}})))

(deftest test-topsort
  (let [input (z/input 0)
        foldp (z/foldp + 0 input)
        lift (z/lift vector input foldp)
        async (z/async lift)]
    (are [out sorted-sigs] (= (z/topsort out) sorted-sigs)
         input [input]
         foldp [input foldp]
         lift [input foldp lift]
         async [input foldp lift async])))

(deftest-async test-wiring-things-up
  (go
    (let [number (event-constructor :numbers)
          letter (event-constructor :letters)
          numbers-input (z/input 0 :numbers)
          letters-input (z/input :a :letters)
          pairs (z/lift vector numbers-input letters-input)
          live-graph (z/spawn pairs)
          output (async/tap live-graph
                            (chan 1 (comp (filter z/fresh?)
                                          (map :value))))]
      (async/onto-chan live-graph
                       [(number 1)
                        (letter :b)
                        (number 2)
                        (letter :c)])
      (is (= [[1 :a] [1 :b] [2 :b] [2 :c]]
             (<! (async/into [] output)))))))

(deftest-async test-io
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          graph (z/spawn in)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init in)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [1 2 3]
             (<! (async/into [] out)))))))

(deftest-async test-lift
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          incremented (z/lift inc in)
          graph (z/spawn incremented)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 1 (:init incremented)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [2 3 4]
             (<! (async/into [] out)))))
    (let [[a b c] (map event-constructor [:a :b :c])
          ins (map (partial z/input 0) [:a :b :c])
          summed (apply z/lift + ins)
          graph (z/spawn summed)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init summed)))
      (async/onto-chan graph [(a 1) (b 2) (c 3) (a 10)])
      (is (= [1 3 6 15]
             (<! (async/into [] out)))))
    (let [zero-arity-+-lift (z/lift +)
          zero-arity-vector-lift (z/lift vector)]
      (is (= 0 (:init zero-arity-+-lift)))
      (is (= [] (:init zero-arity-vector-lift))))))

(deftest-async test-foldp
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          sum (z/foldp + 0 in)
          graph (z/spawn sum)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init sum)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [1 3 6]
             (<! (async/into [] out)))))))

(deftest-async test-regular-signals-are-synchronous
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          decremented (z/lift dec in)
          incremented (z/lift inc in)
          combined (z/lift (fn [a b] {:decremented a
                                            :incremented b})
                                 decremented
                                 incremented)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (async/onto-chan graph (map number [2 10]))
      (is (= [{:decremented 1
               :incremented 3}
              {:decremented 9
               :incremented 11}]
             (<! (async/into [] out)))))))

(deftest-async test-constant
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          foo (z/constant :foo)
          combined (z/lift vector in foo)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= [0 :foo] (:init combined)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [[1 :foo] [2 :foo] [3 :foo]]
             (<! (async/into [] out)))))))

(deftest-async test-merge
  (go
    (let [a (event-constructor :a)
          b (event-constructor :b)
          a-in (z/input 10 :a)
          b-in (z/input 20 :b)
          b-dec (z/lift dec b-in)
          b-inc (z/lift inc b-in)
          merged (z/merge a-in b-dec b-in b-inc)
          graph (z/spawn merged)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 10 (:init merged)))
      (async/onto-chan graph [(a 20) (b 30) (a 40) (b 50)])
      (is (= [20 29 40 49]
             (<! (async/into [] out)))))))

(deftest-async test-combine
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          inc'd (z/lift inc in)
          combined (z/combine [in inc'd])
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= [0 1] (:init combined)))
      (async/onto-chan graph (map number [1 2]))
      (is (= [[1 2] [2 3]]
             (<! (async/into [] out)))))
    (let [empty-combined (z/combine [])]
      (is (= [] (:init empty-combined))))))


(deftest-async test-sample-on
  (go
    (let [pos (event-constructor :mouse-position)
          click ((event-constructor :mouse-clicks) :click)
          fake-mouse-position (z/input [0 0] :mouse-position)
          fake-mouse-clicks (z/input :click :mouse-clicks)
          sampled (z/sample-on fake-mouse-clicks fake-mouse-position)
          graph (z/spawn sampled)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= [0 0] (:init sampled)))
      (async/onto-chan graph
                       [(pos [10 10])
                        click
                        (pos [20 20])
                        (pos [30 30])
                        click
                        (pos [40 40])
                        (pos [50 50])
                        click])
      (is (= [[10 10] [30 30] [50 50]]
             (<! (async/into [] out)))))))

(deftest-async test-transducep
  (go
    (let [number (event-constructor :numbers)
          in (z/input 10 :numbers)
          odd-increments (z/transducep (comp (map inc)
                                                   (filter odd?))
                                             conj
                                             in)
          graph (z/spawn odd-increments)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= [] (:init odd-increments)))
      (async/onto-chan graph (map number [20 21 22 23]))
      (is (= [[21]
              [21 23]]
             (<! (async/into [] out)))))))

(deftest-async test-count
  (go
    (let [in1-event (event-constructor :in1)
          in2-event (event-constructor :in2)
          in1 (z/input 1 :in1)
          in2 (z/input 1 :in2)
          count1 (z/count in1)
          combined (z/lift vector count1 in1 in2)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init count1)))
      (is (= [0 1 1] (:init combined)))
      (async/onto-chan graph [(in1-event 2)
                              (in1-event 3)
                              (in2-event 2)
                              (in1-event 4)])
      (is (= [[1 2 1]
              [2 3 1]
              [2 3 2]
              [3 4 2]]
             (<! (async/into [] out)))))))

(deftest-async test-count-if
  (go
    (let [in1-event (event-constructor :in1)
          in2-event (event-constructor :in2)
          in1 (z/input 1 :in1)
          in2 (z/input 1 :in2)
          count1-odd (z/count-if odd? in1)
          combined (z/lift vector count1-odd in1 in2)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init count1-odd)))
      (async/onto-chan graph [(in1-event 2)
                              (in1-event 3)
                              (in2-event 2)
                              (in1-event 4)
                              (in1-event 5)])
      (is (= [[0 2 1]
              [1 3 1]
              [1 3 2]
              [1 4 2]
              [2 5 2]]
             (<! (async/into [] out)))))))

(deftest-async test-keep-if
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          oddnums (z/keep-if odd? -1 in)
          count-odd (z/count oddnums)
          evennums (z/keep-if even? -2 in)
          count-even (z/count evennums)
          combined (z/lift vector oddnums count-odd evennums count-even)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= [-1 0 0 0] (:init combined)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [[1 1 0 0]
              [1 1 2 1]
              [3 2 2 1]]
             (<! (async/into [] out)))))))

(deftest-async test-keep-when
  (go
    (let [number (event-constructor :numbers)
          letter (event-constructor :letters)
          numbers-in (z/input 0 :numbers)
          letters-in (z/input :a :letters)
          odd-kept-letters (z/keep-when (z/lift odd? numbers-in) :false-init letters-in)
          graph (z/spawn odd-kept-letters)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= :false-init (:init odd-kept-letters)))
      (is (= :a (:init (z/keep-when (z/lift even? numbers-in) :z letters-in))))
      (async/onto-chan graph [(letter :b)
                              (number 1)
                              (letter :c)
                              (number 2)
                              (letter :d)
                              (letter :e)
                              (number 3)
                              (letter :f)])
      (is (= [:c :f] (<! (async/into [] out)))))))

(deftest-async test-drop-repeats
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          no-repeats (z/drop-repeats in)
          graph (z/spawn no-repeats)
          out (async/tap graph (chan 1 z/fresh-values))]
      (is (= 0 (:init no-repeats)))
      (async/onto-chan graph (map number [1 1 2 1 2 2 2 1 1]))
      (is (= [1 2 1 2 1] (<! (async/into [] out)))))))

(deftest-async test-world-building
  (go
    (let [pos (event-constructor :mouse-position)
          event-source (async/to-chan (map pos [[10 10]
                                                [20 20]
                                                [30 30]]))
          mouse-position (assoc (z/input [0 0] :mouse-position)
                           :event-sources {:mouse-position (constantly event-source)})
          graph (z/spawn mouse-position)
          out (async/tap graph (chan 1 z/fresh-values))]
      (z/connect-to-world graph nil)
      (is (= [[10 10] [20 20] [30 30]]
             (<! (async/into [] out)))))))

(deftest-async test-async-makes-signals-asynchronous
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          decremented (z/lift dec in)
          incremented (z/lift inc in)
          async-incremented (z/async incremented)
          combined (z/combine [decremented async-incremented])
          graph (z/spawn combined)
          out (async/tap graph (chan 1 z/fresh-values))]
      (z/connect-to-world graph nil)
      (is (= [-1 1] (:init combined)))
      (>! graph (number 1))
      (is (= [0 1] (<! out)))
      (is (= [0 2] (<! out)))
      (>! graph (number 2))
      (is (= [1 2] (<! out)))
      (is (= [1 3] (<! out)))
      (async/close! graph)
      (is (= nil (<! out))))))

(deftest test-template
  (let [tmpl (z/template {:a (z/input 1 :a)
                          :b (z/input 2 :b)})]
    (is (= {:a 1 :b 2}
           (:init tmpl)))))

(comment
  ; A little excercise to get a feel for how this might work...
  ; Here is Elm's Mario example, translated into a possible Clojure form from this version:
  ; https://github.com/elm-lang/elm-lang.org/blob/009de952c89592c180c43b60137f338651a1f9f6/public/examples/Intermediate/Mario.elm

  ;import Keyboard
  ;import Window
  ;
  ;-- MODEL
  ;mario = { x=0, y=0, vx=0, vy=0, dir="right" }


  (def mario {:x 0 :y 0 :vx 0 :vy 0 :dir "right"})

  ;
  ;
  ;-- UPDATE -- ("m" is for Mario)
  ;jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
  (defn jump
    [{y :y} m]
    (if (and (> y 0)
             (= 0 (:y m)))
      (assoc m :vy 5)
      m))

  ;gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
  (defn gravity
    [t m]
    (if (> (:y m) 0)
      (update-in m [:vy] (partial + (/ t -4.0)))
      m))

  ;physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
  (defn physics
    [t m]
    (-> m
        (update-in [:x] (partial + (* t (:vx m))))
        (update-in [:y] (comp (partial max 0)
                              (partial + (* t (:vy m)))))))

  ;walk {x} m = { m | vx <- toFloat x
  ;               , dir <- if | x < 0     -> "left"
  ;               | x > 0     -> "right"
  ;               | otherwise -> m.dir }
  (defn walk
    [{x :x} m]
    (-> m
        (assoc :vx (float x)
               :dir (cond
                      (< x 0) "left"
                      (> x 0) "right"
                      :else (:dir m)))))

  ;
  ;step (t,dir) = physics t . walk dir . gravity t . jump dir
  ;

  (defn step
    [[t dir] mario]
    (->> mario
         (jump dir)
         (gravity t)
         (walk dir)
         (physics t)))

  ;
  ;-- DISPLAY
  ;render (w',h') mario =
  ;let (w,h) = (toFloat w', toFloat h')
  ;verb = if | mario.y  >  0 -> "jump"
  ;| mario.vx /= 0 -> "walk"
  ;| otherwise     -> "stand"
  ;src  = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  ;in collage w' h'
  ;[ rect w h  |> filled (rgb 174 238 238)
  ;  , rect w 50 |> filled (rgb 74 163 41)
  ;  |> move (0, 24 - h/2)
  ;  , toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
  ;  ]
  ;

  (defn render
    [[w' h'] mario]
    (let [w (float w)
          h (float h)
          verb (cond
                 (> (:y mario) 0) "jump"
                 (not= (:vx mario) 0) "walk"
                 :else "stand")
          src (str "/imgs/mario/" verb "/" (:dir mario) ".gif")]
      (collage w' h'
               [(->> (rect w h)
                     (filled (rgb 174 238 238)))
                (->> (rect w 50)
                     (filled (rgb 74 163 41))
                     (move [0 (- 24 (/ h 2.0))]))
                (->> (to-form (image 35 35 src))
                     (move [(:x mario)
                            (-> (:y mario)
                                (+ 62)
                                (- (/ h 2)))]))])))


  ;-- MARIO
  ;input = let delta = lift (\t -> t/20) (fps 25)
  ;in sampleOn delta (lift2 (,) delta Keyboard.arrows)
  ;

  (def input
    (let [delta (lift #(/ % 20)
                      (fps 25))]
      (sample-on delta
                 (lift vector
                       delta
                       keyboard/arrows))))

  ;main  = lift2 render Window.dimensions (foldp step mario input)

  (def main
    (lift render
          window/dimensions
          (foldp step mario input)))
  )
