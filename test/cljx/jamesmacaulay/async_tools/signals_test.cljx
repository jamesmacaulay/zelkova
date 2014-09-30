#+clj
(ns jamesmacaulay.async-tools.signals-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.async-tools.signals :as signals]
            [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is testing)])
  (:import [java.util.Date]))

#+cljs
(ns jamesmacaulay.async-tools.signals-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.async-tools.signals :as signals]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is testing)])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(deftest-async test-io
  (go
    (let [in (signals/write-port 0)
          out (-> in
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= 0 (<! out)))
      (>! in 1)
      (is (= 1 (<! out)))
      (async/close! in))))

(deftest-async test-lift
  (go
    (let [in (signals/write-port 0)
          incremented (signals/lift inc in)
          out (-> incremented
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= 1 (<! out)))
      (>! in 1)
      (is (= 2 (<! out)))
      (async/close! in))
    (let [ins [(signals/write-port 0)
               (signals/write-port 0)
               (signals/write-port 0)]
          summed (apply signals/lift + ins)
          out (-> summed
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= 0 (<! out)))
      (>! (ins 0) 1)
      (is (= 1 (<! out)))
      (>! (ins 1) 2)
      (is (= 3 (<! out)))
      (>! (ins 2) 3)
      (is (= 6 (<! out)))
      (>! (ins 0) 10)
      (is (= 15 (<! out)))
      (doseq [ch ins]
        (async/close! ch)))))


(deftest-async test-foldp
  (go
    (let [in (signals/write-port 0)
          sum (signals/foldp + 0 in)
          out (-> sum
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= 0 (<! out)))
      (>! in 1)
      (is (= 1 (<! out)))
      (>! in 1)
      (is (= 2 (<! out)))
      (>! in 10)
      (is (= 12 (<! out)))
      (async/close! in))))

(deftest-async test-regular-signals-are-synchronous
  (go
    (let [in (signals/write-port 0)
          decremented (signals/lift dec in)
          incremented (signals/lift inc in)
          combined (signals/lift (fn [a b] {:decremented a
                                            :incremented b})
                                 decremented
                                 incremented)
          out (-> combined
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= {:decremented -1
              :incremented 1}
             (<! out)))
      (>! in 2)
      (is (= {:decremented 1
              :incremented 3}
             (<! out)))
      (>! in 10)
      (is (= {:decremented 9
              :incremented 11}
             (<! out)))
      (async/close! in))))

;(deftest-async test-async-makes-signals-asynchronous
;  (go
;    (let [in (chan 1000)
;          wp (signals/write-port 0)
;          decremented (signals/lift dec wp)
;          incremented (signals/lift inc wp)
;          async-incremented (signals/async incremented)
;          combined (signals/lift (fn [a b] {:decremented a
;                                            :async-incremented b})
;                                 decremented
;                                 async-incremented)
;          out (signals/read-port combined)]
;      (async/pipe in wp)
;      (>! in 2)
;      (is (= {:decremented 1
;              :async-incremented 1}
;             (<! out)))
;      (is (= {:decremented 1
;              :async-incremented 3}
;             (<! out)))
;      (>! in 10)
;      (is (= {:decremented 9
;              :async-incremented 3}
;             (<! out)))
;      (is (= {:decremented 9
;              :async-incremented 11}
;             (<! out))))))

(deftest-async test-constant
  (go
    (let [in (signals/write-port 0)
          foo (signals/constant :foo)
          combine (signals/lift vector in foo)
          out (-> combine
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= [0 :foo] (<! out)))
      (>! in 1)
      (is (= [1 :foo] (<! out)))
      (>! in 2)
      (is (= [2 :foo] (<! out)))
      (async/close! in))))

(deftest-async test-merge
  (go
    (let [in1 (signals/write-port 0)
          in2 (signals/write-port 0)
          merged (signals/merge in1 in2)
          out (-> merged
                  signals/compile-graph
                  signals/spawn
                  :output-channel)]
      (is (= 0 (<! out)))
      (>! in1 1)
      (is (= 1 (<! out)))
      (>! in2 2)
      (is (= 2 (<! out)))
      (>! in1 3)
      (is (= 3 (<! out)))
      (async/close! in1)
      (async/close! in2))))

(deftest-async test-sample-on
  (go
    (let [fake-mouse-position (signals/input [0 0] nil :mouse-position)
          fake-mouse-clicks (signals/input :click nil :mouse-clicks)
          graph (->> (signals/sample-on fake-mouse-clicks fake-mouse-position)
                     signals/compile-graph
                     signals/spawn)
          out (:output-channel graph)
          pos (partial signals/->Event :mouse-position)
          click (signals/->Event :mouse-clicks :click)]
      (async/onto-chan (:events-input graph)
                       [(pos [10 10])
                        click
                        (pos [20 20])
                        (pos [30 30])
                        click
                        (pos [40 40])
                        (pos [50 50])
                        click])
      (is (= [0 0] (<! out)))
      (is (= [10 10] (<! out)))
      (is (= [30 30] (<! out)))
      (is (= [50 50] (<! out))))))

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
