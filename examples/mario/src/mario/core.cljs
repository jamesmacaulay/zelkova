(ns mario.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [>! <!]]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.zelkova.keyboard :as keyboard]
            [jamesmacaulay.zelkova.window :as window]
            [jamesmacaulay.async-tools.core :as tools])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

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

(defn render-state
  [[w h] mario]
  (let [verb (cond
               (> (:y mario) 0) "jump"
               (not= (:vx mario) 0) "walk"
               :else "stand")
        src (str "images/" verb "/" (:dir mario) ".gif")]
    (dom/div #js {:style #js {:width (str w "px")
                              :height (str h "px")
                              :backgroundColor "rgb(174,238,238)"}}
      (dom/div #js {:style #js {:width (str w "px")
                                :height "50px"
                                :position "relative"
                                :top (str (- h 50) "px")
                                :backgroundColor "rgb(74,163,41)"}})
      (dom/img #js {:src src
                    :style #js {:position "relative"
                                :left (str (:x mario) "px")
                                :top (str (-> (- h 130) (- (:y mario))) "px")}}))))

;-- MARIO
;input = let delta = lift (\t -> t/20) (fps 25)
;in sampleOn delta (lift2 (,) delta Keyboard.arrows)
;

(def input
  (let [delta (z/map #(/ % 20)
                     (time/fps 25))]
    (z/sample-on delta
                 (z/map vector
                        delta
                        keyboard/arrows))))

;main  = lift2 render Window.dimensions (foldp step mario input)

(def main
  (z/map render-state
         window/dimensions
         (z/foldp step mario input)))

(def app-state (z/pipe-to-atom main))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (dom/div nil app))))
  app-state
  {:target (. js/document (getElementById "app"))})
