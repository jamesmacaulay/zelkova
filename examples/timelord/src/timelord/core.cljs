(ns ^:figwheel-always timelord.core
  (:require [reagent.core :as reagent :refer [atom]]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as zimpl]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]))

(enable-console-print!)

(def app-signal (z/template {:timestamped (time/timestamp mouse/position)
                             :delayed (time/delay 1000 mouse/position)
                             :debounced (time/debounce 500 mouse/position)}))

(def live-graph (z/spawn app-signal))

;; -------------------------
;; State
(defonce app-state (atom (zimpl/init live-graph)))

(z/pipe-to-atom live-graph app-state)


;; -------------------------
;; Views

(defn main-page []
  [:div [:pre (pr-str @app-state)]])

;; -------------------------
;; Initialize app
(reagent/render-component [main-page] (.getElementById js/document "app"))
