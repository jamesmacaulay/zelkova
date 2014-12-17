(ns timelord.core
    (:require [reagent.core :as reagent :refer [atom]]
              [jamesmacaulay.zelkova.signal :as z]
              [jamesmacaulay.zelkova.impl.signal :as zimpl]
              [jamesmacaulay.zelkova.mouse :as mouse]
              [jamesmacaulay.zelkova.time :as time]))

(enable-console-print!)

(def app-signal (z/template {:timestamped (time/timestamp mouse/position)
                             :delayed (time/delay 1000 mouse/position)
                             :debounced (time/debounce 500 mouse/position)}))


;; -------------------------
;; State
(defonce app-state (atom (zimpl/init app-signal)))

(z/pipe-to-atom app-signal app-state)


;; -------------------------
;; Views

(defn main-page []
  [:div [:pre (pr-str @app-state)]])

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))

