(ns timelord.core
    (:require [reagent.core :as reagent :refer [atom]]
              [jamesmacaulay.zelkova.signal :as z]
              [jamesmacaulay.zelkova.mouse :as mouse]
              [jamesmacaulay.zelkova.keyboard :as keyboard]
              [jamesmacaulay.zelkova.time :as time]))

(enable-console-print!)


(def ticker mouse/position)

(def counter (z/count ticker))

(def timestamped (time/timestamp counter))

(def delayed (->> counter (time/delay 10000) (time/timestamp)))

(def app-signal (z/template {:counter counter
                             :timestamped timestamped
                             :delayed delayed}))

;; -------------------------
;; State
(defonce app-state (atom {:counter 0
                          :timestamped [0 0]
                          :delayed 0}))

(z/pipe-to-atom app-signal app-state)


;; -------------------------
;; Views

(defn main-page []
  [:div [:pre (pr-str @app-state)]])

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))

