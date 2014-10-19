(ns drag-and-drop.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [>! <!]]
            [jamesmacaulay.zelkova.signal :as z :refer [lift sample-on foldp reducep]]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.async-tools.core :as tools])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def init-state {:placed-boxes []
                 :drag nil})

(defn pos->hue
  [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(defn build-box
  [[x1 y1] [x2 y2]]
  (let [[top bottom] (sort [y1 y2])
        [left right] (sort [x1 x2])
        centre [(/ (+ left right) 2)
                (/ (+ top bottom) 2)]]
    {:top top
     :left left
     :width (- right left)
     :height (- bottom top)
     :hue (pos->hue centre)}))

(defn centred-box
  [centre-pos]
  (let [scale [20 20]]
    (build-box (map - centre-pos scale)
               (map + centre-pos scale))))

(defn in-box?
  [[x y]
   {:keys [top left width height]}]
  (and (< left x (+ left width))
       (< top y (+ top height))))

(defn add-box
  [pos state]
  (update-in state
             [:placed-boxes]
             (fn [boxes]
               (if (some (partial in-box? pos)
                         (concat boxes (-> state :drag :moving-boxes)))
                 boxes
                 (conj boxes (centred-box pos))))))


(defn topleft-pos
  [{:keys [left top]}]
  [left top])

(defn start-drag
  [pos state]
  (let [drag-target? (partial in-box? pos)
        drag-offset (comp (partial map - pos) topleft-pos)
        state-with-drag (reduce (fn [state box]
                                  (if (drag-target? box)
                                    (update-in state [:drag :moving-boxes] conj (assoc box :drag-offset (drag-offset box)))
                                    (update-in state [:placed-boxes] conj box)))
                                (assoc state
                                  :placed-boxes []
                                  :drag {:moving-boxes []
                                         :resizing-box nil
                                         :start-pos pos})
                                (:placed-boxes state))]
    (if (-> state-with-drag :drag :moving-boxes seq)
      state-with-drag
      (assoc-in state-with-drag
                [:drag :resizing-box]
                (build-box pos pos)))))

(defn drag
  [pos state]
  (let [drag-to-pos (fn [box]
                      (let [[left top] (map - pos (:drag-offset box))]
                        (assoc box :left left :top top)))]
    (-> state
        (update-in [:drag :moving-boxes] (partial mapv drag-to-pos))
        (update-in [:drag :resizing-box] (fn [box]
                                           (when box
                                             (build-box pos (-> state :drag :start-pos))))))))
(defn drop-moving-boxes
  [state]
  (-> state
      (update-in [:placed-boxes]
                 (fn [boxes]
                   (let [moved-boxes (map #(dissoc % :drag-offset)
                                          (-> state :drag :moving-boxes))]
                     (into [] (concat boxes moved-boxes)))))))

(defn drop-resizing-box
  [state]
  (-> state
      (update-in [:placed-boxes]
                 (fn [boxes]
                   (if-let [resizing-box (-> state :drag :resizing-box)]
                     (conj boxes resizing-box)
                     boxes)))))

(defn stop-drag
  [state]
  (-> state
      drop-moving-boxes
      drop-resizing-box
      (assoc :drag nil)))

(defrecord NoOp [] IFn (-invoke [_ state] state))
(defrecord AddBox [pos] IFn (-invoke [_ state] (add-box pos state)))
(defrecord StartDrag [pos] IFn (-invoke [_ state] (start-drag pos state)))
(defrecord Drag [pos] IFn (-invoke [_ state] (drag pos state)))
(defrecord StopDrag [] IFn (-invoke [_ state] (stop-drag state)))

(def state-signal
  (let [dragging-positions (z/keep-when mouse/down?
                                        [0 0]
                                        mouse/position)
        dragging? (z/drop-repeats (z/merge (z/keep-if not false mouse/down?)
                                           (z/lift (constantly true)
                                                   dragging-positions)))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        new-box-positions (->> mouse/position (z/sample-on mouse/clicks) (z/drop-when dragging? [0 0]))
        actions (z/log (z/merge (z/constant (->NoOp))
                                (z/sample-on dragstarts (z/lift ->StartDrag mouse/position))
                                (z/lift (constantly (->StopDrag)) dragstops)
                                (z/lift ->Drag dragging-positions)
                                (z/lift ->AddBox new-box-positions)))]
    (z/foldp (fn [action state]
               (action state))
             init-state
             actions)))

(def app-state (z/pipe-to-atom state-signal))

(defn box-color
  [opacity box]
  (str "hsla(" (:hue box) ",50%,50%," opacity ")"))

(defn render-box
  [opacity box]
  (when box
    (dom/div #js {:style #js {:backgroundColor (box-color opacity box)
                              :position "absolute"
                              :top (:top box)
                              :left (:left box)
                              :width (:width box)
                              :height (:height box)}}
             nil)))

(def render-solid-box (partial render-box 1))

(def render-ghost-box (partial render-box 0.5))

(defn render-state
  [state]
  (let [placed-boxes (:placed-boxes state)
        moving-boxes (-> state :drag :moving-boxes)
        resizing-box (-> state :drag :resizing-box)]
    (apply dom/div
           nil
           (concat (map render-solid-box (:placed-boxes state))
                   (map render-ghost-box (if resizing-box
                                           (conj moving-boxes resizing-box)
                                           moving-boxes))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  app-state
  {:target (. js/document (getElementById "app"))})
