(ns agricola-automa.ui
  (:require [rum.core :as rum]
            [agricola-automa.game :as game]
            [agricola-automa.automa :as automa]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Events / mutations

(defonce game-state
  (atom (game/new-game 3)))

(defn start-round!
  []
  (swap! game-state game/start-round))

(defn end-round!
  []
  (swap! game-state game/end-round))

(defn pickup-worker!
  [automa-uuid]
  (swap! game-state game/pickup-worker automa-uuid))

(defn confirm-worker!
  [automa-uuid]
  (swap! game-state game/confirm-worker-placement automa-uuid))

(defn block-worker!
  [automa-uuid]
  (swap! game-state game/block-worker-placement automa-uuid))

(defn skip-worker-action!
  [automa-uuid]
  (swap! game-state game/skip-worker-action automa-uuid))

(defn pop-major-improvements!
  []
  (swap! game-state game/pop-major-improvements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interface components

(defn ui-current-round
  [n]
  [:p "Current round: "
   [:span n]])

(defn ui-pickup-worker-button
  [uuid]
  [:button
   {:on-click #(pickup-worker! uuid)}
   "Pick up worker"])

(defn ui-confirm-worker-button
  [uuid]
  [:button
   {:on-click #(confirm-worker! uuid)}
   "Confirm placement"])

(defn ui-block-worker-button
  [uuid]
  [:button
   {:on-click #(block-worker! uuid)}
   "Blocked / Not available"])

(defn ui-skip-action-button
  [automa]
  [:button
   {:on-click #(skip-worker-action! (:uuid automa))}
   (or (-> automa :action-stack first :skip-message)
       "Skip")])

(defn ui-automa-controls
  [automa]
  (let [{:keys [uuid action-stack]} automa]
    (when-not (automa/all-turns-completed? automa)
      [:div
       (if (empty? action-stack)
         (ui-pickup-worker-button uuid)
         [:div
          (ui-confirm-worker-button uuid)
          (ui-block-worker-button uuid)
          (when (automa/skippable-spaces (first action-stack))
            (ui-skip-action-button automa))])])))

(defn ui-major-improvement-button
  [game]
  (let [next-maj-imp (-> game :major-improvements first :name)]
    [:button
     {:on-click pop-major-improvements!}
     (str next-maj-imp " (click if already purchased)")]))

(defn ui-automa
  [game automa]
  (let [{:keys [uuid plan]} automa]
    [:div
     {:key uuid}
     [:h3 (str "Automa: " uuid)]
     [:p (str "Workers: " (automa/workers-remaining automa))]
     (when-let [desire (automa/top-action automa)]
       [:p (str "Desired space: " (:space desire))
        (when (= desire automa/major-improvement)
          (ui-major-improvement-button game))])
     (when plan
       (ui-automa-controls automa))]))

(defn ui-round-controls
  [game]
  [:div
   (when (game/awaiting-round-start? game)
     [:button
      {:on-click start-round!}
      "Start round!"])
   (when (game/round-over? game)
     [:button
      {:on-click end-round!}
      "End round"])])

(defn ui-log
  [log]
  [:ul
   (for [{:keys [time automa action major-improvement]} log]
     (let [time-str (.toLocaleTimeString time)]
       [:li {:key time-str}
        (str time-str
             "\t"
             (:uuid automa)
             " took "
             (:space action)
             (when major-improvement
               (str ": " (:name major-improvement))))]))])

(rum/defc ui-game < rum/reactive
  []
  (let [{:keys [round automas log] :as game} (rum/react game-state)]
    (tap> game)
    [:div
     (ui-current-round round)
     (ui-round-controls game)
     (for [[_ automa] automas]
       (ui-automa game automa))
     (ui-log log)]))