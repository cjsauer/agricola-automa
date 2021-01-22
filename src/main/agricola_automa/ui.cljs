(ns agricola-automa.ui
  (:require [rum.core :as rum]
            [agricola-automa.game :as game]
            [agricola-automa.automa :as automa]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Events / mutations

(defonce game-state
  (atom nil))

(defn new-game!
  [num-bots]
  (reset! game-state (game/new-game num-bots)))

(defn change-automa-color!
  [automa-uuid]
  (swap! game-state game/cycle-automa-color automa-uuid))

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
  [:p.text-2xl "Current round: "
   [:span n]])

(defn ui-button
  [props title]
  [:button.p-2.rounded.text-lg.font-bold.mr-2
   props
   title])

(defn ui-start-round-button
  []
  (ui-button
   {:on-click start-round!
    :class ["bg-green-500" "hover:bg-green-700" "text-white"]}
   "Start round!"))

(defn ui-end-round-button
  []
  (ui-button
   {:on-click end-round!
    :class ["bg-red-400" "hover:bg-red-600"]}
   "End round"))

(defn ui-pickup-worker-button
  [uuid]
  (ui-button
   {:on-click #(pickup-worker! uuid)
    :class ["bg-blue-500" "hover:bg-blue-600" "text-white"]}
   "Pick up worker"))

(defn ui-confirm-worker-button
  [uuid]
  (ui-button
   {:on-click #(confirm-worker! uuid)
    :class ["bg-green-400" "hover:bg-green-600"]}
   "Confirm placement"))

(defn ui-block-worker-button
  [uuid]
  (ui-button
   {:on-click #(block-worker! uuid)
    :class ["bg-red-400" "hover:bg-red-600"]}
   "Blocked / Not available"))

(defn ui-skip-action-button
  [automa]
  (ui-button
   {:on-click #(skip-worker-action! (:uuid automa))
    :class ["bg-yellow-300" "hover:bg-yellow-500"]}
   (or (-> automa :action-stack first :skip-message)
       "Skip")))

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
  (when-let [next-maj-imp (-> game :major-improvements first :name)]
    (ui-button
     {:on-click pop-major-improvements!
      :class ["bg-gray-300" "hover:bg-gray-400"]}
     (str next-maj-imp " (click if already purchased)"))))

(defn ui-automa
  [game automa]
  (let [{:keys [uuid color plan]} automa]
    [:div.border-2.border-gray-400.p-2.my-4.text-lg.leading-10
     {:key uuid
      :class ["h-1/4"]}
     [:h3.bg-gray-100
      {:class [(str "text-" color "-600") "cursor-pointer"]
       :on-click #(change-automa-color! uuid)}
      (str "Automa: " color)]
     [:p (str "Workers: " (automa/workers-remaining automa))]
     (when-let [desire (automa/top-action automa)]
       [:p.font-bold
        (str "Desired space: " (:space desire))
        (when (= desire automa/major-improvement)
          (ui-major-improvement-button game))])
     (when plan
       (ui-automa-controls automa))]))

(defn ui-round-controls
  [game]
  [:div.h-10
   (when (game/awaiting-round-start? game)
     (ui-start-round-button))
   (when (game/round-over? game)
     (ui-end-round-button))])

(defn ui-log
  [log]
  [:ul
   (for [{:keys [time automa action major-improvement]} log]
     (let [time-str (.toLocaleTimeString time)]
       [:li {:key (str (.getMilliseconds time))}
        (str time-str
             (:uuid automa)
             " took "
             (:space action)
             (when major-improvement
               (str ": " (:name major-improvement))))]))])

(defn ui-game
  [game]
  (let [{:keys [round automas log]} game]
    (tap> game)
    [:div.container.mx-auto
     (ui-current-round round)
     (ui-round-controls game)
     [:div.flex.flex-col
      (for [[_ automa] automas]
        (ui-automa game automa))]
     (ui-log log)]))

(defn ui-bot-count-button
  [value]
  (ui-button
   {:on-click #(new-game! value)
    :class ["bg-gray-200" "hover:bg-gray-300" "my-2"]}
   value))

(rum/defc ui-app < rum/reactive
  []
  (let [game (rum/react game-state)]
    (if (:automas game)
      (ui-game game)
      [:div.container.mx-auto.md:w-64.border-2.border-gray-300.p-3
       [:p.text-2xl "How many bots?"]
       [:div.flex.flex-col
        (ui-bot-count-button 1)
        (ui-bot-count-button 2)
        (ui-bot-count-button 3)]])))