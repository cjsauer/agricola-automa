(ns agricola-automa.game
  (:require [agricola-automa.automa :as automa]
            [medley.core :refer [map-kv-vals]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Game state

(defn new-game
  [num-automas]
  (let [automas (vec (repeatedly num-automas automa/make-automa))]
    {:round          1
     :automas        (reduce #(assoc %1 (:uuid %2) %2) {} automas)
     :blocked-spaces #{}
     :log            '()}))

(defn draw-all-automa-plan-cards
  [game]
  (let [current-round (:round game)
        draw          #(automa/draw-plan-card %2 current-round)
        draw-all      #(map-kv-vals draw %)]
    (update game :automas draw-all)))

(defn clear-all-automa-plans
  [game]
  (let [clear          #(dissoc %2 :plan :action-stack)
        clear-all      #(map-kv-vals clear %)]
    (-> game
        (update :round inc)
        (update :automas clear-all))))

(defn start-round
  [game]
  (draw-all-automa-plan-cards game))

(defn end-round
  [game]
  (-> game
      (clear-all-automa-plans)
      (assoc :blocked-spaces #{})))

(defn round-over?
  [game]
  (every? automa/all-turns-completed?
          (-> game :automas vals)))

(defn awaiting-round-start?
  [game]
  (every? #(nil? (:plan %))
          (-> game :automas vals)))

(defn iterate-block-worker-placement
  [game automa]
  (let [retry-cycle    (iterate automa/block-worker-placement automa)
        known-blocked? #(contains? (:blocked-spaces game) %)]
    (first
     (drop-while #(known-blocked? (automa/top-action %))
                 retry-cycle))))

(defn pickup-worker
  [game automa-uuid]
  (let [automa-path    [:automas automa-uuid]
        automa         (-> (get-in game automa-path)
                           (automa/pickup-worker))
        final-automa   (iterate-block-worker-placement game automa)]
    (assoc-in game automa-path final-automa)))

(defn confirm-worker-placement
  [game automa-uuid]
  (let [automa-path  [:automas automa-uuid]
        automa       (get-in game automa-path)
        action       (automa/top-action automa)
        final-automa (automa/confirm-worker-placement automa)]
    (-> game
        (assoc-in automa-path final-automa)
        (update :blocked-spaces conj action)
        (update :log conj {:automa automa
                           :action action
                           :time (js/Date.)}))))

(defn block-worker-placement
  [game automa-uuid]
  (let [automa-path  [:automas automa-uuid]
        automa       (get-in game automa-path)
        action       (automa/top-action automa)
        new-game     (update game :blocked-spaces conj action)
        final-automa (iterate-block-worker-placement new-game automa)]
    (assoc-in new-game automa-path final-automa)))

(defn skip-worker-action
  [game automa-uuid]
  (let [automa-path  [:automas automa-uuid]
        stage        (automa/round->stage (:round game))
        automa       (-> (get-in game automa-path)
                         (automa/draw-action-card stage))
        final-automa (iterate-block-worker-placement game automa)]
    (assoc-in game automa-path final-automa)))