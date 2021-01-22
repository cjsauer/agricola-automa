(ns agricola-automa.game
  (:require [agricola-automa.automa :as automa]
            [medley.core :refer [map-kv-vals]]
            [clojure.set :as cset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Game state

(def player-colors
  #{"red"
    "blue"
    "yellow"
    "purple"})

(defn new-game
  [num-automas]
  (let [rand-colors (shuffle player-colors)
        automas     (vec
                     (map (fn [automa color]
                            (assoc automa :color color))
                          (repeatedly num-automas automa/make-automa)
                          rand-colors))]
    {:round              1
     :automas            (reduce #(assoc %1 (:uuid %2) %2) {} automas)
     :major-improvements (automa/shuffle-major-improvements)
     :blocked-spaces     #{}
     :log                '()}))

(defn cycle-automa-color
  [game automa-uuid]
  (let [automas        (vals (:automas game))
        curr-colors    (set (map :color automas))
        missing-colors (cset/difference player-colors curr-colors)
        rand-colors    (shuffle missing-colors)
        new-color      (first rand-colors)]
    (assoc-in game [:automas automa-uuid :color] new-color)))

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
  (let [blocked-spaces (if (empty? (:major-improvements game))
                         #{automa/major-improvement}
                         #{})]
    (-> game
        (clear-all-automa-plans)
        (assoc :blocked-spaces blocked-spaces))))

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

(defn pop-major-improvements
  [game]
  (update game :major-improvements rest))

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
        final-automa (automa/confirm-worker-placement automa)
        next-maj-imp (first (:major-improvements game))
        maj-imp?     (= action automa/major-improvement)
        cond-pop-imp (if maj-imp? pop-major-improvements identity)
        log-entry    (cond-> {:automa automa
                              :action action
                              :time   (js/Date.)}
                       maj-imp? (assoc :major-improvement next-maj-imp))]
    (-> game
        cond-pop-imp
        (assoc-in automa-path final-automa)
        (update :blocked-spaces conj action)
        (update :log conj log-entry))))

(defn block-worker-placement
  ([game automa-uuid]
   (block-worker-placement game automa-uuid true))
  ([game automa-uuid persist-blocked?]
   (let [automa-path  [:automas automa-uuid]
         automa       (get-in game automa-path)
         action       (automa/top-action automa)
         temp-game    (update game :blocked-spaces conj action)
         final-automa (iterate-block-worker-placement temp-game automa)
         final-game   (if persist-blocked?
                        temp-game
                        game)]
     (assoc-in final-game automa-path final-automa))))

(defn skip-worker-action
  [game automa-uuid]
  (block-worker-placement game automa-uuid false))