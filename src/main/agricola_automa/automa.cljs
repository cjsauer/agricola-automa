(ns agricola-automa.automa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Board and automa data

(def round->stage
  [1 1 1 1
   2 2 2
   3 3
   4 4
   5 5
   6])

(def meeting-place
  {:space "Meeting Place"
   :stage 0
   :skip-message "Automa is already starting player"})

(def skippable-spaces
  #{meeting-place})

(def round-actions
  (conj
   [{:space "Farm Expansion"
     :stage 0}
    {:space "Grain Seeds"
     :stage 0}
    {:space "Farmland"
     :stage 0}
    {:space "Lessons"
     :stage 0}
    {:space "Day Laborer"
     :stage 0}
    {:space "Forest"
     :stage 0}
    {:space "Clay Pit"
     :stage 0}
    {:space "Reed Bank"
     :stage 0}
    {:space "Fishing"
     :stage 0}
    {:space "Fencing"
     :stage 1}
    {:space "Sheep Market"
     :stage 1}
    {:space "Grain Utilization"
     :stage 1}
    {:space "Major Improvement"
     :stage 1}
    {:space "Basic Wish for Children"
     :stage 2}
    {:space "House Redevelopment"
     :stage 2}
    {:space "Western Quarry"
     :stage 2}
    {:space "Pig Market"
     :stage 3}
    {:space "Vegetable Seeds"
     :stage 3}
    {:space "Cattle Market"
     :stage 4}
    {:space "Eastern Quarry"
     :stage 4}
    {:space "Urgent Wish for Children"
     :stage 5}
    {:space "Cultivation"
     :stage 5}
    {:space "Farm Redevelopment"
     :stage 6}]
   meeting-place))

(def automa-plans
  [{:stage 1
    :actions [1 1]}
   {:stage 1
    :actions [1 0]}
   {:stage 1
    :actions [0 0]}
   {:stage 1
    :actions [0 1]}
   {:stage 2
    :actions [1 1 1]}
   {:stage 2
    :actions [1 1 2]}
   {:stage 2
    :actions [1 2 1]}
   {:stage 2
    :actions [2 1 1]}
   {:stage 3
    :actions [1 1 1 2]}
   {:stage 3
    :actions [1 1 2 3]}
   {:stage 3
    :actions [1 2 3 1]}
   {:stage 3
    :actions [2 3 1 1]}
   {:stage 3
    :actions [3 1 1 1]}
   {:stage 4
    :actions [1 1 1 2 3]}
   {:stage 4
    :actions [1 1 2 3 4]}
   {:stage 4
    :actions [1 2 3 4 1]}
   {:stage 4
    :actions [2 3 4 1 1]}
   {:stage 4
    :actions [3 4 1 1 1]}
   {:stage 4
    :actions [4 1 1 1 2]}
   {:stage 5
    :actions [1 1 1 2 3]}
   {:stage 5
    :actions [1 1 2 3 4]}
   {:stage 5
    :actions [1 2 3 4 5]}
   {:stage 5
    :actions [2 3 4 5 1]}
   {:stage 5
    :actions [3 4 5 1 1]}
   {:stage 5
    :actions [4 5 1 1 1]}
   {:stage 5
    :actions [5 1 1 1 4]}
   {:stage 6
    :actions [1 1 1 2 3]}
   {:stage 6
    :actions [1 1 2 3 4]}
   {:stage 6
    :actions [1 2 3 4 5]}
   {:stage 6
    :actions [2 3 4 5 1]}
   {:stage 6
    :actions [3 4 5 1 1]}
   {:stage 6
    :actions [4 5 1 1 1]}
   {:stage 6
    :actions [5 1 1 1 2]}])

(defn shuffle-stage-decks
  [cards stage-range]
  (mapv
   (fn [stage]
     (shuffle
      (filter #(= stage (:stage %)) cards)))
   stage-range))

(defn shuffle-automa-plan-decks
  [automa-plans]
  (shuffle-stage-decks automa-plans (range 1 7)))

(defn shuffle-automa-action-decks
  [automa-actions]
  (shuffle-stage-decks automa-actions (range 0 7)))

(defn draw-card
  [deck]
  [(first deck)
   (subvec deck 1)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automa state

(defn make-automa
  []
  {:uuid         (random-uuid)
   :plan-decks   (shuffle-automa-plan-decks automa-plans)
   :action-decks (shuffle-automa-action-decks round-actions)})

(defn draw-plan-card
  [automa current-round]
  (let [stage            (round->stage (dec current-round))
        stage-idx        (dec stage)
        decks            (:plan-decks automa)
        deck             (nth decks stage-idx)
        [plan-card deck] (draw-card deck)
        worker-cnt       (count (:actions plan-card))
        new-plan         (assoc plan-card :total-workers worker-cnt)
        new-deck         (conj deck plan-card)
        new-decks        (assoc decks stage-idx new-deck)]
    (assoc automa
           :plan new-plan
           :plan-decks new-decks)))

(defn has-actions-for-stage?
  [automa stage]
  (not-empty (nth (:action-decks automa) stage)))

(defn draw-action-card
  [automa stage]
  (let [deck              (nth (:action-decks automa) stage)
        [action new-deck] (draw-card deck)
        action-stack      (or (:action-stack automa) '())
        new-stack         (conj action-stack action)]
    (-> automa
        (assoc :action-stack new-stack)
        (assoc-in [:action-decks stage] new-deck))))

(defn pickup-worker
  [automa]
  (let [plan-actions      (-> automa :plan :actions)
        next-action-stage (first plan-actions)
        new-plan-actions  (subvec plan-actions 1)]
    (if next-action-stage
      (-> automa
          (assoc-in [:plan :actions] new-plan-actions)
          (draw-action-card next-action-stage))
      automa)))

(defn confirm-worker-placement
  [automa]
  (if-let [action-stack (:action-stack automa)]
    (reduce (fn [automa action-card]
              (let [deck-path [:action-decks (:stage action-card)]]
                (update-in automa deck-path conj action-card)))
            (dissoc automa :action-stack)
            (reverse action-stack))
    automa))

(defn surface-meeting-place-card
  [automa]
  (let [farm-deck  (first (:action-decks automa))
        split-deck (group-by #(= % meeting-place) farm-deck)
        card       (first (get split-deck true))
        rdeck      (seq (get split-deck false))
        new-deck   (if card
                     (vec (conj rdeck card))
                     farm-deck)]
    (assoc-in automa [:action-decks 0] new-deck)))

(defn block-worker-placement
  [automa]
  (let [action-stack (:action-stack automa)
        last-action  (first action-stack)
        stage        (:stage last-action)]
    (if (and (zero? stage)
             (not (:starting-player? automa))
             (not= last-action meeting-place))
      (draw-action-card (surface-meeting-place-card automa) stage)
      (if (has-actions-for-stage? automa stage)
        (draw-action-card automa stage)
        (draw-action-card automa 0)))))

(defn workers-remaining
  [automa]
  (or (-> automa :plan :actions count)
      0))

(defn top-action
  [automa]
  (-> automa :action-stack first))

(defn desired-action-space
  [automa]
  (:space (top-action automa)))

(defn all-turns-completed?
  [automa]
  (and (:plan automa)
       (empty? (get-in automa [:plan :actions]))
       (nil? (:action-stack automa))))
