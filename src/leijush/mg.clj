(ns leijush.mg
  (:use [leijush.core] [clojure.contrib.math])) ; took out this line as require

(def *capacity-list* (filter odd? (range 20)))
(def *popsize* 10)
(def *rounds-num* 5)

(defstruct player :number :choices :payoffs)

;;; this is where player logic is inserted
;;; player decisions is a list of the player's past decisions
;;; all-decisions is a list of all past decisions, including the player
(defn player-logic [player-decisions all-decisions] 
  "random player logic"
  (rand-int 2))

(defn create-players [popsize]
  "create the initial struct of players with empty keys"
  (for [x (range 0 (inc popsize))]
	(struct-map player :number x :choices [] :payoffs [])))

(defn get-decisions
  "returns a list of all player decisions for the past round"
  [playerlist]
  (map #(last (% :choices)) playerlist))

(defn get-all-decisions
  "returns a list of all decisions for all players (before current round)"
  [playerlist]
  (map #(% :choices) playerlist))

(defn get-player-decisions
  "returns a list of individual player's past decisions"
  [playernum playerlist]
  (nth (get-all-decisions playerlist) playernum))

(defn payoff-sum
  "sum the player decisions with proper weights"
  [decisions capacity]
  (* capacity
     (apply + decisions)))			; integrate other weights 

(defn apply-payoff			
  "add the payoff to each player"
  [payoff player-struct]
  (if (= (last (player-struct :choices)) 0)
    (update-in player-struct [:payoffs] conj 1)
    (update-in player-struct [:payoffs] conj payoff)))

(defn calculate-payoff
  "returns list of players with payoff applied"
  [playerlist capacity]
  (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
    (map #(apply-payoff payoff %) playerlist)))

(defn player-decide
  "player decide working"
  [player-structs]
  (let [past-decisions (get-all-decisions player-structs)]
    (map #(update-in % [:choices] conj (player-logic (:choices %) past-decisions)) player-structs)))

(defn play-rounds
  "function to play rounds. returns list of player structs"
  [roundnum capacity]
  (cond
   (zero? roundnum) (create-players 8)
   :else (calculate-payoff
	  (player-decide
	   (play-rounds (dec roundnum) capacity))
	  capacity)))

(defn game
  "returns list of players with payoffs and choices in list of rounds"
  []
  (for [x *capacity-list*]
    (play-rounds *rounds-num* x)))

;; (defn game
;;   "returns list of players with payoffs and choices for rounds"
;;   [roundnum]
;;   (loop [result [] x *capacity-list*]
;;     (if (zero? x)
;;       result
;;       (recur (conj result (play-rounds 7)) (dec x)))))

;;;;;;;;;;
;; push ;;
;;;;;;;;;;

;; (define-registered in 
;;   (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

;; (pushgp 
;;   :error-function (fn [program]
;;                     (doall
;;                       (for [input (range 1 6)]
;;                         (let [state (run-push program
;;                                       (push-item input :auxiliary
;;                                         (push-item input :integer
;;                                           (make-push-state))))
;;                               top-int (top-item :integer state)]
;;                           (if (number? top-int)
;;                             (abs (- top-int (factorial input)))
;;                             1000000000))))) ;; big penalty, since errors can be big
;; 	 :atom-generators (concat (registered-for-type :integer)
;;                      (registered-for-type :exec)
;;                      (registered-for-type :boolean)
;;                      (list (fn [] (rand-int 100))
;;                        'in))
;; 	 :max-points 100
;; 	 :population-size 5000
;; 	 :trivial-geography-radius 10)

;; (defn game
;;   "returns player values for a game"
;;   []
;;   (for [a (range 1 rounds)]
;;     )
;;   )

;; (defn round
;;   "returns list of players with payoffs and choices for up to round"
;;   [playerlist roundnum]
;;   (put-payoff
;;    (calculate-payoff
;;     (get-decisions
;;      (play-round roundnum)))
;;    playerlist))

;; http://techbehindtech.com/2010/06/25/parsing-xml-in-clojure/
;; http://stackoverflow.com/questions/4641964/how-to-use-update-in-in-clojure
;; above is update-in
;; http://java.ociweb.com/mark/clojure/article.html
;; martin kaiser - efficiently representing populations in genetic programming 
;; todo
;;; build in changing capacity [just mod total number of rounds or tmi?]
;;; build game around it