(ns leijush.mg
  (:use [leijush.core] [clojure.contrib.math]))

;;;;;;;;;;;;;
;; globals ;;
;;;;;;;;;;;;;

(def *capacity-list* (filter odd? (range 20)))
(def *popsize* 10)
(def *rounds-num* 5)

(defstruct player :number :choices :payoffs :capacity :code)

(def reggie1 @registered-instructions)


;;;;;;;;;;
;; game ;;
;;;;;;;;;;

;;; this is where player logic is inserted
;;; player decisions is a list of the player's past decisions
;;; all-decisions is a list of all past decisions, including the player

;;; see strategies section for options

(defn push-wrapper
  "sanitizes push return"
  [c]
  (cond
   (nil? c) 0
   (neg? c) 0
   (odd? c) 0
   (even? c) 1))

(defn push-strat
  "player logic for push"
					;  [player-decisions all-decisions push-code]
  [& args]
  ;; push player-decisions onto stack (which)
  ;; push all-decisions onto stack (which?)
  ;; eval push code here
;  (let [reggie @registered-instructions]
    (push-wrapper (first (:integer (run-push
				    (random-code 10 reggie1)
				    (->>
				     (make-push-state)
				     (push-item 1 :integer)
				     (push-item 0 :integer)))))))

(defn player-logic [player-decisions all-decisions]
  "random player logic"
  (rand-int 2)
;  (push-strat)
  )				; just for testing until strategies/push implemented

(defn create-players [popsize capacity]
  "create the initial struct of players with empty keys"
  (for [x (range 0 (inc popsize))]
	(struct-map player :number x :choices [] :payoffs [] :capacity capacity)))

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
  (+ 1 (* 2 (- capacity
	       (apply + decisions)))))			; integrate other weights 

(defn apply-payoff			
  "add the payoff to each player"
  [payoff player-struct]
  (if (= (last (player-struct :choices)) 0)
    (update-in player-struct [:payoffs] conj 1) ; do these better
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
   (zero? roundnum) (create-players *popsize* capacity)
   :else (calculate-payoff
	  (player-decide
	   (play-rounds (dec roundnum) capacity))
	  capacity)))

(defn game
  "returns list of players with payoffs and choices in list of rounds"
  []
  (vec (flatten
   (for [x *capacity-list*]
    (play-rounds *rounds-num* x)))))

(defn scores-map
  "return this players with their payoff scores for game"
  []
  (let [data (game)]
    (for [x (range *popsize*)]
      (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) data))))))

(defn winner-map
  "returns the winners of the game sorted by payoffs. Key is player number, value is payoff total"
  []
  (sort-by last >
	   (let [data (game)]
	     (for [x (range *popsize*)]
	       [(keyword (str x)) (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) data)))])))) ; messy

;;;;;;;;;;;;;;;;
;; strategies ;;
;;;;;;;;;;;;;;;;

;;; in strategies, don't vary the capacity but keeping same round number

;; (define-registered decision-info-me
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item (right-in astate) :auxiliary)))))




(defn strat-1 [] "entry strategy" 1)
(defn strat-0 [] "stay-out strategy" 0)

;; probabilistic entry choice rules

;;; linear choice rule
(defn entry-prob-lin
  "linear choice rule for probability of agent entry"
  [strat-1 strat-0]
  (/ strat-1 (+ strat-1 strat-0)))

;;; exponential choice rule
(defn entry-prob-exp
  "exponential choice rule for probability of agent entry"
  []
  nil)

;; learning models

;;; simple reinforcement
(defn entry-lm-sr
  "returns probabilities for simple reinforcement"
  []
  ;; recursive
  ;; p6 of paper
  nil)

;;; hypothetical reinforcement
(defn entry-lm-hr
  "returns probabilities for hypothetical reinforcement"
  [all-decisions]			; past-decisions as well?
  ;; recursive
  ;; p6 of paper
  nil)

(defn entry-prob-sfp
  "stochastic fictitious play"		; combine exp prob and entry hr
  []
  nil)

;; stochastic approximation (p.6)
(defn stoch-approx
  "calculates expected motion of player's strategy adjustment"
  []
  nil)

;;;;;;;;;;;;;;;;;;;;;;
;; equilibria tests ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn eq-nash-pure?
  "returns whether game is pure Nash equilibrium"
  []
  nil)

(defn eq-nash-sme?
  "returns whether game is symmetric mixed Nash equilibrium"
  []
  nil)

(defn eq-nash-asm?
  []
  "returns whether game is asymmetric mixed equilibria"
  nil)

;; random push code
(random-code 100 (concat @registered-instructions
                             (list (fn [] (lrand-int 100))
				   (fn [] (lrand)))))

(run-push '(1 1 integer_add integer_add) (->>
					  (make-push-state)
					  (push-item 3 :integer)
					  (push-item 7 :integer)))

(first (:integer (run-push (random-code 100 @registered-instructions) (->>
								       (make-push-state)
								       (push-item 1 :integer)
								       (push-item 0 :integer)))))

(repeatedly 5 #(push-strat))

(repeatedly 10 #(random-code 10 @registered-instructions)) ; generate random players 
   
	     
;;;;;;;;;;
;; push ;;
;;;;;;;;;;

;; (define-registered out
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item 0 :auxiliary)))))

;; (define-registered in
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item 0 :auxiliary)))))

;; (defn gp-start
;;   []
;;   (pushgp
;;    :error-function (fn [program]
;; 		     (map #(- 3000 %) (scores-map))))
;;   :atom-generators (concat
;; 		    (registered-for-type :integer)
;; 		    (registered-for-type :exec)
;; 		    (registered-for-type :boolean)
;; 		    (list
;; 		     'boolean_and
;; 		     'boolean_not
;; 		     'boolean_or))
;;   :error-threshold 40
;;   :reproduction-simplifications 10)

;;;;;;;;;;
;; push ;;
;;;;;;;;;;

;; http://techbehindtech.com/2010/06/25/parsing-xml-in-clojure/
;; http://stackoverflow.com/questions/4641964/how-to-use-update-in-in-clojure
;; above is update-in
;; http://java.ociweb.com/mark/clojure/article.html
;; maarten kaijser - efficiently representing populations in genetic programming 
;; todo
;;; build in changing capacity [just mod total number of rounds or tmi?]
;;; build game around it
;;; jason nobel richard watson -
;;; juxt - http://richhickey.github.com/clojure/clojure.core-api.html#clojure.core/juxt