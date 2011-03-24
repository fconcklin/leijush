(ns leijush.mg
  (:require [leijush.core] [clojure.contrib.math])
  (:use [leijush.core] [clojure.contrib.math]))

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

;; http://techbehindtech.com/2010/06/25/parsing-xml-in-clojure/
;; http://stackoverflow.com/questions/4641964/how-to-use-update-in-in-clojure
;; above is update-in
;; http://java.ociweb.com/mark/clojure/article.html
;; martin kaiser - efficiently representing populations in genetic programming 
;; todo
;;; build in changing capacity [just mod total number of rounds or tmi?]
;;; write struct alteration helper fn's 
;;; apply-payoff, payoff-sum, get-decisions
;;; build game around it

(def *popsize* 10)
(def *roundsnum* 5)

(defstruct player :number :choices :payoffs)

(defn rounds
  "returns list of players with payoffs and choices for rounds"
  [roundnum]
   (calculate-payoff			; move into (play-round)
    (play-round roundnum)))

; old 					;  (let [payoff (payoff-sum (apply + (map last playerlist)))]
(defn calculate-payoff
  "returns list of players with payoff applied"
  [playerlist]
  (let [payoff (payoff-sum (get-decisions playerlist))]
    (map #(apply-payoff payoff %) playerlist)))

;;; for each struct, give it a payoff based on whether or not it entered in last round
;;; use merge-with a keyword
;;; figure out how to merge back in / maybe atom instead?
(defn apply-payoff			
  "add the payoff to each player"
  [payoff player-struct]
  nil
  )

(defn payoff-sum
  "sum the player decisions with proper weights"
  [decisions]				; takes list of decisions
  nil
  )

(defn get-decisions
  "returns a list of all player decisions for the past round"
  [playerlist]
  nil
  )

;;; this is a recursive function that plays out the rounds
(defn play-round
  "function to play a round. returns list of player structs"
  [roundnum]
  ;; should I move calculate payment down here?
   (if (zero? roundnum)
     (create-players 8)			; put in popsize
     (map #(player-decide %) (play-round (- roundnum 1))))) ; put calculate-payoff here

(defn player-logic []
  "random player logic"
  (rand-int 2))

;; this is also where push players will go
(defn create-players [popsize]
  "create the initial struct of players"
  (for [x (range 1 popsize]
	(struct-map player :number x))))

;; this is where we'll be pulling off int or bool stackfrom push
(defn player-decide [player-struct]
  "returns player with decision in :choices key"
  ;; assoc player logic with choices keyword
  (player-logic))

;; (for [x (range 0 popsize)] ;; declist
;;   (player-decide x))

;; (merge-with cons
;; 	    (:choices plr-list)
;; 	    declist)
  
(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (range 1 6)]
                        (let [state (run-push program
                                      (push-item input :auxiliary
                                        (push-item input :integer
                                          (make-push-state))))
                              top-int (top-item :integer state)]
                          (if (number? top-int)
                            (abs (- top-int (factorial input)))
                            1000000000))))) ;; big penalty, since errors can be big
	 :atom-generators (concat (registered-for-type :integer)
                     (registered-for-type :exec)
                     (registered-for-type :boolean)
                     (list (fn [] (rand-int 100))
                       'in))
	 :max-points 100
	 :population-size 5000
	 :trivial-geography-radius 10)

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