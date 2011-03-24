(ns leijush.gp1
  (:require [leijush.core] [clojure.contrib.math])
  (:use [leijush.core] [clojure.contrib.math]))

(define-registered count_bool
  (fn [state]
    (if (and (not (empty? (rest (:boolean state)))) (not (empty? (rest (:integer
									state)))))
      (let [player (stack-ref :integer 0 state)
            moves (stack-ref :boolean player state)]
        (push-item (count moves) :integer (pop-item ))
	state))))

(defn genetic-runner
  ;Runs computer strategy
  [player tournament myHistory cap]
  (stack-ref :integer 0
     (run-push player
       (push-item tournament :boolean
		  (push-item myHistory :auxiliary
			     (push-item cap :integer
					(make-push-state)))))))

(defn payOff
  ; a function which returns the payoff for each of the players
  [round cap]
  (loop [pay (list)
         counter 0]
    (if (= (count pay) (count round))
      pay
      (recur
        (cons
          (if (= (nth round counter) 0)
            1
            (+ 1 (* 2 (- cap (apply + round)))))
          pay)
        (inc counter)))))

(defn runChoose
  ; returns a move by handing the player past games and current game
  [player tournament myHistory roundsPerGame]
  (player tournament myHistory roundsPerGame))

(defn runRound
  ; makes a round by cons-ing runChoos recusively
  [tournament players cap]
  (let [z (genetic-runner (first players) (second tournament) (first (first
								      tournament)) cap)
        x
        (if (= z 0) 0 1)]
    (cons
     x
     (pmap
      (fn [x y]
	(runChoose
	 x
	 (second tournament)
	 y
	 cap))
      (rest players)
      (rest (first tournament))))))

(defn listMaker
  [size]
  (loop [lisT (list)]
    (if (= (count lisT) (inc size))
      lisT
      (recur (cons (list) lisT)))))

(defn runGame
  ; makes a game by cons-ing runRound recursively
  [tournament gamePertournament players]
  (loop [game (list (listMaker (count players)) (list))
         counter 0]
    (if (= gamePertournament counter)
      (list
        (pmap cons (first game) (first tournament))
        (cons (second game) (second tournament)))
      (let [acts (runRound tournament players (inc (* 2 counter)))
            pay (payOff acts (inc (* 2 counter)))]
        (recur
          (list
            (pmap cons pay (first game))
          (cons acts (second game)))
          (inc counter))))))

(defn runTourn
  [numOftournament gamePertournament players]
  (loop [tournament (list (listMaker (count players)) (list))
         counter 0]
    (if (= counter numOftournament)
      tournament
      (recur (runGame tournament gamePertournament players) (inc counter)))))

(defn strategy1
  ; A sample strategy
  [touny history cap]
  (rand-int 2))

(defn strategy2
  ; A sample strategy
  [touny history cap]
  (if (< (inc (rand-int 20)) cap)
    1
    0))

(defn getScore
  [payOffs]
  (pmap
    (fn [x]
        (apply + x))
    payOffs))

(defn run
  [error strat]
  (pushgp
    :error-function (fn [program]
                      (doall
		       (map
			(fn [x]
			  (- 50 x))
			(getScore
			 (first
			  (first
			   (runTourn 10 10
				     (cons program
					   (loop [players (list)]
					     (if (= (count players) 19)
					       players
					       (recur (cons strat players))))))))))))
    :atom-generators (concat
                       (registered-for-type :integer)
                       (registered-for-type :exec)
                       (registered-for-type :boolean)
                       (registered-for-type :code)
                       (registered-for-type :auxiliary)
                       (registered-for-type :tag)
                       (registered-for-type :zip)
                       (registered-for-type :float)
                       (list
                         'boolean_and
                         'boolean_not
                         'boolean_or
                         'integer_rand
                         'boolean_rand
                         'exec_if
                         'code_if
     ;                    'count_bool
                         ))
    :error-threshold error
    :reproduction-simplifications 15
;    :mutation-probability 0.4
;    :crossover-probability 0.4
;    :simplification-probability 0.1
))

(defn playgame
  [program strat]
  (runTourn 10 10
    (cons program
      (loop [players (list)]
        (if (= (count players) 19)
          players
          (recur (cons strat players)))))))

(run 40 strategy2)

;'(28 20 16 16 20 22 18 8 22 10)
;(getScore (first (first (playgame '(code_if integer_sub (exec_eq integer_dup
(integer_min)) (boolean_stackdepth boolean_yank (boolean_pop integer_eq)
(boolean_rand)) (exec_noop ((integer_rand) boolean_shove (exec_eq exec_s)
integer_mod) (boolean_and (integer_div integer_pop (integer_div) (exec_swap))
(boolean_and exec_stackdepth (integer_dup))) boolean_or boolean_shove)
integer_eq exec_stackdepth) strategy2))))
(first (second (playgame '(code_if integer_sub (exec_eq integer_dup
(integer_min)) (boolean_stackdepth boolean_yank (boolean_pop integer_eq)
(boolean_rand)) (exec_noop ((integer_rand) boolean_shove (exec_eq exec_s)
integer_mod) (boolean_and (integer_div integer_pop (integer_div) (exec_swap))
(boolean_and exec_stackdepth (integer_dup))) boolean_or boolean_shove)
integer_eq exec_stackdepth) strategy2)))