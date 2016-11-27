;; world

(struct orc-world (player lom attack# target) #:transparent)

(struct player (health agility strength) #:transparent #:mutable)

(struct monster (image [health #:mutable]) #:transparent)

(struct slime monster (sliminess) #:transparent)
(struct hydra monster () #:transparent)
(struct brigland monster () #:transparent)

;; player attributes
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

;; monster attributes
(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
(define SLIMINESS 5)
;; game constants

(define PER-ROW 4)
(define MONSTER# 12)

;; IMAGES

;; HYDRA-IMAGE

;; ORC-IMAGE
;; HYDRA-IMAGE
;; SLIME-IMAGE
;; BRIGAND-IMAGE

;; REPL

;(orc MONSTER-HEALTH0 (random (add1 CLUB-STRENGTH)))
;(define my-orc (orc MONSTER-HEALTH0 2))

;; functions

(define (player-update! setter selector delta mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define (player-health+ player delta)
  (player-update! set-player-health! player-health delta MAX-HEALTH))

(define (player-agility+ player delta)
  (player-update! set-player-agility! player-agitlity delta MAX-AGILITY))

(define (player-strength+ player delta)
  (player-update! set-player-strength! player-strength delta MAX-STRENGTH))

;; big-bang

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battles)
            (stop-when end-of-orc-battle? render-the-end)))


(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (end-of-orc-battle? w)
  (or (win? w) (loose? w)))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))


(define (player-acts-on-monsters w k)
  (cond
   ((zero? (orc-world-attack# w) (void))
    ((key=? "s" k) (stab w))
    ((key=? "h" k) (heal w))
    ((key=? "f" k) (flail w))
    ((key=? "e" k) (end-turn w))
    ((key=? "n" k) (initialize-orc-world))
    ((key=? "right" k) (move-target w +1))
    ((key=? "left" k) (move-target w -1))
    ((key=? "down" k) (move-target w (+ PER-ROW)))
    ((key=? "up" k) (move-target w (- PER-ROW)))))
  (give-monster-turn-if-attack#=0 w)
  w)

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))

(define (intitialize-monsters)
  (build-list
   MONSTERS#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGE health (random+ SLIMENESS))]
       [(3) (hydra SLIME-IMAGE health)]))))


(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (string-append REMAINING na))
  (above txt INSTRUCTION-TEXT))

