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

(define TARGET (circle (- (/ w 2) 2) 'outline 'blue))


;; string constants
(define STRENGTH "strength")
(define AGILITY "agility")
(define HEALTH "health")
(define LOSE  "YOU LOSE")
(define WIN "YOU WIN")
(define DEAD "DEAD")
(define REMAINING "Remaining attacks ")
(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1


;; IMAGES

;; HYDRA-IMAGE

;; ORC-IMAGE
;; HYDRA-IMAGE
;; SLIME-IMAGE
;; BRIGAND-IMAGE

;; INSTRUCTION-TEXT-SIZE
;; ATTACK-COLOR

;; GUI


;; V-SPACER
;; H-SPACER

(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))


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
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (message str)
  (text str MESSAGE-SIZE MESSAGE-COLOR))


(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monsters (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (i-monster
                  V-SPACER V-SPACER V-SPACER
                  additional-text)
                 H-SPACER)
         V-SPACER))

(define (render-player p)
  (above/align
   "left"
   (status-bar (player-strength p) MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar (player-agility p) MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar (player-health p) MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))


(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))
  (define bar (overlay/align 'left 'top f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

;; [Listof Monster] [Opt Nat] -> Image
;; add all monsters on lom, including status bar
;; label the target unless it isn't called for
(define (render-monsters lom with-target)
  ;; the currently targeted monster (if needed)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))

  ;; Monster -> Image
  (define (render-one-monster m)
    (define image
      (if (eq? m target)
          (overlay TARGET (monster-image m))
          (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))

  (arrange (map render-one-monster lom)))


(define (arrange lom)
  (cond ((empty? lom) empty-image)
        (else (define r (take lom PER-ROW))
              (above r (arrange (drop lom PER-ROW))))))


(define (win? w)
  (all-dead? (orc-world-lom w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (player-dead? p)
  (or (= (player-health 0))
      (= (player-strength 0))
      (= (player-agility 0))))

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (monster-alive? m)
  (> (monster-health m) 0))




