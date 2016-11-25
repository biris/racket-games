;; world

(struct orc-world (player lom attacks) #:transparent)

(struct player (health agility strength) #:transparent #:mutable)

(struct monster (health) #:transparent #:mutable)

(struct orc monster (club) #:transparent)
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

;; REPL

;(orc MONSTER-HEALTH0 (random (add1 CLUB-STRENGTH)))
;(define my-orc (orc MONSTER-HEALTH0 2))











