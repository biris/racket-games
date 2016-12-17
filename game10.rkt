(define (generate-game-tree board player1 palyer2)
  (define (next-board-tree board0 player not-player)
    (ttt board0 (get-moves board0 player not-player)))
  (define (get-moves board player not-player)
    (define free-fields (get-free-fields board))
    (for/list [(f free-fields)]
      (list (action player f) (next-board-tree board not-player player)))))



(struct dice-world (src board gt))

;; board: listof territories

(struct territory (index player dice x y))

(struct game (board player moves))

(struct move (action gt))

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (to-draw draw-dice-world)
            (stop-when no-more-moves-in-world?
                       draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom)
      new-world))


(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board))
        (= (territory-player t) player))))


(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

;; (or (and (not marked?) (= p-in-focus p))
;;     (and marked? (not (= p-in-focus p))))


;; (if marked (not (= tid player)) (= tid player))


;; (if p1 t1 t2)

;; (or (and p1 t1)
;;     (and (not p1) t2))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

(define (draw-dice n)
  (define first-die  (get-dice-image 0))
  (define height-die (image-height first-die))
  (for/fold ([s first-die]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset  (* height-die (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (color-chooser p)
  (list-ref COLORS p))

(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))



(define (neighbors pos)
  (define top?      (< pos BOARD))
  (define bottom?   (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right?    (zero? (modulo (add1 pos) BOARD)))
  (define left?     (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row  pos top? bottom? right? left?)))p


(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?)    (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top?                (- pos BOARD))
          (add bottom?             (+ pos BOARD))
          (add right?              (add1 pos))
          (add left?               (sub1 pos))))

(define (odd-row pos top? bottom? right? left?)
  (append (add top?               (- pos BOARD))
          (add bottom?            (+ pos BOARD))
          (add (or top? left?)    (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

(define (add b x)
  (if b '() (list x)))
