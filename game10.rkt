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







