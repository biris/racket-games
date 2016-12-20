(define CUPCAKE 15)
(define PLAYER-SIZE (* 3 CUPCAKE)) 
(define PLAYER-FATTEN-DELTA 5)


(provide             ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
 (struct-out player) ;; 
 (struct-out body)
 get-score ;; 
 PLAYER-FATTEN-DELTA)


(struct player (id body waypoints) #:prefab) ;; waypoints? 

(struct body (size loc) #:prefab #:mutable)


(define (get-score f) ; from size to score!
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))




