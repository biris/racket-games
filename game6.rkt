(require 2htdp/image 2htdp/universe)

;; constatns

;; HEAD-IMG
;; GOO-IMG
;; SEG-IMG
;; MT-SCENE
;; ENDGAME-TEXT-SIZE


;; data representation

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)

(struct posn (x y) #:transparent)

(struct goo (loc expire) #:transparent)

;; functions

(define snake-example
  (snake "up" (list (posn 1 1) (posn 1 2) (posn 1 3))))

(define goo-example
  (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))

(define pit-example
  (pit snake-example goo-example))


(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (can-eat snake goos)
  (cond ((empty? goos) #f)
        (else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos))))))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))


(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))


;; slithering

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (cond ((empty? (rest segs)) empty)
        (#t
         (cons (first segs) (all-but-last (rest segs))))))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond ((string=? "up" head) (posn-move head 0 -1))
        ((string=? "down" head) (posn-move head 0 1))
        ((string=? "right" head) (posn-move head 1 0))
        ((string=? "left" head) (posn-move head -1 0))))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (rot goos)
  (cond ((empty? goos) empty)
        (else (cons (decay (first goo)) (rot (rest goos))))))

(define (decay goo)
  (goo (goo-loc goo) (sub1 (goo-expire goo))))

(define (age-goo goos)
  (rot (renew goos)))

(define (renew goos)
  (cond ((empty? goos) empty)
        ((rotten? (first goo))
         (cons (fresh-goo) (age-goo (rest (goo)))))
        (#t
         (cons (first goo) (age-goo (rest goo))))))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       (add1 (random EXPIRATION-TIME)))) ;; we add 1 to avoid 0

(define (direct-snake w ke)
  (cond ((dir? ke) (world-change-dir w ke))
        (else w)))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond ((and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and  at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w))
        (else
         (pit (snake-change-dir the-snake d) (pit-goos w)))))


(define snake-going-left (snake "left" (list (posn 2 18))))
(define plain-world (pit snake-going-left empty))

(define (opposite-dir? d1 d2)
  (cond ((string=? d1 "up") (string=? d2 "down"))
        ((string=? d1 "up") (string=? d2 "down"))
        ((string=? d1 "up") (string=? d2 "down"))
        ((string=? d1 "up") (string=? d2 "down"))))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define (snake-body-scene
           (img-list+scene (snake-body snake) SEG-IMG scene)))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond ((string=? dir "up") HEAD-UP-IMG)
                   ((string=? dir "down") HEAD-DOWN-IMG)
                   ((string=? dir "left") HEAD-LEFT-IMG)
                   ((string=? dir "right") HEAD-RIGHT-IMG))
             snake-body-scene))

(define (img-list+scene posns img scene)
  (cond ((empty? posns) scene)
        (else (img+scene (first posns)
                         img
                         (img-list+scene (rest posns) img scene)))))

(define (img+scene pons img scene)
  (place-image img
               (* (posn-x ponsn) SEG-SIZE)
               (* (posn-y ponsn) SEG-SIZE)
               scene))

(define (goo-list-scene goos scene)
  (define (get-pons-from-goo goos)
    (cond ((empty? goos) empty)
          (else (cons (goo-loc (first goo))
                      (get-posns-from-goo (rest goo))))))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))


(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-snake-world w)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-collding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-x (snake-head snake)))
  (or (= x 0) (= x SIZE)
      (= y 0) (= y SIZE)))



