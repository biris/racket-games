(require 2htdp/image 2htdp/universe)


;; data representation

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)


(struct posn (x y) #:transparent)

(struct goo (loc expire) #:transparent)


(define snake-example
  (snake "up" (list (posn 1 1) (posn 1 2) (posn 1 3))))

(define goo-example
  (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))

(define pit-example
  (pit snake-example goo-example))


(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1))))
            (list (fresh-goo)
                  (fresh-goo)
                  (fresh-goo)
                  (fresh-goo)
                  (fresh-goo)
                  (fresh-goo)
                  (fresh-goo))
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


