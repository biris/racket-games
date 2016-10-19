(require 2htdp/image 2htdp/universe)

(define xxx 3)

(define IMAGE-OF-UFO (circle 10 "solid" "green"))

(define WIDTH 200)
(define HEIGHT 200)


(define NEW 33)

(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-ufo-onto-an-empty-scene state)
  (place-image IMAGE-OF-UFO (/ WIDTH 2) state
               (empty-scene WIDTH HEIGHT)))

(define state-is-HEIGHT
  (lambda (state) (>= state  (- HEIGHT (image-height IMAGE-OF-UFO)))))

(big-bang 0
          (on-tick add-3-to-state)
          (to-draw draw-ufo-onto-an-empty-scene)
          (stop-when state-is-HEIGHT))



