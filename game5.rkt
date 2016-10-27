(require 2htdp/image 2htdp/universe)

(define xxx 3)

(define IMAGE-OF-UFO (circle 10 "solid" "green"))

(define COLOR "red")
(define SIZE 72)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define WIDTH 200)
(define HEIGHT 200)

(define MT-SC 
  (place-image/align 
   HELP-TEXT TEXT-X TEXT-UPPER-Y 
   "left" "top" 
   (place-image/align 
    HELP-TEXT2 
    TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))

(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))

;; GuessRange -> Scene
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

;;
(define render-last-scene w
  (overlay (text "End" SIZE COLOR) MT-SC))

(define single? w
  (= (interval-samll w) (interval-big w)))

(define (start lower upper)
  (big-bang (interval lower uper))
  (on-key deal-with-guess)
  (to-draw render)
  (stop-when single? render-last-scene))






;; our data defintion 
(struct interval (small big))


;; GuessRange Key -> GuessRange
;; Handles key input

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [( or (key=? key "q") (key=? key "=")) (stop-with w)]
        [else w]))



(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w)
                 (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))



;; interval -> number 
(define (guess w)
  (quotient (quotient (+ (interval-small w) (interval-big w)) 2)))



(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-ufo-onto-an-empty-scene state)
  (place-image IMAGE-OF-UFO (/ WIDTH 2) state
               (empty-scene WIDTH HEIGHT)))







;; (define state-is-HEIGHT
;;   (lambda (state) (>= state  (- HEIGHT (image-height IMAGE-OF-UFO)))))

;; (big-bang 0
;;           (on-tick add-3-to-state)
;;           (to-draw draw-ufo-onto-an-empty-scene)
;;           (stop-when state-is-HEIGHT))



