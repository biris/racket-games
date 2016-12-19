
(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-client)


(define ClientState0  "no guess available")

(define TEXT-SIZE 11)
(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))
(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define MT-SC 
  (place-image/align 
   HELP-TEXT TEXT-X TEXT-UPPER-Y 
   "left" "top" 
   (place-image/align 
    HELP-TEXT2 
    TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))



(define (launch-guess-client n host)
  (big-bang ClientState0
            (on-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register host)
            (on-receive handle-msg)))



(define (handle-keys w key)
  (cond [(key=? key "up")   (make-package w "up")]
        [(key=? key "down") (make-package w "down")]
        [(key=? key "q")    (stop-with w)]
        [(key=? key "=")    (stop-with w)]
        [else w]))

(define (handle-msg c msg)
  (number->string msg))

(define (draw-guess c)
  (overlay (text c SIZE COLOR) MT-SC))


