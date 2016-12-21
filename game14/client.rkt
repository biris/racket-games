;; constatnts

(define PLAYER-COLOR "red")
(define MY-COLOR "blue")
(define SEPERATOR ": ")

(define TEXT-SIZE 20)
(define TEXT-COLOR "black")

(define SCORE-LIST-LENGTH 2)


;; PLAYER-IMG
;; WAYPOINT-COLOR
;; WAYPOINT-NODE
;; FOOD-IMG 
(define END-LENGTH 2)

(define ZERO% 0)

(define INITIAL (app #f LOADING ZERO%))

(define PBAR-COLOR "red")
(define PBAR-HEIGHT 35)

(define PBAR-TEXT (text "loading..." 20 "black"))

(define PBAR-LOC (- HEIGHT PBAR-HEIGHT))

(define PBAR-HEIGHT 35)

(struct app (id img countdown) #:transparent)   ;; waiting
(struct entree (id players food) #:transparent) ;; playing


(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal) ;; view of state
            (on-mouse set-waypoint)  ;; send message
            (on-receive handle-server-messages) ;; recived message, this is where we change the state
            (register server) ;; for connection
            (name label)))

(define (render-the-meal meal)
  (cond [(app? meal)   (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))





(define (handle-server-messages meal msg)
  (cond [(app? meal) (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

;; Number Number MouseEvent -> Meal 

;; sends to the server the message (list GOTO x y) and we don't change the world state 
(define (set-waypoint meal x y event)
  (if (and (entree? meal) (string=? event "button-down"))
      (make-package meal (list GOTO x y))
      meal))


;; appetizer (before the game started)

(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

;; add-progress-bar

(define (add-progress-bar base count)
  (place-image (render-progress count) (/ WIDTH 2) PBAR-LOC base))

;; render-id+image

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
   (cond
     [(boolean? id) base-image]
     [else (define s (string-append LOADING-OPEN-TEXT id))
           (above base-image (text s TEXT-SIZE TEXT-COLOR))])
   BASE))

;; what we do with the message recieved from server 
(define (handle-appetizer-message s msg)
  (cond [(id? msg)    (app msg (app-img s) (app-countdown s))] ;; get an id (in app state)
        [(time? msg)  (app (app-id s) (app-img s) msg)]        ;; update the time (in app state)
        [(state? msg) (switch-to-entree s msg)] ;; (list SERIALZE (list player) (list cupcake))        
        ;; fault tolerant 
        [else s]))

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

(define (switch-to-entree s m)
  (apply entree (app-id s) (rest m)))  ;; we conserve our id (it can be a player or spectator) 

;; 


(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))

(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color
    (if (id=? id (player-id player)) MY-COLOR PLAYER-COLOR))
  (above
   (render-text (player-id player))
   (overlay (render-player-score player) 
            PLAYER-IMG
            (circle size 'outline color))))

(define (render-player-score player)
  (render-text (number->string (get-score (body-size (player-body player))))))

;; get-score (in shared)

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))           
  (add-path id pl (add-players id pl (add-food fd BASE))))


;; add food


(define (add-food foods base-scene)
  (for/fold ([scn base-scene]) ([f foods])
    (place-image FOOD-IMG (body-x f) (body-y f) scn)))


(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))



(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color
    (if (id=? id (player-id player)) MY-COLOR PLAYER-COLOR))
  (above
   (render-text (player-id player))
   (overlay (render-player-score player) 
            PLAYER-IMG
            (circle size 'outline color))))


(define (add-path id players base-scene)
  (define player 
    (findf (lambda (x) (id=? id (player-id x))) players)) ;; is our player-id a true player? 
  (if (boolean? player)
      base-scene
      (add-waypoint* player base-scene)))


;; not yet 
(define (add-waypoint* player base-scene)
  (define loc  (body-loc (player-body player)))
  (define ways (player-waypoints player))
  (define-values (resulting-scene _)
    (for/fold ([scn base-scene][from loc]) ([to ways])
      (values (add-waypoint from to scn) to)))
  resulting-scene)

(define (add-waypoint from to s)
  (define x-from (real-part from))
  (define y-from (imag-part from))
  (define x-to (real-part to))
  (define y-to (imag-part to))
  (define with-line (add-line s x-to y-to x-from y-from WAYPOINT-COLOR))
  (place-image WAYPOINT-NODE x-to y-to with-line))


(define (handle-entree-message s msg)
  (cond [(state? msg) (update-entree s msg)] ;; depending on the message recieved we update our state
        [(score? msg) (restart s msg)]
        [else s])) ;; fault tolerance

;; pretty much the same as siwtch-to-entree

(define (update-entree s state-msg)
  (apply entree (entree-id s) (rest state-msg)))

;; (SCORE ((id number) (id number) ...))
;;; (SERIALIZE (player ...) (cupcake ...))

(define (state? msg)
  (and (list? msg)
       (= UPDATE-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (list? (third msg))
       (symbol=? SERIALIZE (first msg))
       (andmap player? (second msg))
       (andmap body? (third msg))))

;; is our message has shape of score? or state?
(define (score? msg)
  (and (list? msg)
       (= END-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (symbol=? SCORE (first msg))
       (score-list? (second msg)))) ; the second part


(define (score-list? l)
  (for/and ([s l])
    (and (list? s)
         (= SCORE-LIST-LENGTH (length s))
         (id? (first s))
         (number? (second s)))))


(define (restart s end-msg)
  (define score-image (render-scores end-msg))
  (app (entree-id s) (above LOADING score-image) ZERO%)) ; preserve our id

(define (render-scores msg)
  (define scores (sort (second msg) < #:key second))
  (for/fold ([img empty-image]) ([name-score scores])
    (define txt (get-text name-score)) ;; get string representation 
    (above (render-text txt) img))) ;; image representation

(define (get-text name-score)
  (define-values (name score) (apply values name-score)) ;; deconstructing a pair 
  (string-append name SEPERATOR (number->string score)))

(define (render-text txt)
  (text txt TEXT-SIZE TEXT-COLOR))


