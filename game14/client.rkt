;; constatnts

(define PLAYER-COLOR "red")
(define MY-COLOR "blue")
;; PLAYER-IMG


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



