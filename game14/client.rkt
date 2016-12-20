(struct app (id img countdown) #:transparent) ;; waiting
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
