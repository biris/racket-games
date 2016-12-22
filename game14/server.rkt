(struct join (clients [time #:mutable]) #:transparent) ;; our waiting state
(struct play (players food spectators) #:transparent #:mutable) ;; our playing state

;; constants

(define FOOD*PLAYERS 5)

(define START-TIME 0) ;; ? why 0

(define JOIN0 (join empty START-TIME)) ;; inint state

(define WAIT-TIME 250)

(define WEIGHT-FACTOR 2.1)


(define BASE-SPEED (/ (expt PLAYER-SIZE 2) WEIGHT-FACTOR))


(define START-TIME 0)

;; (struct ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
(define-values 
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player) #:transparent)
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values 
     create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))

(define (bon-appetit) 
  (universe JOIN0 
            (on-new connect) ;; new connection 
            (on-msg handle-goto-message)
            (on-tick tick-tock TICK)
            (on-disconnect disconnect))) ;; accepts state, iw 


(define (connect s iw)
  (cond [(join? s) (add-player s iw)]
        [(play? s)   (add-spectator s iw)]))


(define (disconnect s iw)
  (cond [(join? s) (drop-client s iw)]                  
        [(play? s) (drop-player s iw)]))

(define (tick-tock s)
  (cond [(join? s) (wait-or-play s)] ;; possibly also transition to play state
        [(play? s) (move-and-eat s)])) ;; possibly also transition to join state

(define (handle-goto-message s iw msg)
  (cond [(and (play? s) (goto? msg)) (goto s iw msg)] ;; add waypoint to player path
        [else (empty-bundle s)])) ;; doesn't change the state

(define (emtpy-bundle s)  
  (make-bundle s empty empty))

;; join state handing events

;; [Universe Player -> Universe] -> [Universe IWorld -> [Bundle Universe]]
;; creates a function that deals with a new connection during join or play phase 
(define (make-connection adder)
  (lambda (u iw)
    (define player (named-player iw)) ;; of type ip (keeps track to iw)
    (define mails  (list (make-mail iw (ip-id player))))
    (make-bundle (adder u player) mails empty)))


;; JoinUniverse Player -> JoinUniverse
(define (join-add-player j new-p)
  (join (cons new-p (join-cients j)) (join-time j)))

(define add-player (make-connection join-add-player))

(define (join-add-player j new-p)
  (join (cons new-p (join-clients j)) (join-time j)))

(define (named-player iw)
  (create-player iw (symbol->string (gensym (iworld-name iw)))))

(define (create-player iw n)
  (ip iw n (create-a-body PLAYER-SIZE) empty))


(define (create-a-body size)
  (define x (+ size (random (- WIDTH size))))
  (define y (+ size (random (- HEIGHT size))))
  (body size (make-rectangular x y)))


(define (drop-client j iw)
  (empty-bundle (join-remove j iw)))


(define (join-remove j iw)
  (join (rip iw (join-clients j)) (join-time j)))


(define (rip iw players)
  (remove iw players (lambda (iw p) (iworld=? iw (ip-iw p)))))

(define (empty-bundle s)
  (make-bundle s empty empty))


;; join state and ticks

(define (wait-or-play j)
  (cond [(keep-waiting? j) (keep-waiting j)]
        [else              (start-game j)]))


(define (keep-waiting? j)
  (or (> PLAYER-LIMIT (length (join-clients j)))
      (> WAIT-TIME (join-time j))))


(define (keep-waiting j)
  (set-join-time! j (+ (join-time j) 1))
  (time-broadcast j))

(define (time-broadcast j)
  (define iworlds (map ip-iw (join-clients j)))
  (define load%   (min 1 (/ (join-time j) WAIT-TIME)))
  (make-bundle j (broadcast iworlds load%) empty))

(define (broadcast iws msg)
  (map (lambda (iw) (make-mail iw msg)) iws))

(define (start-game j)
  (define clients  (join-clients j))
  (define cupcakes (bake-cupcakes (length clients)))
  (broadcast-universe (play clients cupcakes empty)))

(define (bake-cupcakes player#)
  (for/list ([i (in-range (* player# FOOD*PLAYERS))])
    (create-a-body CUPCAKE)))

(define (broadcast-universe p)
  (define mails (broadcast (get-iws p) (serialize-universe p)))
  (make-bundle p mails empty))

(define (get-iws p)
  (map ip-iw (append (play-players p) (play-spectators p))))

(define (serialize-universe p)
  (define serialized-players (map ip-player (play-players p)))
  (list SERIALIZE serialized-players (play-food p))) ;; to send it across the wire

;; play state 

(define (play-add-spectator pu new-s)
  (define players (play-players pu))
  (define spectators (play-spectators pu))
  (play players (play-food pu) (cons new-s spectators)))

(define add-spectator (make-connection play-add-spectator)) ;; add and send mails (serialize)

(define (goto p iw msg)
  (define c (make-rectangular (second msg) (third msg)))
  (set-play-players! p (add-waypoint (play-players p) c iw))
  (broadcast-universe p))

;; [Listof IPs] Complex IWorld -> [Listof IPs]

(define (add-waypoint ps c iw)
  (for/list ([p ps])
    (cond [(iworld=? (ip-iw p) iw)
           (ip (ip-iw p)
               (ip-id p) 
               (ip-body p) 
               (append (ip-waypoints p) (list c)))]
          [else p])))

(define (drop-player p iw)
  (broadcast-universe (play-remove p iw)))

(define (play-remove p iw)
  (define players (play-players p))
  (define spectators (play-spectators p))
  (play (rip iw players) (play-food) (rip iw spectators)))



;; clock tick in play state

(define (move-and-eat pu)
  (define nplayers  (move-player* (play-players pu)))
  (define nfood (feed-em-all nplayers (play-food pu)))
  (progress nplayers nfood (play-spectators pu))) ;; state transitioning 


(define (move-player* players)
  (for/list ([p players])
    (define waypoints (ip-waypoints p))
    (cond [(empty? waypoints) p]
          [else (define body  (ip-body p))
                (define nwpts 
                  (move-toward-waypoint body waypoints)) 
                (ip (ip-iw p) (ip-id p) body nwpts)])))

;; mutate body and give up next waypoints

(define (move-toward-waypoint body waypoints)
  (define goal  (first waypoints))
  (define bloc  (body-loc body))
  (define line  (- goal bloc))
  (define dist  (magnitude line))
  (define speed (/ BASE-SPEED (body-size body)))
  (cond
   [(<= dist speed) 
    (set-body-loc! body goal)
    (rest waypoints)]
   [else                                ; (> distance speed 0)
    (define velocity (* (/ line dist) speed))
     (set-body-loc! body (+ bloc velocity))
    waypoints]))

(define (feed-em-all players foods)
  (for/fold ([foods foods]) ([p players])
    (eat-all-the-things p foods)))


;; effect grow the play 
;; make the cupcake disspear from list 

(define (eat-all-the-things player foods)
  (define b (ip-body player))
  (for/fold ([foods '()]) ([f foods])
    (cond
     [(body-collide? f b)
      (set-body-size! b (+ PLAYER-FATTEN-DELTA (body-size b)))
      foods]
     [else (cons f foods)])))

(define (body-collide? s1 s2)
  (<= (magnitude (- (body-loc s1) (body-loc s2)))
      (+ (body-size s1) (body-size s2))))

(define (progress pls foods spectators)
  (define p (play pls foods spectators))
  (cond [(empty? foods) (end-game-broadcast p)] ;; transition and send off scores
        [else (broadcast-universe p)]))


(define (end-game-broadcast p)
  (define iws (get-iws p))
  (define msg (list SCORE (score (play-players p))))
  (define mls (broadcast iws msg))
  (make-bundle (remake-join p) mls empty))


(define (score ps)
  (for/list ([p ps])
    (list (ip-id p) (get-score (body-size (ip-body p))))))

(define (remake-join p)
  (define players (refresh (play-players p)))
  (define spectators (play-spectators p))
  (join (append players spectators) START-TIME))


(define (refresh players)
  (for/list ([p players])
    (create-player (ip-iw p) (ip-id p))))





