(struct join (clients [time #:mutable]) #:transparent)
(struct play (players food spectators) #:transparent #:mutable)


;; (struct ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
(define-values 
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player) #:transparent)
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values 
     create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))





