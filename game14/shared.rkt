(provide ;;;;;;;;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;
         (struct-out player) ;; 
         (struct-out body))


(struct player (id body waypoints) #:prefab)

(struct body (size loc) #:prefab #:mutable)



