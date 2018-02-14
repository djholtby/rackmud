#lang racket
(require racket/rerequire)
(require racket/unsafe/ops)
(require racket/serialize)
(require racket/undefined)
(require racket/class)
(require racket/async-channel)
(require json)
(require "objects.rkt")
(require "telnet.rkt")
(provide master-object% master-object start-up)

(define master-object #f)



; (startup mudlib master-file master-class db-type db-port db-sock db-srv db-db db-user db-pass) starts the mud racket side running
;    using the specified settings for the mudlib and database connection.  Returns a reference to the master object
; Effects: * Opens database connection
;          * Loads or instantiates master object
;              - this starts the master's event thread, too

(define (start-up mudlib master-file master-class db-type db-port db-sock db-srv db-db db-user db-pass)
  (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-lib-path! mudlib)
  (dynamic-rerequire master-file)
  (define the-master% (dynamic-require master-file master-class))
  (unless (subclass?/mud the-master% master-object%)
    (raise-argument-error 'start-up "master-object%" master-class))
  (get-singleton the-master%))

#|
The Master Object is responsible for saving any server-wide values that need saving.

It also has the methods for establishing, maintaining, and disconnecting telnet connections (Telnet-User structs)

Suggestion:  You'll almost definitely want a HashMap to map Telnet-Users to some kind of mud object
|#

(define master-object%
  (class* mud-object% (tickable<%> listener<%>)
    (super-new)
    ;; (con-connect tel-conn) is called when a new user connects
    ;; on-connect: Telnet-User -> Void
    (define/public (on-connect tel-conn) (void))

    ;; (on-disconnect tel-conn) is called when user tel-conn has disconneted 
    ;; on-disconnect: Telnet-User -> Void
    (define/public (on-disconnect tel-conn) (void))


    ;; (disconnect tel-conn) performs a server-side disconnect of tel-conn
    ;;    will trigger an on-disconnect call once the socket has been closed
    ;; disconnect: Telnet-User -> Void
    (define/public (disconnect tel-conn)
      (unless (telnet-user? tel-conn) (raise-argument-error 'master-object%:disconnect "telnet-user?" tel-conn))
      (async-channel-put (telnet-user-out tel-conn)
                         (telnet-msg 'QUIT #"")))

    (define/public (on-listener-message msg)
      (when (telnet-user? msg) (on-connect msg)))
    
    (define tick-length 3000) ; 3 second tick
    (define tick-offset (random tick-length))

    
    (define/public (set-tick t)
      (set! tick-length t)
      (set! tick-offset (random t)))
    
    (define/public (get-tick)
      (values tick-length tick-offset))
    
    (define/public (on-tock)
      (void))

    (define/public (on-message msg)
      (eprintf "Master needs to handle messages ~a" msg))
    
    (define/override (on-load)
      (set! master-object (oref this))
      (super on-load))))


