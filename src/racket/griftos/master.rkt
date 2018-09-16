#lang racket/base

(require racket/class racket/list (for-syntax racket/base))

(require json)
(require "objects.rkt")
(require "telnet.rkt")
(require "scheduler.rkt")
(require racket/rerequire)
(provide master-object master<%> server-settings start-up)
(provide add-user-to-griftos)

(provide yield! queue-event current-time seconds->ticks ticks->seconds delay:now delay:one-tick delay:x-minutes delay:x-seconds delay:one-minute delay:one-second send/yield!)

(provide make-jit set-make-jit!)


;(provide ssl-settings ssl-settings? ssl-settings-private-key ssl-settings-certificate ssl-settings-port)
(provide db-settings  db-settings? db-settings-type db-settings-user db-settings-password db-settings-db
         db-settings-server db-settings-port db-settings-socket)
(provide griftos-config griftos-config? griftos-config-mudlib griftos-config-master-file griftos-config-master-class
         griftos-config-encodings griftos-config-database)



(define master-object #f)
(define server-settings #f)


#|||||||||||||||||||||||||||||||
Native Functions

These are hooks that the C backend will override with C functions

|||||||||||||||||||||||||||||||#

;; (make-jit proc) requests that proc be compiled  to native code
;; make-jit : Procedure -> Void
(define make-jit void)

(define (set-make-jit! proc)
  (unless (procedure? proc)
    (raise-argument-error 'set-make-jit! "procedure?" proc))
  (set! make-jit proc))



#|||||||||||||||||||||
Master Interface

The C backend will instantiate a singleton of this class.
The connection manager will send a telnet-object to it whenever a user connects.
|||||||||||||||||||||#


(define master<%>
  (interface ()
    ;; on-connect (CPointer user_info_t) -> Telnet
    on-connect))


#|||||||||||||||||||||
Main Loop
|||||||||||||||||||||#


(define scheduler (make-event-scheduler 1))
(define thread-pool #f)

(define (event-handler)
  (define event (sync scheduler))
  (with-handlers ([exn:break? void]
                  [(λ (e) #t) (λ (e) (eprintf "event exception ~e" e))])
    (when event (event)))
  (event-handler))

(define (yield! [time 0])
  (scheduler-yield! scheduler time))

(define (current-time)
  (event-scheduler-time scheduler))

(define (seconds->ticks s)
  (ceiling (/ s (griftos-config-sec/tick server-settings))))

(define (ticks->seconds t)
  (* t (griftos-config-sec/tick server-settings)))

(define (delay:now)
  0)

(define (delay:one-tick)
  (current-time))

(define (delay:x-minutes x)
  (+ (current-time) (seconds->ticks (* 60 x))))

(define (delay:x-seconds x)
  (+ (current-time) (seconds->ticks x)))

(define (delay:one-minute)
  (+ (current-time) (seconds->ticks 60)))

(define (delay:one-second)
  (+ (current-time) (seconds->ticks 1)))

(define-syntax (queue-event stx)
  (syntax-case stx ()
    [(_ delay body ...)
     #'(scheduler-add! scheduler delay (λ () body ...))]))


(define-syntax (send/yield! stx)
  (syntax-case stx ()
    [(_ obj-expr method parameter ...)
     #'(scheduler-call-and-return! sched (λ () (send/griftos obj-expr method parameter ...)) 0)]))


 



  

; DB-Settings is a (db-settings (anyof 'postgres 'sqlite 'odbc 'mysql) - type
;                               String                                 - user
;                               String                                 - password
;                               String                                 - db (for ODBC this is the DSN)
;                               (anyof String False)                   - server (false = localhost or use socket or N/A)
;                               (anyof Port False)                     - port (false = default or use socket or N/A)
;                               (anyof Path False))                    - socket (false = none or N/A)
(struct db-settings (type user password db server port socket) #:transparent)

; GriftOS-Config is a (griftos-config
;                        Path-String                     - mudlib
;                        String                          - master-file
;                        Sym                             - master-class
;                        (listof Sym)                    - encodings
;                        DB-Settings                     - database
;                        positive-integer                - threads
;                        positive-real)                   - sec/tick


(struct griftos-config (mudlib master-file master-class encodings database threads sec/tick) #:transparent)


; (startup cfg) starts the mud racket side running using the provided config struct cfg
; Effects: * Opens database connection
;          * Loads or instantiates master object
;              - this starts the master's event thread, too

; start-up: Griftos-CFG


(define (start-up cfg)
  (let ([mudlib (griftos-config-mudlib cfg)]
        [master-file (griftos-config-master-file cfg)]
        [master-class (griftos-config-master-class cfg)]
        [db-type (db-settings-type (griftos-config-database cfg))]
        [db-port (db-settings-port (griftos-config-database cfg))]
        [db-sock (db-settings-socket (griftos-config-database cfg))]
        [db-srv  (db-settings-server (griftos-config-database cfg))]
        [db-db   (db-settings-db (griftos-config-database cfg))]
        [db-user (db-settings-user (griftos-config-database cfg))]
        [db-pass (db-settings-password (griftos-config-database cfg))]
        [thread-count (griftos-config-threads cfg)])
    (set! server-settings cfg)
    ;#(set! event-q (make-pq))
    ;(set! thread-pool (build-list thread-pool-size (λ (x) (thread event-handler))))
    (set! thread-pool (map (λ (i) (thread event-handler)) (range thread-count)))
    (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
    (set-lib-path! mudlib)
    (dynamic-rerequire (build-path mudlib master-file))
    (define the-master% (dynamic-require (build-path mudlib master-file) master-class))
    (unless (implementation? the-master% master<%>)
      (raise-argument-error 'start-up "(implementation?/c master<%>)" master-class))
    (unless (subclass? the-master% saved-object%)
      (raise-argument-error 'start-up "(subclass?/c saved-object%)" master-class))
    (set! master-object  (get-singleton the-master%))
    master-object))

(define (add-user-to-griftos cptr)
  (unless master-object
    (error 'add-user-to-griftos "GriftOS has not been started!"))
  (send/griftos master-object on-connect cptr))
  


#|
The Master Object is responsible for saving any server-wide values that need saving.

It also has the methods for establishing, maintaining, and disconnecting telnet connections (Telnet-User structs)

|#


