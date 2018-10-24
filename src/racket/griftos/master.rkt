#lang racket/base

(require racket/class racket/list (for-syntax racket/base) racket/async-channel)

(require web-server/websocket/server)


(require json)
(require "objects.rkt")
(require "telnet.rkt")
(require "scheduler.rkt")
(require racket/rerequire)
(provide master-object master<%> server-settings start-up)
(provide add-user-to-griftos)

(provide yield! queue-event)
(provide update-certs stop-webserver)
(provide make-jit set-make-jit!)


;(provide ssl-settings ssl-settings? ssl-settings-private-key ssl-settings-certificate ssl-settings-port)
(provide db-settings  db-settings? db-settings-type db-settings-user db-settings-password db-settings-db
         db-settings-server db-settings-port db-settings-socket)

(provide www-settings www-settings? www-settings-port www-settings-ssl-port www-settings-www-root www-settings-griftos-url
         www-settings-ssl-cert www-settings-ssl-key www-settings-websock-url www-settings-ssl-redirect?)

(provide griftos-config griftos-config? griftos-config-mudlib griftos-config-master-file griftos-config-master-class
         griftos-config-encodings griftos-config-database griftos-config-www)



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
    on-connect
    get-servlet-handler
    get-websocket-mapper
    ))


#|||||||||||||||||||||
Main Loop
|||||||||||||||||||||#


(define scheduler (make-event-scheduler))
(define thread-pool #f)

(define (event-handler)
  (define event (sync scheduler))
  (with-handlers ([exn:break? void]
                  [(λ (e) #t) (λ (e) ((error-display-handler) "event exception:" e))])
    (when event (event)))
  (event-handler))

(define (yield! [time 0])
  (scheduler-yield! scheduler time))


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

; WWW-Settings is one of
;  *  #f
;  *  (www-settings ListenPort (anyof ListenPort False) Bool Path-String String Path-String Path-String)

(struct www-settings (port ssl-port ssl-redirect? www-root griftos-url websock-url ssl-cert ssl-key) #:transparent)

; GriftOS-Config is a (griftos-config
;                        Path-String                     - mudlib
;                        WWW-Settings                    - www 
;                        String                          - master-file
;                        Sym                             - master-class
;                        (listof Sym)                    - encodings
;                        DB-Settings                     - database
;                        positive-integer                - threads



(struct griftos-config (mudlib www master-file master-class encodings database threads) #:transparent)




(define webserver #f)

(define (update-certs cert key)
  (renew-websocket-server-certificate webserver cert key))

(define (stop-webserver)
  (stop-websocket-server webserver))
  


(define (start-webserver cfg)
  (let* ([www-settings (griftos-config-www cfg)]
         [port (www-settings-port www-settings)]
         [www-root (www-settings-www-root www-settings)]
         [ssl-port (www-settings-ssl-port www-settings)]
         [servlet-url (www-settings-griftos-url www-settings)]
         [websock-url (www-settings-websock-url www-settings)]
         [ssl-redirect? (www-settings-ssl-redirect? www-settings)]
         [ssl-cert (www-settings-ssl-cert www-settings)]
         [ssl-key (www-settings-ssl-key www-settings)]
         [confirmation-channel (make-async-channel)]
         [the-server (serve/servlet+websockets
                      (send/griftos master-object get-servlet-handler)
                      (send/griftos master-object get-websocket-mapper websock-url)
                      #:confirmation-channel confirmation-channel
                      #:http-port port
                      #:ssl-port ssl-port
                      #:http? (and port #t)
                      #:ssl? (and ssl-port #t)
                      #:listen-ip #f ; TODO give config control of this
                      #:force-ssl? ssl-redirect?
                      #:ssl-cert ssl-cert
                      #:ssl-key ssl-key
                      #:server-root-path www-root
                      #:servlet-path servlet-url
                      #:servlet-regexp (regexp (format "^~a$|^~a/" servlet-url servlet-url))
                      #:websocket-path websock-url
                      #:websocket-regexp (regexp (format "^~a$|^~a/" websock-url websock-url)))]
         [bound-ports (if (and port ssl-port)
                          (list (async-channel-get confirmation-channel)
                                (async-channel-get confirmation-channel)
                                #f)
                          (list (async-channel-get confirmation-channel) #f))])
    (unless (and (memq port bound-ports)
                 (memq ssl-port bound-ports))
      (error 'start-webserver "Failed to bind to ports: ~v" bound-ports))
    (set! webserver the-server)))
    
                      ; TODO #:log-file griftos-log-port
                      
                      
                      
    
    
    
         
         
                                 
           
           


; (startup cfg) starts the mud racket side running using the provided config struct cfg
; Effects: * Opens database connection
;          * Loads or instantiates master object
;              - this starts the master's event thread, too

; start-up: Griftos-CFG





(define (start-up cfg)
  (let ([mudlib (griftos-config-mudlib cfg)]
        [www-settings (griftos-config-www cfg)]
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
    (when www-settings (start-webserver cfg))
    master-object))

(define (add-user-to-griftos cptr)
  (unless master-object
    (error 'add-user-to-griftos "GriftOS has not been started!"))
  (send/griftos master-object on-connect cptr))
  


#|
The Master Object is responsible for saving any server-wide values that need saving.

It also has the methods for establishing, maintaining, and disconnecting telnet connections (Telnet-User structs)

|#


