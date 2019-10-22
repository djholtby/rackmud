#lang racket/base

(require racket/class racket/list racket/contract (for-syntax racket/base) racket/async-channel)

(require web-server/websocket)


(require json)
(require "objects.rkt")
(require "connection.rkt")
(require "scheduler.rkt")
(require racket/rerequire)
(provide master-object master<%> start-scheduler! shut-down! load-master-object!)
;(provide add-user-to-rackmud)

(provide yield! queue-event)
(provide update-certs start-webserver webserver-stop webserver)



;(provide ssl-settings ssl-settings? ssl-settings-private-key ssl-settings-certificate ssl-settings-port)
#| 
(provide db-settings  db-settings? db-settings-type db-settings-user db-settings-password db-settings-db
         db-settings-server db-settings-port db-settings-socket)

(provide www-settings www-settings? www-settings-port www-settings-ssl-port www-settings-www-root www-settings-rackmud-url
         www-settings-ssl-cert www-settings-ssl-key www-settings-websock-url www-settings-ssl-redirect?)

(provide rackmud-config rackmud-config? rackmud-config-mudlib rackmud-config-master-file rackmud-config-master-class
         rackmud-config-encodings rackmud-config-database rackmud-config-www)
|#


(define master-object #f)

#|||||||||||||||||||||
Master Interface

The connection manager will send a telnet-object to it whenever a user connects.
|||||||||||||||||||||#


(define master<%>
  (interface ()
    ;; on-connect (CPointer user_info_t) -> Telnet
    [on-connect (->m (is-a?/c terminal<%>) any/c)]
    [on-shutdown (->m void?)]
    [get-connection-mixin (->m (-> (implementation?/c terminal<%>) (implementation?/c terminal<%>)))]
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
                  [(λ (e) #t) (λ (e) ((error-display-handler) (string-append "event exception - " (exn-message e)) e))])
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
     #'(scheduler-call-and-return! sched (λ () (send/rackmud obj-expr method parameter ...)) 0)]))

#|
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

(struct www-settings (port ssl-port ssl-redirect? www-root rackmud-url websock-url ssl-cert ssl-key) #:transparent)

; GriftOS-Config is a (rackmud-config
;                        Path-String                     - mudlib
;                        WWW-Settings                    - www 
;                        String                          - master-file
;                        Sym                             - master-class
;                        (listof Sym)                    - encodings
;                        DB-Settings                     - database
;                        positive-integer                - threads



(struct rackmud-config (mudlib www master-file master-class encodings database threads) #:transparent)
|#



(define webserver #f)

(define (update-certs cert key)
  (renew-websocket-server-certificate webserver cert key))

(define (webserver-stop)
  (stop-websocket-server webserver))
  

#|
(define (start-webserver cfg)
  (let* ([www-settings (rackmud-config-www cfg)]
         [port (www-settings-port www-settings)]
         [www-root (www-settings-www-root www-settings)]
         [ssl-port (www-settings-ssl-port www-settings)]
         [servlet-url (www-settings-rackmud-url www-settings)]
         [websock-url (www-settings-websock-url www-settings)]
         [ssl-redirect? (www-settings-ssl-redirect? www-settings)]
         [ssl-cert (www-settings-ssl-cert www-settings)]
         [ssl-key (www-settings-ssl-key www-settings)]
         [confirmation-channel (make-async-channel)]
         [the-server (serve/servlet+websockets
                      (send/rackmud master-object get-servlet-handler)
                      (send/rackmud master-object get-websocket-mapper websock-url)
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
|#    
                      ; TODO #:log-file rackmud-log-port
                      
(define (start-webserver mode port ssl-port static-root servlet-url servlet-handler websock-url websock-mapper certificate private-key)
  (when (and (not (eq? mode 'https))
             (not port))
    (raise-argument-error 'start-webserver "listen-port-number?" port))
  (when (and (not (eq? mode 'http))
             (not ssl-port))
    (raise-argument-error 'start-webserver "listen-port-number?" ssl-port))
  (unless (lazy-ref? master-object)
    (error 'start-webserver "master-object not found.  webserver must be started AFTER the object persistence layer"))
  (let* ([ssl-redirect? (eq? mode 'http->https)]
         [confirmation-channel (make-async-channel)]
         [the-server (serve/servlet+websockets
                      servlet-handler
                      websock-mapper

                      #:confirmation-channel confirmation-channel
                      #:http-port port
                      #:ssl-port ssl-port
                      #:http? (not (eq? mode 'https))
                      #:ssl? (not (eq? mode 'http))
                      #:listen-ip #f ; TODO give config control of this
                      #:force-ssl? ssl-redirect?
                      #:ssl-cert certificate
                      #:ssl-key private-key
                      #:servlet-namespace '(rackmud)
                      #:server-root-path static-root
                      #:servlet-path servlet-url
                      #:servlet-regexp (regexp (let ([quoted-url (root-url-quote servlet-url)])
                                                 (format "^~a$|^~a/" quoted-url quoted-url)))
                      #:websocket-path websock-url
                      #:websocket-regexp (regexp (let ([quoted-url (root-url-quote websock-url)])
                                                 (format "^~a$|^~a/" quoted-url quoted-url))))]
         [bound-ports (if (and port ssl-port)
                          (list (async-channel-get confirmation-channel)
                                (async-channel-get confirmation-channel)
                                #f)
                          (list (async-channel-get confirmation-channel) #f))])
    (unless (and (memq port bound-ports)
                 (memq ssl-port bound-ports))
      (error 'start-webserver "Failed to bind to ports: ~v" bound-ports))
    (set! webserver the-server)))
    
    
    
         
         
                                 
           
           


; (startup cfg) starts the mud racket side running using the provided config struct cfg
; Effects: * Opens database connection
;          * Loads or instantiates master object
;              - this starts the master's event thread, too

; start-up: Griftos-CFG



(define (start-scheduler! thread-count)
  (set! thread-pool (map (λ (i) (thread event-handler)) (range thread-count)))
  (scheduler-start! scheduler))

(define (load-master-object! resolved-mudlib-path master-class)
  (dynamic-rerequire resolved-mudlib-path)
  (define % (dynamic-require resolved-mudlib-path master-class))
  (unless (implementation? % master<%>)
    (raise-argument-error 'load-master-object! "(implementation?/c master<%>" %))
  (unless (subclass? % saved-object%)
    (raise-argument-error 'load-master-object! "(subclass?/c saved-object%" %))
  (set! master-object (get-singleton %)))
#|
(define (start-up! cfg)
  (let ([mudlib (rackmud-config-mudlib cfg)]
        [www-settings (rackmud-config-www cfg)]
        [master-file (rackmud-config-master-file cfg)]
        [master-class (rackmud-config-master-class cfg)]
        [db-type (db-settings-type (rackmud-config-database cfg))]
        [db-port (db-settings-port (rackmud-config-database cfg))]
        [db-sock (db-settings-socket (rackmud-config-database cfg))]
        [db-srv  (db-settings-server (rackmud-config-database cfg))]
        [db-db   (db-settings-db (rackmud-config-database cfg))]
        [db-user (db-settings-user (rackmud-config-database cfg))]
        [db-pass (db-settings-password (rackmud-config-database cfg))]
        [thread-count (rackmud-config-threads cfg)])
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
    (scheduler-start! scheduler)
    master-object))
|#

;(define (add-user-to-rackmud cptr ip)
;  (unless master-object
;    (error 'add-user-to-rackmud "GriftOS has not been started!"))
;  (when (lazy-ref? master-object)
;    (send/rackmud master-object on-connect cptr ip)))
  
(define (shut-down!)
  (displayln "Shutdown!")
  (send/rackmud master-object on-shutdown)
  (displayln "Shut down master object")
  (set! master-object 'shutting-down)
  (scheduler-stop! scheduler)
  (displayln "Stopped scheduler")
  (when webserver
    (webserver-stop))
  (displayln "Stopped webserver")
  (for-each kill-thread thread-pool)
  (set! thread-pool #f)
  (semaphore-wait object-table/semaphore)
  (hash-for-each object-table
                 (λ (oid obj)
                   (let ([o (weak-box-value obj)])
                     (when o (save-object o)))))
  (semaphore-post object-table/semaphore)
  (displayln "Saved cached objects")
  (database-disconnect)
  (displayln "Closed database connection"))
  

#|
The Master Object is responsible for saving any server-wide values that need saving.

It also has the methods for establishing, maintaining, and disconnecting telnet connections (Telnet-User structs)

|#


