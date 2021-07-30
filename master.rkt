#lang racket/base

(require racket/class racket/list racket/contract (for-syntax racket/base) racket/async-channel)
(require "websocket-server.rkt"  web-server/http/request-structs)
(require json)
(require "objects.rkt" "db.rkt")
(require telnet/connection)
(require "scheduler.rkt")
(require racket/rerequire)

(provide master-object master<%> websocket-server<%> start-scheduler! shut-down! load-master-object!)
(provide yield! queue-event send/yield!)
(provide update-certs start-webserver webserver-stop webserver)

(define master-object #f)

#|||||||||||||||||||||
Master Interface

The connection manager will send a telnet-object to it whenever a user connects.
|||||||||||||||||||||#

(define websocket-server<%>
  (interface ()
    [websock-request (->m (listof symbol?) request? void?)]))

(define master<%>
  (interface ()
    ;; on-connect (CPointer user_info_t) -> Telnet
    [on-connect (->m (is-a?/c terminal<%>) any/c)]
    [on-shutdown (->m void?)]
    [get-connection-mixin (->m (-> (implementation?/c terminal<%>) (implementation?/c terminal<%>)))]
    get-servlet-handler
    ))

(define scheduler (make-event-scheduler))
(define thread-pool #f)

(define (event-handler)
  (define event (sync scheduler))
  (with-handlers ([exn:break? void]
                  [exn? (λ(e) (log-message (current-logger) 'error #f (exn-message e) (exn-continuation-marks e) #f))]
                  [(λ (e) #t) (λ (e) (log-message (current-logger) 'error #f (format "event exception - ~v"  e) #f #f))])
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

(define webserver #f)

(define (update-certs cert key)
  (renew-websocket-server-certificate webserver cert key))

(define (webserver-stop)
  (stop-websocket-server webserver))
                      
(define (start-webserver mode port ssl-port socket-path static-root servlet-url servlet-handler websock-url ws-conn-req ws-conn-headers
                         certificate private-key)
  (when (and (not (eq? mode 'https))
             (not port))
    (raise-argument-error 'start-webserver "listen-port-number?" port))
  (when (and (not (eq? mode 'http))
             (not ssl-port))
    (raise-argument-error 'start-webserver "listen-port-number?" ssl-port))
  (unless (lazy-ref? master-object)
    (error 'start-webserver "master-object not found.  webserver must be started AFTER the object persistence layer"))
  (when (and socket-path (file-exists? socket-path))
    (delete-file socket-path))
  
  (let* ([ssl-redirect? (eq? mode 'http->https)]
         [confirmation-channel (make-async-channel)]
         [the-server (serve/servlet+websockets
                      servlet-handler
                      ws-conn-req ws-conn-headers
                      #:confirmation-channel confirmation-channel
                      #:http-port port
                      #:ssl-port ssl-port
                      #:socket-path socket-path
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
         [bound-ports (if socket-path
                          (list (async-channel-get confirmation-channel) #f)
                          (if
                           
                           (and port ssl-port)
                           (list (async-channel-get confirmation-channel)
                                 (async-channel-get confirmation-channel)
                                 #f)
                           (list (async-channel-get confirmation-channel) #f)))])
    (unless (and (memq (if socket-path 1 port) bound-ports)
                 (memq ssl-port bound-ports))
      (error 'start-webserver "Failed to bind to ports: ~v" bound-ports))
    (when socket-path
      (file-or-directory-permissions socket-path #o660))
    (set! webserver the-server)))
    
(define (start-scheduler! thread-count)
  (set! thread-pool (map (λ (i) (thread event-handler)) (range thread-count)))
  (scheduler-start! scheduler))

(define (load-master-object! resolved-mudlib-path master-class)
  (define paths (dynamic-rerequire resolved-mudlib-path #:verbosity 'none))
  (define % (dynamic-require resolved-mudlib-path master-class))
  (unless (implementation? % master<%>)
    (raise-argument-error 'load-master-object! "(implementation?/c master<%>" %))
  (unless (subclass? % saved-object%)
    (raise-argument-error 'load-master-object! "(subclass?/c saved-object%" %))
  (set! master-object (get-singleton %))
  paths)
  
(define (shut-down!)
  (displayln "Shutdown!")
  (send/rackmud master-object on-shutdown)
  (displayln "Shut down master object")
  (set! master-object 'shutting-down)
  (log-message (current-logger)
               'info
               'rackmud
               "Master object shutdown method completed"
               #f #f)
  (scheduler-stop! scheduler)
  (displayln "Stopped scheduler")
  (when webserver
    (webserver-stop)
    (displayln "Stopped webserver")
    (log-message (current-logger)
                 'info
                 'rackmud
                 "Webserver stopped"
                 #f #f))
  (for-each kill-thread thread-pool)
  (set! thread-pool #f)
  (displayln "Killed thread pool")
  (log-message (current-logger)
               'info
               'rackmud
               "Thread pool terminated"
               #f #f)
  (save-all-objects)
  (displayln "Saved cached objects")
  (log-message (current-logger)
               'info
               'rackmud
               "Saved cached objects successfully"
               #f #f)
  
  (database-disconnect)
  (displayln "Closed database connection"))
