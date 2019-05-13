#lang griftos

(define t0 (current-inexact-milliseconds))
(require racket/tcp openssl griftos/griftos-config griftos/telnet #|griftos/websock|# griftos/charset "defaults.rkt")

(define cfg (load-griftos-settings))

(define telnet-port (hash-ref cfg 'telnet:port #f))
(define telnet-ssl-port (hash-ref cfg 'telnet:ssl-port #f))
(define telnet-enabled? (port-number? telnet-port))
(define telnet-ssl-enabled?
  (and ssl-available?
       (hash-ref cfg 'telnet:ssl #f)
       (hash-ref cfg 'ssl:certificate #f)
       (hash-ref cfg 'ssl:private-key #f)
       (port-number? telnet-ssl-port)))
       telnet-ssl-port

(define http-port (hash-ref cfg 'webserver:port #f))
(define http-enabled? (and (hash-ref cfg 'webserver:http #f)
                           (port-number? http-port)))

(define https-port (hash-ref cfg 'webserver:ssl-port #f))
(define https-enabled? (and ssl-available?
                            (hash-ref cfg 'webserver:https #f)
                            (hash-ref cfg 'ssl:certificate #f)
                            (hash-ref cfg 'ssl:private-key #f)
                            (port-number? https-port)))
                            
(define telnet-encodings
  (and (or telnet-enabled? telnet-ssl-enabled?)
       ;; convert the encodings list from the config file into the names that (mostly) always work with iconv
       (let ([encodings (map encoding->symbol (hash-ref cfg 'telnet:encodings '("ASCII")))])
         ;; If they didn't include ASCII, they should have done as a last resort, since telnet MUST default to ASCII
         (if (memq 'ASCII encodings) encodings (append encodings '(ASCII))))))
(define telnet-charset-seq
  (encodings->charset-req-sequence telnet-encodings))

(unless (or telnet-enabled? telnet-ssl-enabled? http-enabled? https-enabled?)
  (error 'griftos "telnet and websock are both disabled -- griftos must have at least some kind of outside connection!"))

(display "Establishing Database Connection...")
(set! t0 (current-inexact-milliseconds))
;; First, establish a DB connection so that the master object can be loaded
(let ([db-type (hash-ref cfg 'database:type #f)]
      [db-db (hash-ref cfg 'database:name #f)]
      [db-user (hash-ref cfg 'database:username #f)]
      [db-pass (hash-ref cfg 'database:password #f)]
      [db-srv (hash-ref cfg 'database:server #f)]
      [db-port (hash-ref cfg 'database:port #f)]
      [db-sock (hash-ref cfg 'database:socket #f)])
  (when db-type
    (database-setup (string->symbol db-type) db-port db-sock db-srv db-db db-user db-pass)))
(printf "connected in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))

;; Next, start the scheduler so events can be processed





;; With that taken care of, load the master object
(display "Loading Master Object...")
(set! t0 (current-inexact-milliseconds))
(define mudlib (hash-ref cfg 'mudlib-path "./lib"))
(define mudlib/path (path->directory-path (simplify-path (string->path mudlib))))

(set-lib-path! mudlib)


(parameterize ([current-library-collection-paths (cons mudlib/path (current-library-collection-paths))])
  (start-scheduler! (hash-ref cfg 'thread-count DEFAULT-THREAD-COUNT))  
  ;(displayln (current-library-collection-paths))
  (load-master-object! mudlib/path
                       (hash-ref cfg 'master-module "custom-master.rkt")
                       (string->symbol (hash-ref cfg 'master-classname "custom-master%")))
  
  (define custom-telnet%
    ((send master-object get-connection-mixin)
     telnet-conn%))
  (printf "loaded in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  ;(define custom-websock%
  ;  ((send master-object get-connection-mixin)
  ;   server-websock-conn%))

  (define (add-telnet-user in out ip [secure? #f])
    (define conn (new custom-telnet% [in in] [out out] [ip ip] [secure? secure?]
                      [telopts '(echo compress2 gmcp naws ttype charset binary mssp)]
                      [option-managers
                       `((,mccp2-manager%)
                         (,gmcp-manager%)
                         (,naws-manager%)
                         (,mssp-manager%)
                         (,ttype-manager%)
                         (,charset-manager% telnet-encodings telnet-charset-seq))]))
    ;    (eprintf "adding new user from ~v" ip)
    (send* conn
      (enable-telopt telopt:compress2 'local)
      (enable-telopt telopt:ttype 'remote)
      (enable-telopt telopt:gmcp 'local)
      (enable-telopt telopt:mssp 'local)
      (enable-telopt telopt:naws 'remote))
    (send master-object on-connect conn))

  ;; Next, start the telnet listeners (if enabled)
  (display "Starting Listeners...")
  (set! t0 (current-inexact-milliseconds))
  (define telnet-serv
    (and telnet-enabled? (tcp-listen telnet-port 16)))
  (define telnet-thread
    (and telnet-enabled?
         (thread (lambda ()
                   (let loop ()
                     (define-values (in out) (tcp-accept telnet-serv))
                     (define-values (lip ip) (tcp-addresses in))
                     (add-telnet-user in out ip)
                     (loop))))))

  (define ssl-serv
    (and telnet-ssl-enabled? (tcp-listen telnet-ssl-port 16)))
  (define ssl-ctxt
    (and telnet-ssl-enabled? (ssl-make-server-context 'secure)))
  (when ssl-serv
    (ssl-load-certificate-chain! ssl-ctxt (hash-ref cfg 'ssl:certificate))
    (ssl-load-private-key! ssl-ctxt (hash-ref cfg 'ssl:private-key)))
  (define ssl-thread
    (and telnet-ssl-enabled?
         (thread (lambda ()
                   (let loop ()
                     (define-values (in out) (tcp-accept ssl-serv))
                     (define-values (lip ip) (tcp-addresses in))
                     (define-values (sin sout) (ports->ssl-ports in out #:mode 'accept #:context ssl-ctxt #:close-original? #t #:shutdown-on-close? #t))
                     (add-telnet-user sin sout ip #t)
                     (loop))))))



  ;; and also start the webserver (if enabled)

  ;(define (start-webserver mode port ssl-port static-root servlet-url websock-url certificate private-key)
  
  (when (or http-enabled? https-enabled?)
    (start-webserver
     (cond [(not http-enabled?) 'https]
           [(not https-enabled?) 'http]
           [(hash-ref cfg 'webserver:ssl-redirect #f) 'http->https]
           [else 'http+https])
     http-port
     https-port
     (hash-ref cfg 'webserver:web-path "./www")
     (hash-ref cfg 'webserver:servlet-url "/servlet")
     (send master-object get-servlet-handler)
     (hash-ref cfg 'webserver:websock-url "/socket")
     (send master-object get-websocket-mapper
           (hash-ref cfg 'webserver:websock-url "/socket")
           (hash-ref cfg 'webserver:websock-client-url #f)
           (void))
     (hash-ref cfg 'ssl:certificate #f)
     (hash-ref cfg 'ssl:private-key #f)))

  (printf "started in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  
  (define shutdown-semaphore (make-semaphore 0))
  (define (griftos-shutdown!)
    (displayln "Shutting down...")
    (when telnet-thread (kill-thread telnet-thread))
    (when telnet-serv (tcp-close telnet-serv))
    (when ssl-thread (kill-thread ssl-thread))
    (when ssl-serv (tcp-close ssl-serv))
    (when repl-thread (kill-thread repl-thread))
    (shut-down!))


  ;; Finally, if running in interactive mode, start the REPL thread
  (define repl-thread
    (and (hash-ref cfg 'interactive #f)
         (thread
          (lambda ()
            (define ns (make-base-namespace))
            (define (shutdown!)
              (semaphore-post shutdown-semaphore))
            (namespace-attach-module (current-namespace) 'griftos ns)
            (namespace-require 'griftos ns)
            (parameterize ([current-namespace ns]
                           ;; todo - can parameterize some of the repl parameters to make a better interface
                           )
              (namespace-set-variable-value! 'shutdown! shutdown!)
              (read-eval-print-loop)
              (shutdown!))))))

  (displayln "Running")

  (with-handlers ([exn:break?
                   (lambda (e)
                     (log-info "User Break - Shutting Down")
                     (griftos-shutdown!))])
    (semaphore-wait shutdown-semaphore)
    (griftos-shutdown!)))


   
