#lang racket/base

(define t0 (current-inexact-milliseconds))
(require net/rfc6455 net/url web-server/http/request-structs
         racket/tcp openssl telnet telnet/charset racket/place racket/runtime-path
         "../main.rkt" "../backtrace.rkt" "../db.rkt"  "../websock.rkt"  "../connections.rkt" "config.rkt" "new-setup.rkt"
         )
(define-namespace-anchor anc)

(define (list=? a b)
  (or (and (null? a) (null? b))
      (and (cons? a) (cons? b) (eq? (car a) (car b))
           (list=? (cdr a) (cdr b)))))

(define cfg (load-rackmud-settings))
(define-runtime-path COMPILER-PLACE "./compiler-place.rkt")

(define missing-file? (hash-ref cfg 'no-file? #f))

(when missing-file?
  
  (set! cfg (rackmud-configure cfg))
  (unless cfg
    (exit 1)))

(define custom-telnet%
  (conn-mixin telnet-conn%))

(define custom-websock%
  (conn-mixin websock-terminal%))

(define mudlib (hash-ref cfg 'mudlib-path #f))
(define mudlib/path (and mudlib (path->complete-path (path->directory-path (simplify-path (string->path mudlib))))))
(define mudlib-collect (string->symbol (hash-ref cfg 'mudlib-collect "mudlib")))
(unless (module-path? mudlib-collect)
  (error 'mudlib-collection: "Expected module-path? but found ~a" mudlib-collect))
(define mudlib-module (hash-ref cfg 'master-module "main.rkt"))

;(parameterize ([current-library-collection-links (if mudlib/path
;                                                       (cons (hasheq mudlib-collect (list mudlib/path)) (current-library-collection-links))
;                                                       (current-library-collection-links))])

(define compiler-place
  (dynamic-place COMPILER-PLACE 'compiler-place-main))

(place-channel-put compiler-place mudlib/path)
(place-channel-put compiler-place mudlib-collect)

(define rackmud-logger (make-logger #f (current-logger) 'info #f))
(define rackmud-log-rec (make-log-receiver (current-logger) 'debug 'rackmud 'debug 'grapevine 'warning))
(current-logger rackmud-logger)


(parameterize ([current-library-collection-paths (if mudlib/path
                                                     (cons  mudlib/path (current-library-collection-paths))
                                                     (current-library-collection-paths))]
               [current-logger rackmud-logger])

  (define logger-thread
        (thread
         (λ ()
           (let loop ()
             (match (sync rackmud-log-rec)
               [(vector level msg data topic)
                (when (and (string? msg) (database-connected?))
                  (database-log level (or topic "racket") msg (backtrace data)))])
             (loop)))))
  
  (define telnet-port (hash-ref cfg 'telnet:port #f))
  (define telnet-ssl-port (hash-ref cfg 'telnet:ssl-port #f))
  (define telnet-enabled? (port-number? telnet-port))
  (define telnet-ssl-enabled?
    (and ssl-available?
         (hash-ref cfg 'ssl:certificate #f)
         (hash-ref cfg 'ssl:private-key #f)
         (port-number? telnet-ssl-port)))
  
  (define http-port (hash-ref cfg 'webserver:port #f))
  (define http-enabled? (port-number? http-port))
  
  (define https-port (hash-ref cfg 'webserver:ssl-port #f))
  (define https-enabled? (and ssl-available?
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
    (error 'rackmud "telnet and websock are both disabled -- rackmud must have at least some kind of outside connection!"))
  
  (display "Establishing Database Connection...")
  (set! t0 (current-inexact-milliseconds))
  ;; First, establish a DB connection so that the master object can be loaded
  (let ([db-type (hash-ref cfg 'database:type #f)]
        [db-db (hash-ref cfg 'database:database #f)]
        [db-user (hash-ref cfg 'database:username #f)]
        [db-pass (hash-ref cfg 'database:password #f)]
        [db-srv (hash-ref cfg 'database:server #f)]
        [db-port (hash-ref cfg 'database:port #f)]
        [db-sock (hash-ref cfg 'database:socket #f)])
    (when db-type
      (database-setup (if (string? db-type) (string->symbol db-type) db-type) db-port db-sock db-srv db-db db-user db-pass)))
  (printf "connected in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  
  ;; Next, start the scheduler so events can be processed
  
  
  
  (unless (place-channel-get compiler-place)
    (displayln "\nmudlib is not in a buildable state.  Aborting!\n" (current-error-port))
    (exit 1))

  ;; With that taken care of, load the master object
  
  (define resolved-collect (collection-file-path mudlib-module (symbol->string mudlib-collect) #:fail
                                                 (lambda (message)
                                                   (eprintf "Error loading mudlib collect\n~a\n" message)
                                                   (exit 1))))
  
  (let-values ([(base-lib-path module-name _) (split-path resolved-collect)])
    (set-lib-path! base-lib-path))
  
  
  (start-scheduler! (hash-ref cfg 'thread-count))  
  ;(displayln (current-library-collection-paths))
  (display "Loading Master Object...")
  (set! t0 (current-inexact-milliseconds))
  
  (load-master-object! resolved-collect
                       (string->symbol (hash-ref cfg 'master-classname "custom-master%")))
  

  
  (printf "loaded in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  ;(define custom-websock%
  ;  ((send master-object get-connection-mixin)
  ;   server-websock-conn%))

#|
  (define (add-websock-user ws-conn)
    (define handshake (ws-recv ws-conn))
    (unless (eof-object? handshake)
      (define handshake/json (string->jsexpr handshake))
      (define conn (new custom-websock%
                        [websock-connection ws-conn]
                        [secure? (hash-ref handshake/json 'ssl)]
                        [ip (hash-ref handshake/json 'ip)]))
      (send master-object on-connect conn)))
  |#
  
  (define (add-telnet-user in out ip [secure? #f])
    (define conn (new custom-telnet% [in in] [out out] [ip ip] [secure? secure?]
                      [telopts '(echo compress2 gmcp naws ttype charset binary mssp mxp)]
                      [option-managers
                       `((,mccp2-manager%)
                         (,gmcp-manager%)
                         (,naws-manager%)
                         (,mssp-manager%)
                         (,ttype-manager%)
                         (,charset-manager% telnet-encodings telnet-charset-seq))]))
    ;    (eprintf "adding new user from ~v" ip)
    (send* conn
      (enable-telopt telopt:mxp 'local)
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
    (and telnet-enabled? (tcp-listen telnet-port 16 #t)))
  (define telnet-thread
    (and telnet-enabled?
         (thread (lambda ()
                   (let loop ()
                     (with-handlers ([exn? (lambda (e)
                                             (database-log 'error "Telnet-Listener" (exn-message e) (backtrace (exn-continuation-marks e))))])
                       (define-values (in out) (tcp-accept telnet-serv))
                       (define-values (lip ip) (tcp-addresses in))
                       (add-telnet-user in out ip))
                     (loop))))))

  (define ssl-serv
    (and telnet-ssl-enabled? (tcp-listen telnet-ssl-port 16 #t)))
  (define ssl-ctxt
    (and telnet-ssl-enabled? (ssl-make-server-context 'secure)))
  (when ssl-serv
    (ssl-load-certificate-chain! ssl-ctxt (hash-ref cfg 'ssl:certificate))
    (ssl-load-private-key! ssl-ctxt (hash-ref cfg 'ssl:private-key)))
  (define ssl-thread
    (and telnet-ssl-enabled?
         (thread (lambda ()
                   (let loop ()
                     (with-handlers ([exn? (lambda (e)
                                             (database-log 'error "SSL-Telnet-Listener" (exn-message e) (backtrace (exn-continuation-marks e))))])

                       (define-values (in out) (tcp-accept ssl-serv))
                       (define-values (lip ip) (tcp-addresses in))
                       (define-values (sin sout) (ports->ssl-ports in out #:mode 'accept #:context ssl-ctxt #:close-original? #t #:shutdown-on-close? #f))
                       (add-telnet-user sin sout ip #t))
                     (loop))))))



  (define (reload-certificates!)
    (define certificate (hash-ref cfg 'ssl:certificate #f))
    (define private-key (hash-ref cfg 'ssl:private-key #f))
    (when (and private-key certificate)
      (log-info "SSL Certificate Changed.  Reloading")
      (when https-enabled?
        (update-certs certificate private-key))
      (when ssl-ctxt
        (ssl-load-certificate-chain! ssl-ctxt certificate)
        (ssl-load-private-key! ssl-ctxt private-key))))
  
  ;; and also start the webserver (if enabled)

  ;(define (start-webserver mode port ssl-port static-root servlet-url websock-url certificate private-key)

  (define websock-url-prefix-length (length (explode-path (string->path (hash-ref cfg 'webserver:websock-url "socket")))))

  (define websock-client-url/path-list (map (compose string->symbol path->string) (explode-path (string->path
                                                                                                 (hash-ref cfg 'webserver:websock-client-url)))))
  
  (define (ws-conn-req ws-conn req)
    (let ([path-elements (map (compose string->symbol path/param-path)
                                     (drop (url-path (request-uri req))
                                           websock-url-prefix-length))])
      (if (list=? path-elements websock-client-url/path-list)
          (send master-object
                on-connect
                (new
                 custom-websock%
                 [websock-connection ws-conn]
                 [secure? (eqv? (request-host-port req) https-port)]
                 [ip (request-client-ip req)]
                 ))
          (if (is-a? master-object websocket-server<%>)
              (send master-object websock-request path-elements req)
              (ws-close! ws-conn 1008 "BAD WEBSOCKET URL")
              ))))

  (define (ws-req-headers request-line headers req)
    (values '() req))
          
  
  (when (or http-enabled? https-enabled?)
    (start-webserver
     (cond [(not http-enabled?) 'https]
           [(not https-enabled?) 'http]
           [else 'http+https])
     http-port
     https-port
     (hash-ref cfg 'webserver:web-path "./www")
     (hash-ref cfg 'webserver:servlet-url "servlet")
     (send master-object get-servlet-handler (hash-ref cfg 'webserver:servlet-url "servlet"))
     (hash-ref cfg 'webserver:websock-url "socket")
     ;(send master-object get-websocket-mapper
     ;      (hash-ref cfg 'webserver:websock-url "socket")
     ;      (hash-ref cfg 'webserver:websock-client-url #f)
     ;      add-websock-user)
     ws-conn-req
     ws-req-headers
     
     (hash-ref cfg 'ssl:certificate #f)
     (hash-ref cfg 'ssl:private-key #f)))

  (define cert-thread
    (and (or https-enabled? ssl-ctxt)
         (thread (lambda ()
                   (let ([cert-evt (filesystem-change-evt (hash-ref cfg 'ssl:certificate))]
                         [pkey-evt (filesystem-change-evt (hash-ref cfg 'ssl:private-key))])
                     (let loop ()
                       (sync cert-evt pkey-evt)
                       (sleep 1/10) ; just in case there's a delay between one changing and the other 
                       (reload-certificates!)))))))

  (printf "started in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))

  (define shutdown-semaphore (make-semaphore 0))
  (define (rackmud-shutdown!)
    (displayln "Shutting down...")
    (when cert-thread (kill-thread cert-thread))
    (when telnet-thread (kill-thread telnet-thread))
    (when telnet-serv (tcp-close telnet-serv))
    (when ssl-thread (kill-thread ssl-thread))
    (when ssl-serv (tcp-close ssl-serv))
    (when repl-thread (kill-thread repl-thread))
    (displayln "Telnet and REPL stopped...")
    (shut-down!))


  ;; Finally, if running in interactive mode, start the REPL thread

  (define rebuild-thread
    (thread
     (λ ()
       (let loop ()
         (define changes (place-channel-get compiler-place))
         (when (cons? changes)
           ;; TODO!!!
           (rackmud-mark-reloads changes)
           (void))
         (loop)))))
  
  
  (define repl-thread
    (and (hash-ref cfg 'interactive #f)
         (thread
          (lambda ()
            (define ns (namespace-anchor->namespace anc))
            (define (shutdown!)
              (semaphore-post shutdown-semaphore))
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
                     (rackmud-shutdown!))])
    (semaphore-wait shutdown-semaphore)
    (rackmud-shutdown!)))


   
