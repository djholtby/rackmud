#lang racket/base

(define t0 (current-inexact-milliseconds))
(require net/rfc6455 net/url web-server/http/request-structs xml
         racket/tcp openssl telnet charset racket/place racket/runtime-path
         net/base64 racket/random unix-signals racket/system no-echo
         (except-in "../main.rkt" rackmud:domain rackmud:telnet-port rackmud:ssl-telnet-port
                    rackmud:web-root-path
                    rackmud:http-port rackmud:https-port rackmud:servlet-path rackmud:websock-path
                    rackmud:websock-client-path
                    rackmud:proxy-mode)
         "../web.rkt" "../backtrace.rkt" "../db.rkt"  "../websock.rkt" "../crypto/pepper.rkt"
         "../connections.rkt" "config.rkt" "new-setup.rkt" "../auth.rkt" "../parameters.rkt"
         "../menu.rkt")

(require readline/rep-start)


(define (list=? a b)
  (or (and (null? a) (null? b))
      (and (cons? a) (cons? b) (eq? (car a) (car b))
           (list=? (cdr a) (cdr b)))))

(struct request+object request (authorized-object))

(define (request->request+object req obj)
  (request+object
   (request-method req)
   (request-uri req)
   (request-headers/raw req)
   (request-bindings/raw-promise req)
   (request-post-data/raw req)
   (request-host-ip req)
   (request-host-port req)
   (request-client-ip req)
   obj))

(define jwt-filename "JWT-KEY.rktd")
(define (get-jwt-secret)
  (if (file-exists? jwt-filename)
      (with-input-from-file jwt-filename read)
      (let [(secret (bytes->string/utf-8 (base64-encode (crypto-random-bytes 128))))]
        (with-output-to-file jwt-filename (λ () (write secret)))
        secret)))

(define cfg (load-rackmud-settings))
(define-runtime-path COMPILER-PLACE "./compiler-place.rkt")

(define missing-file? (hash-ref cfg 'no-file? #f))

(define (in-tmux?)
  (and (environment-variables-ref (current-environment-variables) #"TMUX") #t))


(when missing-file?
  (set! cfg (rackmud-configure cfg #:in pre-readline-input-port))
  (unless cfg
    (exit 1)))

(define custom-telnet%
  (conn-mixin telnet-conn%))

(define custom-websock%
  (conn-mixin websock-terminal%))

(define mudlib (hash-ref cfg 'mudlib-path #f))
(define mudlib/path (and mudlib (simplify-path
                                 (path->directory-path
                                  (path->complete-path (string->path mudlib))))))

(define mudlib-runtime-path (simplify-path
                             (path->directory-path
                              (path->complete-path
                               (hash-ref cfg 'mudlib-runtime-path "./runtime-collects/")))))

(unless (directory-exists? mudlib-runtime-path)
  (make-directory mudlib-runtime-path))

(define mudlib-collect (string->symbol (hash-ref cfg 'mudlib-collect "mudlib")))
(unless (module-path? mudlib-collect)
  (error 'mudlib-collection: "Expected module-path? but found ~a" mudlib-collect))
(define mudlib-module (hash-ref cfg 'master-module "main.rkt"))

(define compiler-place
  (dynamic-place COMPILER-PLACE 'compiler-place-main))

(place-channel-put compiler-place
                   `((mudlib-collect . ,mudlib-collect)
                     (extra-collects-path . ,mudlib/path)
                     (compile-on-launch? . ,(hash-ref cfg 'compile-mudlib-on-launch? #t))
                     (runtime-collects-path . ,mudlib-runtime-path)))

;(define rackmud-logger (make-logger #f (current-logger) 'warning #f))
;(current-logger rackmud-logger)
(define rackmud-log-rec
  (make-log-receiver (current-logger)
                     'debug 'compiler-place
                     'debug 'rackmud:servlet
                     'debug 'rackmud:auth
                     'debug 'rackmud
                     'debug 'telnet
                     'info 'grapevine
                     'warning)) ; default level to log
(define telnet-port (hash-ref cfg 'telnet:port #f))
(define telnet-ssl-port (hash-ref cfg 'telnet:ssl-port #f))
(define telnet-enabled? (port-number? telnet-port))
(define telnet-ssl-enabled?
  (and ssl-available?
       (hash-ref cfg 'ssl:certificate #f)
       (hash-ref cfg 'ssl:private-key #f)
       (port-number? telnet-ssl-port)))
  
(define http-port (hash-ref cfg 'webserver:port #f))
(define proxy-mode (hash-ref cfg 'webserver:proxy-mode #f))
(define unix-domain-socket (and proxy-mode (hash-ref cfg 'webserver:proxy-socket-path #f)))
(define http-enabled? (or unix-domain-socket (port-number? http-port)))
  
(define https-port (hash-ref cfg 'webserver:ssl-port #f))
(define https-enabled? (and ssl-available?
                            (hash-ref cfg 'ssl:certificate #f)
                            (hash-ref cfg 'ssl:private-key #f)
                            (port-number? https-port)))


(define servlet-url (hash-ref cfg 'webserver:servlet-url "servlet"))
(define socket-url (hash-ref cfg 'webserver:websock-url "socket"))
(define socket-client-url (hash-ref cfg 'webserver:websock-client-url "client"))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(parameterize ((current-library-collection-paths
                (cons mudlib-runtime-path (current-library-collection-paths)))
               (current-namespace ns)
               (jwt-secret (get-jwt-secret))
               (jwt-duration (hash-ref cfg 'jwt-duration 600))
               (jwt-domain (hash-ref cfg 'server-domain #f))
               (current-unescaped-tags html-unescaped-tags)
               (rackmud:domain (hash-ref cfg 'server-domain #f))
               (rackmud:telnet-port (and telnet-enabled? telnet-port))
               (rackmud:ssl-telnet-port (and telnet-ssl-enabled? telnet-ssl-port))
               (rackmud:http-port (and http-enabled? http-port))
               (rackmud:https-port (and https-enabled? https-port))
               (rackmud:web-root-path (hash-ref cfg 'webserver:web-root-path ""))
               (rackmud:servlet-path (and (or http-enabled? https-enabled?)
                                          (path-string? servlet-url)
                                          servlet-url))
               (rackmud:websock-path (and (or http-enabled? https-enabled?)
                                          (path-string? socket-url)
                                          socket-url))
               (rackmud:websock-client-path (and (or http-enabled? https-enabled?)
                                                 (path-string? socket-client-url)
                                                 socket-client-url))
               (rackmud:proxy-mode proxy-mode)
               )


  (define logger-thread
    (thread
     (λ ()
       (let loop ()
         (match (sync rackmud-log-rec)
           [(vector level msg data topic)
            (when (and (string? msg) (database-connected?))
              (database-log level (or topic "racket") msg (backtrace data)))])
         (loop)))))
  

  



  (define telnet-encodings
    (and (or telnet-enabled? telnet-ssl-enabled?)
         ;; convert the encodings list from the config file into the names that work with iconv
         (let ([encodings (map string->charset-name (hash-ref cfg 'telnet:encodings '("ASCII")))])
           ;; If they didn't include ASCII, they should have done as a last resort
           (if (memq 'ASCII encodings) encodings (append encodings '(ASCII))))))

  (define telnet-charset-seq
    (encodings->charset-req-sequence telnet-encodings))
  
  (unless (or telnet-enabled? telnet-ssl-enabled? http-enabled? https-enabled?)
    (error 'rackmud
           (string-append
            "telnet and websock are both disabled --"
            " rackmud must have at least some kind of outside connection!")))
  
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
      (database-setup (if (string? db-type) (string->symbol db-type) db-type)
                      db-port db-sock db-srv db-db db-user db-pass)))
  (printf "connected in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  (log-message (current-logger) 'info 'rackmud "Server starting up..." #f #f)
  ;; wait for the compiler place to finish its thing
  (define initial-build-response (place-channel-get compiler-place))
  (unless (eq? #t initial-build-response)
    (displayln "\nmudlib is not in a buildable state.  Aborting!\n" (current-error-port))
    (displayln initial-build-response (current-error-port))
    (exit 1))

  (void (get-pepper))

  ;; With that taken care of, load the master object
  
  (define resolved-collect (collection-file-path mudlib-module (symbol->string mudlib-collect)
                                                 #:fail
                                                 (lambda (message)
                                                   (eprintf "Error loading mudlib collect\n~a\n"
                                                            message)
                                                   (exit 1))))
  
  (let-values ([(base-lib-path module-name _) (split-path resolved-collect)])
    (set-lib-path! base-lib-path))
    
  ;; Next, start the scheduler so events can be processed
    
  (start-executor!)
  (start-scheduler! (hash-ref cfg 'thread-count))  
  (display "Loading Master Object...")
  (set! t0 (current-inexact-milliseconds))
  (define t-master t0)
  (void
   
     (load-master-object! resolved-collect
                          (string->symbol (hash-ref cfg 'master-classname "custom-master%"))))

  (printf "loaded in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))
  
  (when (is-a? master-object has-setup-menu<%>)
    (displayln "master object has setup menu")
    ;(displayln (send master-object setup-complete?))
    (unless (eq? #t (send master-object setup-complete?))
      (displayln "Master Object has a setup menu that has not been completed.  Starting menu...")
      (send master-object run-setup-menu! pre-readline-input-port (current-output-port))))
        
  (start-object-reload-thread!)
    
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
    (log-message (current-logger) 'debug 'rackmud
                 (format "~atelnet login from ~v"
                         (if secure? "ssl-" "")
                         ip)
                 #f #f)
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
                     (with-handlers ([exn?
                                      (lambda (e)
                                        (log-message (current-logger)
                                                     'error 'rackmud:telnet
                                                     (exn-message e)
                                                     (exn-continuation-marks e)
                                                     #f)
                                        (loop))])
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
                     (with-handlers ([exn?
                                      (lambda (e)
                                        (loop))])
                       (define-values (in out) (tcp-accept ssl-serv))
                       (define-values (lip ip) (tcp-addresses in))
                       (define-values (sin sout) (ports->ssl-ports in out
                                                                   #:mode 'accept
                                                                   #:context ssl-ctxt
                                                                   #:close-original? #t
                                                                   #:shutdown-on-close? #f))
                       (with-handlers ([exn?
                                        (lambda (e)
                                          (log-message (current-logger)
                                                       'error 'rackmud:telnet
                                                       (exn-message e)
                                                       (exn-continuation-marks e)
                                                       #f)
                                          (loop))])
                       (add-telnet-user sin sout ip #t)))
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

  (define websock-url-prefix-length
    (length (explode-path (string->path (hash-ref cfg 'webserver:websock-url "socket")))))

  (define websock-client-url/path-list
    (map (compose string->symbol path->string)
         (explode-path (string->path
                        (hash-ref cfg 'webserver:websock-client-url)))))
  
  (define (ws-conn-req ws-conn req)
    (let ([path-elements (map (compose string->symbol path/param-path)
                              (drop (url-path (request-uri req))
                                    websock-url-prefix-length))])
      (void
       (if (list=? path-elements websock-client-url/path-list)
           (send master-object
                 on-connect
                 (new
                  custom-websock%
                  [websock-connection ws-conn]
                  [secure? (or
                            (rackmud:proxy-mode)
                            (and (string? (url-scheme (request-uri req)))
                                 (string=? "wss" (url-scheme (request-uri req)))))]
                  [ip (request-client-ip req)]
                  [preauthorized (request+object-authorized-object req)]
                  ))
           (if (is-a? master-object websocket-server<%>)
               (send master-object websock-request path-elements req)
               (ws-close! ws-conn 1008 "BAD WEBSOCKET URL"))))))

  (define (ws-req-headers request-line headers req)
    (let-values ([(obj cookies) (request->authorized-object req)])
      (values (map cookie->header cookies)
              (request->request+object req obj))))
          
  
  (when (or http-enabled? https-enabled?)
    (parameterize ([error-display-handler
                    (let ([edh (error-display-handler)])
                      (λ (msg exn) (unless (exn:fail:network? exn)
                                     (edh msg exn))))])
      (start-webserver
       (cond [(not http-enabled?) 'https]
             [(not https-enabled?) 'http]
             [else 'http+https])
       http-port
       https-port
       unix-domain-socket
       (hash-ref cfg 'webserver:web-path "./www")
       (hash-ref cfg 'webserver:servlet-url "servlet")
       (send master-object get-servlet-handler (hash-ref cfg 'webserver:servlet-url "servlet"))
       (hash-ref cfg 'webserver:websock-url "socket")
       ws-conn-req
       ws-req-headers
       (hash-ref cfg 'ssl:certificate #f)
       (hash-ref cfg 'ssl:private-key #f))))

  (define cert-thread
    (and (or https-enabled? ssl-ctxt)
         (thread (lambda ()
                   (let ([cert-evt (filesystem-change-evt (hash-ref cfg 'ssl:certificate))]
                         [pkey-evt (filesystem-change-evt (hash-ref cfg 'ssl:private-key))])
                     (let loop ()
                       (sync cert-evt pkey-evt)
                       (sleep 1/2) ; just in case there's a delay between the first change
                       (reload-certificates!)))))))

  (printf "started in ~vms\n" (round (inexact->exact (- (current-inexact-milliseconds) t0))))

  (log-message (current-logger)
               'info
               'rackmud
               (format "Startup completed in ~vms"
                       (round (inexact->exact (- (current-inexact-milliseconds) t-master))))
               #f #f)
    
  (if (rackmud:proxy-mode)
      (printf "Webserver running in proxy mode [~a].  Listening at ~a\n"
              proxy-mode
              (if unix-domain-socket
                  (format "unix:~a" unix-domain-socket)
                  (format "port ~a" http-port)))
      (begin
        (when http-enabled?
          (printf "webserver started at ~a\n"
                  (webserver-absolute-path "" (url "http" #f #f http-port #f '() '() #f))))
        (when https-enabled?
          (printf "webserver started at ~a\n"
                  (webserver-absolute-path "" (url "https" #f #f https-port #f '() '() #f))))))
    
  (define (rackmud-rebuild!)
    (define changes-or-exn
      (place-channel-put/get compiler-place #t))
    (log-message (current-logger) 'debug 'compiler-place
                 "Compiler Place reports compilation completed" #f #f)
    (match changes-or-exn
      [(cons 'changes list-of-changes) (rackmud-mark-reloads list-of-changes) #t]
      [(cons 'errors list-of-errors)
       (for ([error (in-list list-of-errors)])
         (log-message (current-logger) 'error 'compiler-place
                      (second error) #f #f))
       #f]
      [else #f]))

  (define rebuild-thread
    (thread
     (λ ()
       (let loop ()
         (sync rebuild-channel)
         (log-message (current-logger) 'info 'compiler-place
                      "Received rebuild request" #f #f)
         (log-message (current-logger) 'info 'compiler-place
                      (format "Rebuild returned result: ~v" (rackmud-rebuild!))
                      #f #f)
         (loop)))))
  (load-jwt-revokations)
  (define jwt-prune-thread
    (thread
     (letrec
         ([loop
           (λ ()
             (prune-jwt-revokation)
             (sleep (jwt-duration))
             (loop))])
       loop)))
           
                      
  (define snapshot-thread
    (thread
     (λ ()
       (let loop ()
         (sleep (database-take-snapshot-as-needed))
         (loop)))))

  (define shutdown-semaphore (make-semaphore 0))
  (define (rackmud-shutdown!)
    (displayln "Shutting down...")
    (log-message (current-logger)
                 'info
                 'rackmud
                 "Shutdown sequence begun"
                 #f #f)
    (when cert-thread (kill-thread cert-thread))
    (when telnet-thread (kill-thread telnet-thread))
    (when telnet-serv (tcp-close telnet-serv))
    (when ssl-thread (kill-thread ssl-thread))
    (when ssl-serv (tcp-close ssl-serv))
    (when jwt-prune-thread (kill-thread jwt-prune-thread))
    (prune-jwt-revokation)
    (save-jwt-revokations)
    (when (and (thread? repl-thread) (thread-running? repl-thread))
      (kill-thread repl-thread))
    (kill-thread rebuild-thread)
    (kill-thread snapshot-thread)
    (displayln "Telnet and REPL stopped...")
    (shut-down!))     

  (displayln "Running")
  (sleep 0.01)
  ;; Finally, if running in interactive mode, start the REPL thread

  (define repl-thread
    (and (hash-ref cfg 'interactive #f)
         (thread
          (lambda ()
            (define old-error-display-handler (error-display-handler))
            (error-display-handler (λ (msg exn)
                                     (unless (exn:break:terminate? exn)
                                       (old-error-display-handler msg exn))))
            (define (shutdown! [code 0])
              (semaphore-post shutdown-semaphore)
              (kill-thread repl-thread))

            (define (rebuild!)
              (channel-put rebuild-channel 'go))
              
            (parameterize ([current-namespace ns])
              (namespace-set-variable-value! 'shutdown! shutdown!)
              (namespace-set-variable-value! 'rebuild! rebuild!)
              (namespace-set-variable-value! 'exit shutdown!) ; override default exit
              (let loop () 
                (read-eval-print-loop) 
                ;; runs if EOF breaks out of REPL
                (cond[(in-tmux?) 
                      (displayln "ETATCH") ; ctrl-d shows as ^D,so now they see ^DETACH in history
                      (system "tmux detach")]
                     [else (displayln "")])
                (loop)))))))

  (capture-signal! 'SIGTERM)
  (with-handlers ([exn:break?
                   (lambda (e)
                     (log-info "User Break - Shutting Down")
                     (rackmud-shutdown!))])
    (sync shutdown-semaphore next-signal-evt)
    (rackmud-shutdown!))
  (displayln "Goodbye")
  (flush-output (current-output-port))
  (flush-output (current-error-port)))

(when pre-readline-input-port
  (close-input-port (current-input-port))
  (current-input-port pre-readline-input-port))