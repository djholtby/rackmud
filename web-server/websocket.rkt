#lang racket/base

;; Ugly smushing-together of net/rfc6455 with web-server/servlet, 90% copy+paste,
;; Gets you servlets, static pages, and websockets.  Allows HTTP and HTTPS together,
;; optionally the HTTP server can redirect to HTTPS 

;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require racket/contract
         racket/list
         racket/serialize
         racket/runtime-path
         racket/async-channel
         racket/unit
         (only-in racket/tcp listen-port-number?)
         (for-syntax racket/base))



(require net/rfc6455
         net/rfc6455/dispatcher
         net/rfc6455/service-mapper
         net/url
         openssl
         web-server/http
         web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/private/mime-types
         web-server/private/dispatch-server-sig
         web-server/servlet/setup
         web-server/servlet/servlet-structs
         web-server/servlet-dispatch
         web-server/dispatchers/dispatch
         web-server/stuffers
         web-server/configuration/responders
         web-server/web-server
         web-server/http/response
         web-server/http/response-structs
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         (prefix-in log: web-server/dispatchers/dispatch-log))

(struct websocket-server (shutdown ssl-context))

(define (fix-url path)
  (third (regexp-match #rx"^(/)?(.*)(/)?$" path)))


(define (root-url-quote url-string)
  (regexp-quote (string-append "/" (fix-url url-string))))



(provide/contract
 [fix-url (string? . -> . string?)]
 [root-url-quote (string? . -> . string?)]
 [websocket-server? (any/c . -> . boolean?)]
 [stop-websocket-server (websocket-server? . -> . void?)]
 [renew-websocket-server-certificate (websocket-server? path-string? path-string? . -> . void?)]
 [serve/servlet+websockets
  (((request? . -> . can-be-response?)
    ;(url? . -> . ((or/c symbol? #f) . -> . (or/c #f (-> ws-conn? void)))))
    (-> ws-conn? request? void)
    (or/c (-> bytes? (listof header?) request?
          (values (listof header?) any/c))
      (-> bytes? (listof header?)
          (values (listof header?) any/c))))
    
   (#:connection-close? boolean?
    #:listen-ip (or/c false/c string?)
    #:confirmation-channel (or/c false/c async-channel?)
    #:http-port (or/c listen-port-number? #f)
    #:ssl-port (or/c listen-port-number? #f)
    #:max-waiting exact-nonnegative-integer?
    #:http? boolean?
    #:ssl? boolean?
    #:force-ssl? boolean?
    #:ssl-cert (or/c false/c path-string?)
    #:ssl-key (or/c false/c path-string?)
    #:manager manager?
    #:servlet-namespace (listof module-path?)
    #:server-root-path path-string?
    #:websocket-path path-string?
    #:websocket-regexp regexp?
    #:stateless? boolean?
    #:stuffer (stuffer/c serializable? bytes?)
    #:extra-files-paths (listof path-string?)
    #:servlets-root path-string?
    #:servlet-current-directory path-string?
    #:file-not-found-responder (request? . -> . can-be-response?)
    #:servlet-loading-responder (url? any/c . -> . can-be-response?)
    #:servlet-responder (url? any/c . -> . can-be-response?)
    #:mime-types-path path-string?
    #:servlet-path string?
    #:servlet-regexp regexp?
    #:log-file (or/c false/c path-string? output-port?)
    #:log-format (or/c log:log-format/c log:format-req/c))
   . ->* .
   websocket-server?)])
   

(define-runtime-path default-web-root "default-web-root")



(define (stop-websocket-server wss)
  ((websocket-server-shutdown wss)))

(define (renew-websocket-server-certificate wss ssl-cert ssl-key)
  (define cxt (websocket-server-ssl-context wss))
  (when cxt
    (ssl-load-certificate-chain! cxt ssl-cert)
    (ssl-load-private-key! cxt ssl-key)))

(define (make-ssl-connect the-ctxt)
  (define-unit ssl:dispatch-server-connect@
    (import) (export dispatch-server-connect^)
    (define (port->real-ports ip op)
      (with-handlers ([exn:fail? (λ (e)
                                   (values ip op))])
        (ports->ssl-ports	ip op
                                #:close-original? #t
                                #:mode 'accept
                                #:context the-ctxt))))
  ssl:dispatch-server-connect@)

(define (dispatcher-sequence . dispatchers)
  (let loop ([ds dispatchers] [r '()])
    (cond [(null? ds) (apply sequencer:make (reverse r))]
          [(not (car ds))   (loop (cdr ds) r)]
          [(list? (car ds)) (loop (append (car ds) (cdr ds)) r)]
          [else (loop (cdr ds) (cons (car ds) r))])))

(define (serve/servlet+websockets
         servlet-manager
         ;websocket-service-mapper
         ws-conn-req
         ws-conn-headers
         #:connection-close?
         [connection-close? #f]
         #:command-line?
         [command-line? #f]
         #:confirmation-channel
         [confirmation-channel #f]
         #:listen-ip
         [listen-ip "127.0.0.1"]
         #:http-port
         [http-port 8000]
         #:http?
         [http? #t]         
         #:max-waiting
         [max-waiting 511]
         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             (response/xexpr
              `(html (head (title "Page Has Expired."))
                     (body (p "Sorry, this page has expired. Please go back.")))))
           (* 128 1024 1024))]

         #:servlet-path
         [servlet-path "/servlets/standalone.rkt"]
         #:servlet-regexp
         [servlet-regexp (regexp (format "^~a$" (regexp-quote servlet-path)))]
         #:stateless?
         [stateless? #f]
         #:stuffer
         [stuffer default-stuffer]

         #:servlet-namespace
         [servlet-namespace empty]
         #:server-root-path
         [server-root-path default-web-root]
         #:extra-files-paths
         [extra-files-paths (list (build-path server-root-path "htdocs"))]
         #:servlets-root
         [servlets-root (build-path server-root-path "htdocs")]
         #:servlet-current-directory
         [servlet-current-directory servlets-root]
         #:file-not-found-responder
         [file-not-found-responder
          (gen-file-not-found-responder
           (build-path server-root-path "conf" "not-found.html"))]
         #:servlet-loading-responder
         [responders-servlet-loading servlet-loading-responder]
         #:servlet-responder 
         [responders-servlet servlet-error-responder]

         #:mime-types-path
         [mime-types-path (let ([p (build-path server-root-path "mime.types")])
                            (if (file-exists? p)
                                p
                                (build-path default-web-root "mime.types")))]
         
         #:ssl?
         [ssl? #f]
         #:ssl-cert
         [ssl-cert (and ssl? (build-path server-root-path "server-cert.pem"))]
         #:ssl-key
         [ssl-key (and ssl? (build-path server-root-path "private-key.pem"))]
         #:force-ssl?
         [force-ssl? #f]
         #:ssl-port
         [ssl-port 8433]
         
         #:websocket-path
         [websocket-path "/socket"]
         #:websocket-regexp
         [websocket-regexp (regexp (let ([quoted-url (root-url-quote websocket-path)])
                                     (format "^/~a$|^/~a/" quoted-url quoted-url)))]
         #:log-file
         [log-file #f]
         #:log-format
         [log-format 'apache-default])
  
  (define ssl-ctxt
    (if ssl? (ssl-make-server-context 'secure) #f))
  (when ssl?
    (ssl-set-verify! ssl-ctxt #f)
    (ssl-try-verify! ssl-ctxt #f)
    (ssl-set-verify-hostname! ssl-ctxt #f)
    (ssl-load-certificate-chain! ssl-ctxt ssl-cert)
    (ssl-load-private-key! ssl-ctxt ssl-key))

  (define websocket-dispatch
    (let ([dispatch (make-general-websockets-dispatcher ws-conn-req ws-conn-headers)])
      (λ (connection request)
        (with-handlers [(exn:dispatcher?
                         (λ (e)
                           (output-response/method
                            connection
                            (response 400 #"Bad WebSocket request" (current-seconds) #f '() void)
                            (request-method request))))]
          (dispatch connection request)))))

  (define dispatcher
    (dispatcher-sequence
     (filter:make websocket-regexp websocket-dispatch)

     (and log-file (log:make #:format 
                             (if (symbol? log-format)
                                 (log:log-format->format log-format)
                                 log-format)
                             #:log-path log-file))
     (dispatch/servlet 
      servlet-manager
      #:regexp servlet-regexp
      #:stateless? stateless?
      #:stuffer stuffer
      #:current-directory servlet-current-directory
      #:manager manager
      #:responders-servlet-loading
      responders-servlet-loading
      #:responders-servlet 
      responders-servlet)

     (map (lambda (extra-files-path)
            (files:make
             #:url->path (fsmap:make-url->path extra-files-path)
             #:path->mime-type (make-path->mime-type mime-types-path)
             #:indices (list "index.html" "index.htm")))
          extra-files-paths)
     (files:make
      #:url->path (fsmap:make-url->path (build-path server-root-path "htdocs"))
      #:path->mime-type (make-path->mime-type mime-types-path)
      #:indices (list "index.html" "index.htm"))
     (lift:make (compose any->response file-not-found-responder))))
  
  (websocket-server 
   (cond
     [(and ssl? http? force-ssl?)
      (let ([shutdown-http
             (serve  #:dispatch (lift:make (λ (request)
                                             (response 301 #"Moved Permanently" (current-seconds) TEXT/HTML-MIME-TYPE
                                                       (list (make-header #"Location"
                                                                          (string->bytes/utf-8 (format "https://~a:~a~a"
                                                                                                       (request-host-ip request)
                                                                                                       ssl-port
                                                                                                       (url->string (request-uri request))))))
                                                       (λ (out-port) (write-bytes #"HTTPS Required" out-port)))))
                     #:confirmation-channel confirmation-channel
                     #:connection-close? connection-close?
                     #:listen-ip listen-ip
                     #:port http-port)]
            [shutdown-https
             (serve
              #:dispatch dispatcher
              #:confirmation-channel confirmation-channel
              #:connection-close? connection-close?
              #:listen-ip listen-ip
              #:port ssl-port
              #:max-waiting max-waiting
              #:dispatch-server-connect@  (make-ssl-connect ssl-ctxt))])
        (λ ()
          (with-handlers ([exn? void])
            (shutdown-http))
          (with-handlers ([exn? void])
            (shutdown-https))))]
     [(and ssl? http?)
      (let ([shutdown-http
             (serve
              #:dispatch dispatcher
              #:confirmation-channel confirmation-channel
              #:connection-close? connection-close?
              #:listen-ip listen-ip
              #:port http-port
              #:max-waiting max-waiting)]
            [shutdown-https
             (serve
              #:dispatch dispatcher
              #:confirmation-channel confirmation-channel
              #:connection-close? connection-close?
              #:listen-ip listen-ip
              #:port ssl-port
              #:max-waiting max-waiting
              #:dispatch-server-connect@  (make-ssl-connect ssl-ctxt))])
        (λ ()
          (with-handlers ([exn? void])
            (shutdown-http))
          (with-handlers ([exn? void])
            (shutdown-https))))]
     [ssl?
      (serve
       #:dispatch dispatcher
       #:confirmation-channel confirmation-channel
       #:connection-close? connection-close?
       #:listen-ip listen-ip
       #:port ssl-port
       #:max-waiting max-waiting
       #:dispatch-server-connect@  (make-ssl-connect ssl-ctxt))]
     [else 
      (serve
       #:dispatch dispatcher
       #:confirmation-channel confirmation-channel
       #:connection-close? connection-close?
       #:listen-ip listen-ip
       #:port http-port
       #:max-waiting max-waiting)]) ssl-ctxt))
  
