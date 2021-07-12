#lang racket/base

(require  web-server/http
          web-server/http/response
          web-server/http/response-structs
          web-server/http/id-cookie
          web-server/servlet
          web-server/servlet/servlet-structs
          web-server/dispatchers/dispatch
          net/url
          net/rfc6455
          net/cookies/common
          (for-syntax racket/base)
          racket/tcp
          racket/stxparam
          racket/format
          racket/class
          racket/path
          gregor
          "objects.rkt" "auth.rkt" "parameters.rkt" "db.rkt"
          )

(provide (all-from-out web-server/http
                       web-server/http/response
                       web-server/http/response-structs
                       web-server/http/id-cookie
                       web-server/servlet
                       web-server/servlet/servlet-structs
                       web-server/dispatchers/dispatch
                       net/url
                       net/rfc6455
                       racket/path)
         parse-sig-token
         )

(provide logout-cookies request->authorized-object object->auth-cookies define-authorized-responder
         response/text web-root-path get-auth-cookies
         webserver-absolute-path servlet-path
         websock-path websock-client-path)

(define (simplify-path/param pp-lst)
  (filter (λ (pp) (positive? (string-length (path/param-path pp)))) pp-lst))


;; (webserver-absolute-path path-suffix cur-url) produces an absolute URL to path-suffix,
;;   based on the racmud:domain and rackmud:web-root-path
;;   cur-url, if specified, determines the host (if the parameter is not set) as well as the
;;   method and port.
;; webserver-absolute-path: (or/c path-string? url?) [url?] -> url?

(define (webserver-absolute-path path-suffix [cur-url (url #f #f #f #f #t '() '() #f)])
  (let ([as-url (if (url? path-suffix) path-suffix (string->url path-suffix))])
    (url->string
     (url (url-scheme cur-url)
          (url-user cur-url)
          (or (rackmud:domain) (url-host cur-url))
          (url-port cur-url) #t
          (simplify-path/param
           (cons (path/param (rackmud:web-root-path) '())
                 (url-path as-url)))
          (url-query as-url)
          (url-fragment as-url)))))

(define (->web-path-element maybe-path)
  (cond [(or (and (path-for-some-system? maybe-path)
                  (eq? 'unix (path-convention-type maybe-path)))
             (symbol? maybe-path)) maybe-path]
        [(path-for-some-system? maybe-path) ; windows path
         (apply build-path/convention-type 'unix
                (map  (λ (p) (bytes->path (path->bytes p) 'unix)) (explode-path maybe-path)))]
        [(string? maybe-path) (bytes->path-element  (string->bytes/utf-8 maybe-path) 'unix)]
        [else (bytes->path-element maybe-path 'unix)]))

(define (web-root-path . path-parts)
  (some-system-path->string
   (apply
    build-path/convention-type 'unix 
    (map ->web-path-element `(,@(explode-path
                                 (bytes->path (string->bytes/utf-8
                                               (rackmud:web-root-path))
                                              'unix))
                              ,@path-parts)))))
  


(define (servlet-path . path-parts)
  (some-system-path->string
   (apply
    build-path/convention-type 'unix 
    (map ->web-path-element `(,@(explode-path
                                 (bytes->path (string->bytes/utf-8
                                               (rackmud:web-root-path))
                                              'unix))
                              ,@(explode-path
                                 (bytes->path
                                  (string->bytes/utf-8 (rackmud:servlet-path)) 'unix))
                              ,@path-parts)))))

(define (websock-path . path-parts)
  (some-system-path->string
   (apply
    build-path/convention-type 'unix 
    (map ->web-path-element `(,@(explode-path
                                 (bytes->path (string->bytes/utf-8
                                               (rackmud:web-root-path))
                                              'unix))
                              ,@(explode-path
                                 (bytes->path
                                  (string->bytes/utf-8 (rackmud:websock-path)) 'unix))
                              ,@path-parts)))))

(define (websock-client-path)
  (websock-path (rackmud:websock-client-path)))

;;(define rackmud:websock-path (make-parameter #f))
;;(define rackmud:websock-client-path (make-parameter #f))


(define jwt-cookie-name "rackmud-auth")
(define refresh-cookie-name "rackmud-token")
(define TEXT/PLAIN-MIME-TYPE #"text/plain; charset=utf-8")
(define private-key (make-secret-salt/file "COOKIE"))

(define (response/text body #:code [code 200] #:message [message #f]
                       #:headers [headers '()] #:cookies [cookies '()])
  (response/full code message (current-seconds)
                 TEXT/PLAIN-MIME-TYPE
                 (append headers (map cookie->header cookies))
                 (list (if (bytes? body) body (string->bytes/utf-8 (~a body))))))


(define (logout-cookies)
  (list (make-cookie jwt-cookie-name "" #:expires (date* 0 0 0 1 1 1970 4 0 #f 0 0 "UTC")
                     #:domain (rackmud:domain)
                     #:path (rackmud:web-root-path))
        (make-cookie refresh-cookie-name "" #:expires (date* 0 0 0 1 1 1970 4 0 #f 0 0 "UTC")
                     #:domain (rackmud:domain)
                     #:path (rackmud:web-root-path))))

(define (jwt->cookie jwt)
  (make-cookie jwt-cookie-name jwt #:http-only? #t #:max-age (jwt-duration)
               #:domain (rackmud:domain)
               #:path (rackmud:web-root-path)))

(define (token->cookie token duration)
  (make-id-cookie refresh-cookie-name token #:key private-key #:max-age duration
                  #:domain (rackmud:domain) #:path (rackmud:web-root-path)))

;; get-auth-cookies: Request -> (or Str #f) (or Str #f)

(define (get-auth-cookies req)
  ;(eprintf "~v\n" (request-cookies req))
  (values
   (let ([v (findf (λ (cookie) (string=? jwt-cookie-name (client-cookie-name cookie)))
                   (request-cookies req))])
     (if (client-cookie? v) (client-cookie-value v) #f))
   (request-id-cookie req #:name refresh-cookie-name #:key private-key)))

;; get-authentication: Request -> (or #f Object) (listof Cookie)

(define (request->authorized-object req)
  (let-values ([(jwt token) (get-auth-cookies req)])
    (let-values ([(object duration new-jwt new-token) (get-authorization jwt token)])
      (values object
              (if new-jwt
                  (list (jwt->cookie new-jwt)
                        (token->cookie new-token duration))
                  (if object '() (logout-cookies)))))))

(define-syntax (define-authorized-responder stx)
  (syntax-case stx ()
    [(_ (responder-name request) body ...)
     #'(define (responder-name request)
         (let-values ([(object auth-cookies) (request->authorized-object request)])
           (parameterize ([signed-in-user object])
             (let ([resp (any->response (begin body ...))])
               (unless resp
                 (error 'responder-name "did not produce a response"))
               (struct-copy response resp [headers (append (map cookie->header auth-cookies)
                                                           (response-headers resp))])))))]))


(define (token-duration->cookie-duration dur)
  (case dur
    [(forever) 3141592653] ; pi seconds = 1 century...ish
    [(session) #f]
    [else dur]))


(define (object->auth-cookies object [token-duration #f])
  (let-values ([(jwt token) (make-authorization
                             object
                             (and (exact-positive-integer? token-duration) token-duration))])
    (list (jwt->cookie jwt)
          (token->cookie token (token-duration->cookie-duration token-duration)))))
