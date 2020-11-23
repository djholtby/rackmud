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
          gregor
          "objects.rkt" "auth.rkt" "parameters.rkt"
          )

(provide (all-from-out web-server/http
                       web-server/http/response
                       web-server/http/response-structs
                       web-server/http/id-cookie
                       web-server/servlet
                       web-server/servlet/servlet-structs
                       web-server/dispatchers/dispatch
                       net/url
                       net/rfc6455))

(provide logout-cookies request->authorized-object object->auth-cookies define-authorized-responder
         response/text  
         webserver-absolute-path)

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

(define jwt-cookie-name "rackmud-auth")
(define refresh-cookie-name "rackmud-token")
(define TEXT/PLAIN-MIME-TYPE #"text/plain; charset=utf-8")
(define private-key (make-secret-salt/file "COOKIE"))

(define (response/text body #:code [code 200] #:message [message #f]
                       #:headers [headers '()] #:cookies [cookies '()])
  (response/full code message (current-seconds)
                 TEXT/PLAIN-MIME-TYPE
                 (append headers (map cookie->header cookies)) (list (if (bytes? body) body (string->bytes/utf-8 (~a body))))))


(define logout-cookies
  (list (make-cookie jwt-cookie-name "" #:expires (date* 0 0 0 1 1 1970 4 0 #f 0 0 "UTC"))
        (logout-id-cookie refresh-cookie-name)))

(define (jwt->cookie jwt)
  (make-cookie jwt-cookie-name jwt #:http-only? #t #:max-age (jwt-duration)))

(define (token->cookie token duration)
  (make-id-cookie refresh-cookie-name token #:key private-key #:max-age duration))

;; get-auth-cookies: Request -> (or Str #f) (or Str #f)

(define (get-auth-cookies req)
  (values
   (findf (λ (cookie) (string=? jwt-cookie-name (client-cookie-name cookie))) (request-cookies request))
   (request-id-cookie req #:name refresh-cookie-name #:key private-key)))

;; get-authentication: Request -> (or #f Object) (listof Cookie)

(define (request->authorized-object req)
  (let-values ([(jwt token) (get-auth-cookies req)])
    (let-values ([(object duration new-jwt new-token) (get-authorization jwt token)])
      (values object
              (if new-jwt
                  (list (jwt->cookie new-jwt)
                        (token->cookie new-token duration))
                  (if object '() logout-cookies))))))

(define-syntax (define-authorized-responder stx)
  (syntax-case stx ()
    [(_ (responder-name request object) body ...)
     #'(define (responder-name request)
         (let-values ([(object auth-cookies) (request->authorized-object request)])
           (let ([resp (begin body ...)])
             (struct-copy response resp [headers (append (map cookie->header auth-cookies) (response-headers resp))]))))]))


(define (object->auth-cookies object [refresh-duration #f])
  (let-values ([(jwt token) (make-authorization object refresh-duration)])
    (list (jwt->cookie jwt)
          (token->cookie token))))
