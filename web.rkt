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
          (for-syntax racket/base)
          racket/stxparam
          racket/class
          gregor
          json
          "objects.rkt" "auth.rkt"
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
                       ))
(provide logout-cookies request->authorized-object object->auth-cookies define-authorized-responder
         response/text response/json)

(define jwt-cookie-name "rackmud-auth")
(define refresh-cookie-name "rackmud-token")
(define TEXT/PLAIN-MIME-TYPE #"text/plain; charset=utf-8")
(define JSON-MIME-TYPE #"application/json")
(define private-key (make-secret-salt/file "COOKIE"))

(define (response/text body #:code [code 200] #:message [message #f] #:headers [headers '()] #:cookies [cookies '()])
  (response/full code message (current-seconds) TEXT/PLAIN-MIME-TYPE (append headers (map cookie->header cookies)) (list body)))

(define (response/json jsexpr #:code [code 200] #:message [message #f] #:headers [headers '()] #:cookies [cookies '()])
  (response/full code message (current-seconds) JSON-MIME-TYPE (append headers (map cookie->header cookies))
                 (jsexpr->string jsexpr)))

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
