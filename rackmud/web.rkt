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
          "objects.rkt"
          "db.rkt"
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


(provide make-login-cookie expire-all-tokens validate-login-cookie auth-account protected-page authed-page logout-headers)

(define login-cookie-name "rackmud-login-auth")
(define redirect-cookie-name "rackmud-login-redirect")
(define private-key (make-secret-salt/file "COOKIE"))

(define (expire-all-tokens acct)
  (database-expire-all-tokens (send acct get-id)))

(define (make-login-cookie acct [expires "9999-01-01T00:00:00Z"] [secure? #f])
  (make-id-cookie login-cookie-name
                  (database-make-token (send acct get-id) expires)
                  #:key private-key
                  #:secure? secure?
                  #:http-only? #t))

(define (validate-login-cookie request)
  (define cookie-value (request-id-cookie request
                                          #:name login-cookie-name
                                          #:key private-key))
  (and cookie-value (make-lazyref (database-verify-token cookie-value))))
                    
                    
(define-syntax-parameter auth-account
  (lambda (stx)
    (raise-syntax-error #f "use outside the context of an authorized or protected page body" stx)))

;(protected-page request auth-settings id body ...) binds the authentication value (specified by auth-settings) from request to id 
;   and evaluates expressions (body  ...) using this binding.  If no user is authenticated, will instead redirect to the login page

(define-syntax (protected-page stx)
  (syntax-case stx ()
    [(_ [request] body ...)
     #'(let ([auth-account (validate-login-cookie request)])
         (if auth-id
             (begin body ...)
             (redirect-to login-url
                          see-other
                          #:headers (list (cookie->header (make-cookie redirect-cookie-name auth-settings
                                                                       (url->string (request-uri request))))))))]))

;(authed-page request auth-settings id body ...) binds the authentication value (specified by auth-settings) from request to id 
;   and evaluates expressions (body  ...) using this binding.  If no user is authenticated, id will be bound to #f

(define-syntax (authed-page stx)
  (syntax-case stx ()
    [(_ [request]  body ...)
     #'(let ([auth-account (validate-login-cookie request)])
         body ...)]))


;; (logout-headers auth-settings) generates unset cookie headers to expire the login-redirect and authentication cookies

(define (logout-headers auth-settings)
  (list
   (cookie->header (logout-id-cookie login-cookie-name))
   (cookie->header (make-cookie redirect-cookie-name "" #:expires (seconds->date 0)))))

