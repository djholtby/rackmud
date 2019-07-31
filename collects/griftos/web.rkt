#lang racket/base

(require  web-server/http
          web-server/http/response
          web-server/http/response-structs
          web-server/http/id-cookie
          web-server/servlet
          web-server/servlet/servlet-structs
          net/url
          net/rfc6455
          (for-syntax racket/base)
          racket/stxparam)

(provide (all-from-out web-server/http
                       web-server/http/response
                       web-server/http/response-structs
                       web-server/http/id-cookie
                       web-server/servlet
                       web-server/servlet/servlet-structs
                       net/url
                       net/rfc6455
                       ))

(provide auth auth? auth-id protected-page authed-page logout-headers)

(struct auth
  (salt-path
   login-url
   redirect-cookie-name
   cookie-name
   ; ...?
   ))

(define-syntax-parameter auth-id
  (lambda (stx)
    (raise-syntax-error #f "use outside the context of an authorized or protected page body" stx)))

;(protected-page request auth-settings id body ...) binds the authentication value (specified by auth-settings) from request to id 
;   and evaluates expressions (body  ...) using this binding.  If no user is authenticated, will instead redirect to the login page

(define-syntax (protected-page stx)
  (syntax-case stx ()
    [(_ request auth-settings body ...)
     #'(let ([auth-id (request-id-cookie request
                                    #:name (auth-cookie-name auth-settings)
                                    #:key (make-secret-salt/file (auth-salt-path auth-settings)))])
         (if auth-id
             (begin body ...)
             (redirect-to (auth-login-url auth-settings)
                          see-other
                          #:headers (list (cookie->header (make-cookie (auth-redirect-cookie-name auth-settings)
                                                                       (url->string (request-uri request))))))))]))

;(authed-page request auth-settings id body ...) binds the authentication value (specified by auth-settings) from request to id 
;   and evaluates expressions (body  ...) using this binding.  If no user is authenticated, id will be bound to #f

(define-syntax (authed-page stx)
  (syntax-case stx ()
    [(_ request auth-settings body ...)
    #'(let ([auth-id (request-id-cookie request
                                   #:name (auth-cookie-name auth-settings)
                                   #:key (make-secret-salt/file (auth-salt-path auth-settings)))])
        body ...)]))


;; (logout-headers auth-settings) generates unset cookie headers to expire the login-redirect and authentication cookies

(define (logout-headers auth-settings)
  (list
   (cookie->header (logout-id-cookie (auth-cookie-name auth-settings)))
   (cookie->header (make-cookie (auth-redirect-cookie-name auth-settings) "" #:expires (seconds->date 0)))))

