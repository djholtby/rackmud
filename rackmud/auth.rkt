#lang racket/base

(require net/jwt net/base64 racket/random "db.rkt")
(provide jwt-secret jwt-duration jwt-domain make-auth-jwt verify-auth-jwt refresh-jwt)

(define jwt-secret (make-parameter #f)) ; need to set the secret before using
(define jwt-duration (make-parameter (* 10 60))) ; default 10 minutes
(define jwt-domain (make-parameter #f)) ; default is to not check the domain

(define (maybe-list v)
  (and v (list v)))


;; make-auth-jwt: OID -> JWT-String

(define (make-auth-jwt oid)
  (unless (string? (jwt-secret))
    (raise-arguments-error 'make-auth-jwt "jwt key is not a string" "(jwt-secret)" (jwt-secret)))
  (encode/sign "HS384" (jwt-secret)
                #:iss (jwt-domain)
                #:aud (maybe-list (jwt-domain))
                #:iat (current-seconds)
                #:exp (+ (current-seconds) (jwt-duration))
                #:other (hasheq 'user-id oid)))

;; verify-auth-jwt: Str -> (or OID #f)

(define (verify-auth-jwt jwt)
  (unless (string? (jwt-secret))
    (raise-arguments-error 'verify-auth-jwt "jwt key is not a string" "(jwt-secret)" (jwt-secret)))
  (let ([verified-jwt (decode/verify jwt "HS384" (jwt-secret) #:iss (jwt-domain)  #:aud (jwt-domain))])
      (and verified-jwt (claims-ref verified-jwt 'user-id))))
                  

;; refresh-jwt: (or #f Str) -> (or #f (values OID Nat JWT-Str Token-Str))

(define (refresh-jwt old-token)
  (let-values ([(id duration new-token)
         (database-token-refresh old-token)])
    (if new-token
        (values id duration (make-auth-jwt id) new-token)
        (values #f #f #f #f))))