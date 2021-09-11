#lang racket/base

(require net/jwt net/base64 racket/random "db.rkt" gregor)
(provide jwt-secret jwt-duration jwt-domain jwt-audience make-auth-jwt verify-auth-jwt auth-jwt-exp
         refresh-jwt revoke-jwt prune-jwt-revokation load-jwt-revokations save-jwt-revokations)

(define jwt-secret (make-parameter #f (λ (v) (unless (string? v) 
                                                (raise-argument-error 'jwt-secret "string?" v))
                                         v))) 
(define jwt-duration (make-parameter (* 10 60))) ; default 10 minutes
(define jwt-domain (make-parameter #f))          ; default is to not check the domain
(define jwt-audience (make-parameter '()))


(define jwt-revokation-list (make-hash))
(define revokation-semaphore (make-semaphore 1))

(define (load-jwt-revokations [filename "jwt-revokation.rktd"])
  (with-handlers ([exn:fail:filesystem? void])
    (for ([(jwt exp) (in-hash (call-with-input-file filename read))])
      (hash-set! jwt-revokation-list jwt exp))))

(define (save-jwt-revokations [filename "jwt-revokation.rktd"])
  (call-with-output-file filename #:exists 'replace
    (λ (out) (write jwt-revokation-list out))))

(define (database-check-jwt jwt-string)
  (call-with-semaphore
   revokation-semaphore
   (λ ()
     (not (hash-has-key? jwt-revokation-list jwt-string)))))

(define (revoke-jwt jwt-string expiration)
  (call-with-semaphore
   revokation-semaphore
   (λ()
     (hash-set! jwt-revokation-list jwt-string expiration))))

(define (prune-jwt-revokation)
  (call-with-semaphore
   revokation-semaphore
   (λ ()
     (define expired
       (for/fold ([acc '()])
                 ([(jwt exp) (in-hash jwt-revokation-list)])
         (if (< exp (current-seconds))
             (cons jwt acc)
             acc)))
     (for ([jwt (in-list expired)])
       (hash-remove! jwt-revokation-list jwt)))))

(define (maybe-list v)
  (and v (list v)))

(define (try-append v lst)
  (if v (cons v lst)
      (if (pair? lst) lst #f)))

;; make-auth-jwt: OID -> JWT-String

(define (make-auth-jwt oid)
  (unless (string? (jwt-secret))
    (raise-arguments-error 'make-auth-jwt "jwt key not set" "(jwt-secret)" (jwt-secret)))
  (encode/sign "HS384" (jwt-secret)
                #:iss (jwt-domain)
                #:aud (try-append (jwt-domain) (jwt-audience))
                #:iat (current-seconds)
                #:exp (+ (current-seconds) (jwt-duration))
                #:other (hasheq 'user-id oid)))

;; verify-auth-jwt: Str -> (or OID #f)

(define (verify-auth-jwt jwt)
  (unless (string? (jwt-secret))
    (raise-arguments-error 'verify-auth-jwt "jwt key not set" "(jwt-secret)" (jwt-secret)))
  (let ([verified-jwt (and (database-check-jwt jwt)
                           (decode/verify jwt "HS384" (jwt-secret)
                                          #:iss (jwt-domain)
                                          #:aud (jwt-domain)))])
    (cond [verified-jwt (claims-ref verified-jwt 'user-id)]
          [else (log-message (current-logger)
                             'warning (format "Recieved an invalid JWT - ~a" jwt) #f #f)])))
                  

;; auth-jwt-exp: Str -> (or DateTime #f)

(define (auth-jwt-exp jwt)
  (unless (string? (jwt-secret))
    (raise-arguments-error 'verify-auth-jwt "jwt key not set" "(jwt-secret)" (jwt-secret)))
  (let ([verified-jwt (and (database-check-jwt jwt)
                           (decode/verify jwt "HS384" (jwt-secret)
                                          #:iss (jwt-domain)
                                          #:aud (jwt-domain)))])
      (and verified-jwt (posix->datetime (expiration-date verified-jwt)))))
  

;; refresh-jwt: (or #f Str) -> (or #f (values OID Nat JWT-Str Token-Str))

(define (refresh-jwt old-token)
  (let-values ([(id duration new-token)
         (database-token-refresh old-token)])
    (if new-token
        (values id duration (make-auth-jwt id) new-token)
        (begin
          (when old-token ; if old-token is #f it just means this isn't a refresh, it's a login
            (log-message (current-logger)
                         'warning 'rackmud:auth (format "JWT refresh failed - id=~a token=~a"
                                                        id old-token) #f #f))
          (values #f #f #f #f)))))