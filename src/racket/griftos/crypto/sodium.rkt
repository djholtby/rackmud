#lang racket

(provide have-sodium? sodium-fail-reason sodium-key-len sodium-hash sodium-verify)
;(require "sodium-worker.rkt")

(define sodium-fail-reason #f)
;(define sodium-place (place p (sodium-place-main p)))
(define here (variable-reference->module-path-index (#%variable-reference)))
(define worker-path (module-path-index-resolve (module-path-index-join "sodium-worker.rkt" here)))


(define sodium-place (dynamic-place (resolved-module-path-name worker-path) 'sodium-place-main))
(define have-sodium? (place-channel-get sodium-place))
(unless have-sodium? (set! sodium-fail-reason (place-channel-get sodium-place)))
(define sodium-key-len (if have-sodium? (place-channel-put/get sodium-place 'get-key-length) #f))
(define sodium-semaphore (make-semaphore 1))

(define (sodium-hash password)
  (unless (string? password)
    (raise-argument-error 'sodium-hash "string?" password))
  (unless have-sodium? (error 'sodium-hash "libsodium >= 1.0.9 not available"))
  (call-with-semaphore sodium-semaphore place-channel-put/get #f sodium-place password))

(define (sodium-verify password salt hash)
  (unless have-sodium? (error 'sodium-hash "libsodium >= 1.0.9 not available"))
  (call-with-semaphore sodium-semaphore place-channel-put/get #f sodium-place (list password salt hash)))