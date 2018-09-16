#lang racket

(require ffi/unsafe ffi/unsafe/define openssl/libcrypto racket/random)
(provide PBKDF2/sha512-hash PBKDF2/sha512-verify)

;(define sha512-place (place p (sha512-place-main p)))

(define here (variable-reference->module-path-index (#%variable-reference)))
(define worker-path (module-path-index-resolve (module-path-index-join "sha512-worker.rkt" here)))

(define sha512-place (dynamic-place (resolved-module-path-name worker-path) 'sha512-place-main))
(define sha512-sema (make-semaphore 1))

(define (PBKDF2/sha512-hash password)
  (unless (string? password)
    (raise-argument-error 'PBKDF2/sha512-hash "string?" password))
  (call-with-semaphore sha512-sema place-channel-put/get #f sha512-place password))

(define (PBKDF2/sha512-verify password salt hash)
  (unless (string? password)
    (raise-argument-error 'PBKDF2/sha512-verify "string?" password))
  (unless (and (bytes? salt) (= 64 (bytes-length salt)))
    (raise-argument-error 'PBKDF2/sha512-verify "sha512-bytes?" salt))
    (unless (and (bytes? hash) (= 64 (bytes-length hash)))
    (raise-argument-error 'PBKDF2/sha512-verify "sha512-bytes?" hash))
  (call-with-semaphore sha512-sema place-channel-put/get #f sha512-place (list password salt hash)))
