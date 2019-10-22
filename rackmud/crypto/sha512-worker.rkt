#lang racket

(require ffi/unsafe ffi/unsafe/define openssl/libcrypto racket/random)
(provide sha512-place-main)
(define sha512-key-length 64)
(define pbkdf2-c 65536)

(define (sha512-hash* password salt)
  (error "libcrypto not available"))


(define opad (make-list sha512-key-length #x5C))
(define ipad (make-list sha512-key-length #x36))

(define (sha512-hmac password key)
  (define password-bytes (string->bytes/utf-8 password))
  (define k/prime (cond [(= (bytes-length key) sha512-key-length) key]
                        [(< (bytes-length key) sha512-key-length) (bytes-append key (make-bytes (- sha512-key-length (bytes-length key)) 0))]
                        [else (sha512-hash* key #"")]))
  (define key/list (bytes->list k/prime))
  (define key-outer (list->bytes (map bitwise-xor key/list opad)))
  (define key-inner (list->bytes (map bitwise-xor key/list ipad)))
  (sha512-hash* key-outer (sha512-hash* key-inner password-bytes)))
  


(define (PBKDF2/hmac-sha512 password salt c)
  (let loop ([i 1]
             [U/i (bytes-append salt #"\0\0\0\1")])
    (if (= i c) U/i (loop (add1 i) (sha512-hmac password U/i)))))
        

(when libcrypto
  (define-ffi-definer defcrypto libcrypto)
  (defcrypto SHA512_Init   (_fun _pointer -> _int))
  (defcrypto SHA512_Update (_fun _pointer _pointer _long -> _int))
  (defcrypto SHA512_Final  (_fun _pointer _pointer -> _int))
  (set! sha512-hash* (λ (password-bytes salt)
                      (define bytes (bytes-append password-bytes salt))
                      (define ctx (malloc 256))
                      (define result (make-bytes 64))
                      (SHA512_Init ctx)
                      (SHA512_Update ctx bytes (bytes-length bytes))
                      (SHA512_Final result ctx)
                      result)))


(define (sha512-place-main chan)
  (unless libcrypto
    (place-channel-put chan #f)
    (place-channel-put chan libcrypto-load-fail-reason)
    (exit 1))

  (let loop ()
    (define msg (place-channel-get chan))
      (match msg
        [(? eof-object?) (exit 0)]
        [(? string? password) (let ([salt (crypto-random-bytes sha512-key-length)])
                                (place-channel-put chan (cons salt (PBKDF2/hmac-sha512 password salt pbkdf2-c))))]
        [(list (? string? password) (? bytes? salt) (? bytes? hash))
         (place-channel-put chan (and (= (bytes-length salt) sha512-key-length)
                                      (= (bytes-length hash) sha512-key-length)
                                      (bytes=? hash (PBKDF2/hmac-sha512 password salt pbkdf2-c))))]
        [else (void)])
      (loop)))