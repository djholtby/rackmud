#lang racket

(require racket/runtime-path racket/random ffi/unsafe ffi/unsafe/define)

(provide sodium-place-main)


(define-runtime-path libsodium-so  (list 'so "libsodium"))
(define libsodium-fail-reason #f)
(define libsodium (with-handlers ([exn:fail? (λ (x) (set! libsodium-fail-reason (exn-message x))
                                               #f)])
                    (ffi-lib libsodium-so '("23" "22" "21" "20" "19"
                                                 "1.0.16" "1.0.15" "1.0.14" "1.0.13" "1.0.12" "1.0.11" "1.0.10" "1.0.9" #f)))) ;; Why oh why can't we set a minimum???

(define sodium-ops-limit 10)
(define sodium-mem-limit (* 32 1024 1024))
(define sodium-alg #f)
(define sodium-salt-length #f)
(define sodium-key-length 32)
(define sodium-hash void)

(when libsodium
  (define-ffi-definer defsodium  libsodium)
  (defsodium sodium_init (_fun -> _int))
  (sodium_init)
  (defsodium crypto_pwhash_passwd_min (_fun -> _size))
  (defsodium crypto_pwhash_bytes_min (_fun -> _size))
  (defsodium crypto_pwhash_bytes_max (_fun -> _size))
  (defsodium crypto_pwhash_alg_default (_fun -> _int))
  (defsodium crypto_pwhash_saltbytes (_fun -> _size))

  (set! sodium-alg (crypto_pwhash_alg_default))
  (set! sodium-salt-length (crypto_pwhash_saltbytes))
  (define sodium-bytes-min (crypto_pwhash_bytes_min))
  (define sodium-bytes-max (crypto_pwhash_bytes_max))
  (set! sodium-key-length (min sodium-bytes-max (max sodium-key-length sodium-bytes-min)))
  (defsodium crypto_pwhash (_fun _bytes _ullong _bytes _ullong _bytes _ullong _size _int -> _int))

  (set! sodium-hash
        (λ (password salt)
          (define password-bytes (string->bytes/utf-8 password))
          (define result (make-bytes sodium-key-length))
          (unless (zero? (crypto_pwhash result sodium-key-length password-bytes (bytes-length password-bytes) salt sodium-ops-limit sodium-mem-limit sodium-alg))
            (error 'sodium-pwhash "crypto_phash returned non-zero code.  Out of memory?"))
          result)))

(define (sodium-place-main chan)
  (unless libsodium
    (place-channel-put chan #f)
    (place-channel-put chan libsodium-fail-reason)
    (exit 1))
  (place-channel-put chan #t)
 
  (let loop ()
    (define msg (place-channel-get chan))
    (match msg
      ['get-key-length (place-channel-put chan sodium-key-length)]
      [(? eof-object?) (exit 0)]
      [(? string? password) (let ([salt (crypto-random-bytes sodium-salt-length)]) (place-channel-put chan (cons salt (sodium-hash password salt))))]
      [(list (? string? password) (? bytes? salt) (? bytes? hash))
       (place-channel-put chan (and (= (bytes-length salt) sodium-salt-length)
                                    (= (bytes-length hash) sodium-key-length)
                          (bytes=? (sodium-hash password salt) hash)))]
      [else (void)])
    (loop)))
    
    
    
    

  
