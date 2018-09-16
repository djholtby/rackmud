#lang racket
(require racket/random "phc.rkt")
(provide Lyra2 Lyra2/phc Lyra2/verify-phc lyra2-default-cost lyra2-default-row-count lyra2-default-col-count)




(define blen 64)
(define blen/8 (/ blen 8))

(define blake2b_IV
  '#(#x6a09e667f3bcc908
     #xbb67ae8584caa73b
     #x3c6ef372fe94f82b
     #xa54ff53a5f1d36f1
     #x510e527fade682d1
     #x9b05688c2b3e6c1f
     #x1f83d9abfb41bd6b
     #x5be0cd19137e2179))

(define (bytes->vector/64 b)
  (build-vector (ceiling (/ (bytes-length b) 8))
                (λ (i) (integer-bytes->integer b #f (system-big-endian?) (* i 8) (+ 8 (* i 8))))))

(define (vector->bytes/64 v len dest)
  (for ([i (in-range len)])
    (define bytes64 (integer->integer-bytes (vector-ref v i) 8 #f))
    (bytes-copy! dest (* 8 i) bytes64)))

;; (make-sponge) returns a newly initialized sponge with the blake2b initial value
;; make-sponge: Void -> Sponge

(define (make-sponge)
  (vector-append (make-vector blen/8 0) blake2b_IV))

(define (rotr64 w c)
  (bitwise-ior (arithmetic-shift w (- c))
               (arithmetic-shift w (- 64 c))))
;; (blake_G v i j k l) performs blake's G function on vector v with the a b c d coming from index i j k and l
;; blaks_G: (vectof UInt64) Nat Nat Nat Nat
;; requires: (vector-length v == 16)
;;           0 <= i,j,k,l < 16

(define (blake_G v i j k l)
  (define a (vector-ref v i))
  (define b (vector-ref v j))
  (define c (vector-ref v k))
  (define d (vector-ref v l))
  (set! a (+ a b))
  (set! d (rotr64 (bitwise-xor d a) 32))
  (set! c (+ c d))
  (set! b (rotr64 (bitwise-xor b c) 24))
  (set! a (+ a b))
  (set! d (rotr64 (bitwise-xor d a) 16))
  (set! c (+ c d))
  (set! b (rotr64 (bitwise-xor b c) 63))
  (vector-set! v i (bitwise-and a #xFFFFFFFFFFFFFFFF))
  (vector-set! v j (bitwise-and b #xFFFFFFFFFFFFFFFF))
  (vector-set! v k (bitwise-and c #xFFFFFFFFFFFFFFFF))
  (vector-set! v l (bitwise-and d #xFFFFFFFFFFFFFFFF)))


;; (blake2b v) performs 12 iterations of Blake's permutation on sponge v

(define (blake2b v)
  (for ([i (in-range 12)])
    (blake_G v 0 4 8 12)
    (blake_G v 1 5 9 13)
    (blake_G v 2 6 10 14)
    (blake_G v 3 7 11 15)
    (blake_G v 0 5 10 15)
    (blake_G v 1 6 11 12)
    (blake_G v 2 7 8 13)
    (blake_G v 3 4 9 14)))

;; (blake2b v) performs a single iteration of Blake's permutation on sponge v

(define (blake2b/reduced v)
  (blake_G v 0 4 8 12)
  (blake_G v 1 5 9 13)
  (blake_G v 2 6 10 14)
  (blake_G v 3 7 11 15)
  (blake_G v 0 5 10 15)
  (blake_G v 1 6 11 12)
  (blake_G v 2 7 8 13)
  (blake_G v 3 4 9 14))


;; (pad b) pads bytes b with the bitpattern 10*1 to ensure that the total number of bytes is divisible by blen
;; pad: Bytes -> Bytes

(define (pad b)
  (define new-len (* blen (ceiling (/ (bytes-length b) blen))))
  (define pad-len (- new-len (bytes-length b)))
  (bytes->vector/64
   (if (zero? pad-len) b
       (let ([pad-bytes (make-bytes pad-len 0)])
         (if (= 1 pad-len)
             (bytes-set! pad-bytes 0 #x81)
             (begin
               (bytes-set! pad-bytes 0 #x80)
               (bytes-set! pad-bytes (sub1 pad-len) #x81)))
         (bytes-append b pad-bytes)))))


;; (absorb sponge blocks) absorbs the blocks into the sponge
;; absorb: Sponge (vectof UInt64) -> Void
;; requires: (vector-length blocks) is a multiple of blen/8
;; effects: sponge has absorbed new bytes

(define (absorb sponge blocks)
  (define loops (/ (vector-length blocks) blen/8))
  (unless (exact-positive-integer? loops) (error 'absorb (format "block length must be a multiple of ~v" blen/8)))
  (for ([i (in-range loops)])
    (define offset (* i blen/8))
    (for ([j (in-range blen/8)])
      (define block-word (vector-ref blocks (+ j offset)))
      (define sponge-word (vector-ref sponge j))
      (vector-set! sponge j (bitwise-xor block-word sponge-word)))
    (blake2b sponge)))

;; (absorb/reduced sponge blocks) absorbs the blocks into the sponge, using the reduced cost permutator
;; absorb: Sponge (listof Int64) -> Void
;; requires: (length blocks) is a multiple of blen/8
;; effects: sponge has absorbed new bytes

(define (absorb/reduced sponge blocks)
  (define loops (/ (vector-length blocks) blen/8))
  (unless (exact-positive-integer? loops) (error 'absorb (format "block length must be a multiple of ~v" blen/8)))
  (for ([i (in-range loops)])
    (define offset (* i blen/8))
    (for ([j (in-range blen/8)])
      (define block-word (vector-ref blocks (+ j offset)))
      (define sponge-word (vector-ref sponge j))
      (vector-set! sponge j (bitwise-xor block-word sponge-word)))
    (blake2b/reduced sponge)))

;; (squeeze/row sponge C row) for all columns i in [0,C) squeezes the sponge into
;;   row[i]

(define (squeeze/row sponge C row)
  (for ([i (in-range C)])
    (for ([j (in-range blen/8)])
      (vector-set! (vector-ref row i)
                   j
                   (vector-ref sponge j))
      (blake2b/reduced sponge))))

;; (squeeze/bytes sponge outlen) squeezes outlen bytes from the sponge
;; squeeze/bytes: Sponge Nat -> Bytes

(define (squeeze/bytes sponge outlen)
  (define full-blocks (quotient outlen blen))
  (define rem (remainder outlen blen))
  (define result (make-bytes outlen 0))
  (define sponge-bytes (make-bytes blen 0))
  (for ([i (in-range full-blocks)])
    ;(define (vector->bytes/64 v len dest)
    (vector->bytes/64 sponge blen/8 sponge-bytes)
    (bytes-copy! result (* i blen) sponge-bytes)
    (blake2b sponge))
  (vector->bytes/64 sponge blen/8 sponge-bytes)
  (bytes-copy! result (* full-blocks blen) sponge-bytes 0 rem)
  result)
  
;; (duplex/row/setup sponge C dest src) for all columns i in [0,C), absorbs src[i] and
;;   then copies the sponge to dest[i]

(define (duplex/row/setup sponge C dest src)
  (for ([i (in-range C)])
    (absorb/reduced sponge (vector-ref src i))
    (vector-copy! (vector-ref dest i) 0
                  sponge 0 blen/8)))

;; (dupliex/row sponge C row) for all columns i in [0,C) absorbs row[i] and then copies
;;    the sponge back to row[i]

(define (duplex/row sponge C row)
  (duplex/row/setup sponge C row row))

;; (duplex/block sponge block) absorbs a single block into the sponge, and then returns the first word of
;;   the sponge
(define (duplex/block sponge block)
  (absorb sponge block)
  (vector-ref sponge 0))

(define lyra2-default-cost 1)
(define lyra2-default-row-count 64)
(define lyra2-default-col-count 64)

(define (Lyra2/phc password [outlen 64] [t_cost lyra2-default-cost] [R lyra2-default-row-count] [C lyra2-default-col-count])
  (define salt (crypto-random-bytes outlen))
  (define hash (Lyra2 password salt outlen t_cost R C ))
  (phc 'lyra2 (make-immutable-hash `((t . ,t_cost) (r . ,R) (c . ,C) (len . ,outlen))) salt hash))

(define (Lyra2/verify-phc password phc)
  (define-values (name params salt hash)
    (unpack-phc phc))
  (unless (symbol=? name 'lyra2)
    (error 'Lyra2/verify-phc (format "Attempting to verify a PHC string that states the method is ~a" name)))

  (define password/hash
    (Lyra2 password
           salt
           (hash-ref params 'len (λ () (bytes-length hash)))
           (hash-ref params 't (λ () lyra2-default-cost))
           (hash-ref params 'r (λ () lyra2-default-row-count))
           (hash-ref params 'c (λ () lyra2-default-col-count))))
  (bytes=? password/hash hash))

    

(define (Lyra2 password salt outlen [t_cost lyra2-default-cost] [R lyra2-default-row-count] [C lyra2-default-col-count])
  (define password-bytes (string->bytes/utf-8 password))
  (define sponge (make-sponge))
  (define params (bytes-append
                  (integer->integer-bytes outlen 4 #f)
                  (integer->integer-bytes (bytes-length password-bytes) 4 #f)
                  (integer->integer-bytes (bytes-length salt) 4 #f)
                  (integer->integer-bytes t_cost 4 #f)
                  (integer->integer-bytes R 4 #f)
                  (integer->integer-bytes C 4 #f)))
  (absorb sponge (pad (bytes-append password-bytes salt params)))
  
  (define M
    (build-vector R
                  (λ (row)
                    (build-vector C (λ (col)
                                      (make-vector blen/8 0))))))

  (squeeze/row sponge C (vector-ref M 0))
  (for ([i (in-range 1 R)])
    (duplex/row/setup sponge C
                      (vector-ref M i)
                      (vector-ref M (sub1 i))))
  (define row 0)
  (define col 0)
  (for ([i (in-range (* R t_cost))])
    (duplex/row sponge C (vector-ref M row))
    (set! col (remainder (vector-ref (vector-ref (vector-ref M row) (sub1 C)) 0) C))
    (set! row (remainder (duplex/block sponge (vector-ref (vector-ref M row) col)) R)))
  (absorb sponge (pad salt))
  
  (squeeze/bytes sponge outlen))
  
