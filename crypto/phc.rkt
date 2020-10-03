#lang racket

(provide bytes->string/64 string->bytes/64 phc unpack-phc)

(define alphabet64 (list->vector (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")))

(define (lookup/64 c)
  (define result #f)
  (for ([i (in-range 64)])
    #:final (and (char=? c (vector-ref alphabet64 i))
                 (set! result i))
    (void))
  result)

(define (params->string params)
  (define output (open-output-string))
  (define first #t)
  (hash-for-each
   params
   (λ (param value)
     (unless first
       (display "," output))
     (when first
       (set! first #f))
     (display (format "~a=~a" param value) output)))
  (begin0
    (get-output-string output)
    (close-output-port output)))

(define (phc method params salt hash)
  (format "$~a$~a$~a$~a"
          method
          (params->string params)
          (bytes->string/64 salt)
          (bytes->string/64 hash)))

(define (unpack-phc phc)
  (define parts (string-split phc "$"))
  (define l (length parts))
  (when (< l 3)
    (raise-argument-error 'unpack-phc "phc?" phc))
  (define name (first parts))
  (define param-table
    (if (= l 4 )
        (make-hasheq
         (map (λ (pair)
                (cons (string->symbol (first pair))
                      (or (string->number (second pair)) (second pair))))
              (map (λ (s)
                     (string-split s "="))
                   (string-split (second parts) ","))))
        (make-hasheq '())))

  (define salt
    (if (= l 3)
        (second parts)
        (third parts)))

  (define hash
    (if (= l 3)
        (third parts)
        (fourth parts)))
  (values (string->symbol name) param-table (string->bytes/64 salt) (string->bytes/64 hash)))


(define (string->bytes/64 s)
  (define output (open-output-bytes))
  (define input (open-input-string s))

  (define (char->64 c)
    (if (eof-object? c) 0 (lookup/64 c)))
  
  (define (char->bytes c0 c1 c2 c3)
    (define x (+ (arithmetic-shift (char->64 c0) 18)
                 (arithmetic-shift (char->64 c1) 12)
                 (arithmetic-shift (char->64 c2) 6)
                 (char->64 c3)))
    (define b (bytes (bitwise-and #xFF (arithmetic-shift x -16))
                     (bitwise-and #xFF (arithmetic-shift x -8))
                     (bitwise-and #xFF x)))
    (define b-len
      (cond [(eof-object? c0) 0]
            [(eof-object? c1) (error 'string->bytes/64 "valid base64 string" s)]
            [(eof-object? c2) 1]
            [(eof-object? c3) 2]
            [else             3]))
    (subbytes b 0 b-len))
          
  
  (let loop ()
    (define c0 (read-char input))
    (define c1 (read-char input))
    (define c2 (read-char input))
    (define c3 (read-char input))
    (write-bytes (char->bytes c0 c1 c2 c3) output)
    (unless (eof-object? c0)
      (loop)))
    
  (begin0
    (get-output-bytes output)
    (close-output-port output)))

(define (bytes->string/64 b)
  (define l (bytes-length b))
  (define output (open-output-string))
  ;; (write-bytes/64 b0 b1 b2 mod3) writes the byte-triplet b0 b1 b2 as 4 characters to output,
  ;;   * if mod3 is 0 then b2 and b1 are not padding and all 4 characters are written
  ;;   * if mod3 is 1 then b2 and b1 are padding and the last 2 characters are skipped
  ;;   * if mod3 is 2 then b2 is padding and the last character is skipped
  (define (write-bytes/64 b0 b1 b2 mod3)
    (define x (bitwise-xor
               (arithmetic-shift b0 16)
               (arithmetic-shift b1 8)
               b2))
    (write-char (vector-ref alphabet64 (bitwise-and #x3f (arithmetic-shift x -18))) output)
    (write-char (vector-ref alphabet64 (bitwise-and #x3f (arithmetic-shift x -12))) output)
    (unless (= mod3 1)
      (write-char (vector-ref alphabet64 (bitwise-and #x3f (arithmetic-shift x -6 ))) output))
    (when (zero? mod3)
      (write-char (vector-ref alphabet64 (bitwise-and #x3f x)) output)))
  
  (for ([i (in-range (quotient l 3))])
    (define b0 (bytes-ref b (* i 3)))
    (define b1 (bytes-ref b (+ 1 (* i 3))))
    (define b2 (bytes-ref b (+ 2 (* i 3))))
    (write-bytes/64 b0 b1 b2 0))
  (cond [(= 1 (remainder l 3))
         (write-bytes/64 (bytes-ref b (sub1 l)) 0 0 1)]
        [(= 2 (remainder l 3))
         (write-bytes/64 (bytes-ref b (- l 2)) (bytes-ref b (sub1 l)) 0 2)])
    
  (begin0 (get-output-string output)
          (close-output-port output)))
  
 
                
(module+ test
  (define test-bytes #"A+\335\\&1l\375")
  (displayln test-bytes)
  (displayln (bytes->string/64 test-bytes))
  (displayln (string->bytes/64 (bytes->string/64 test-bytes))))