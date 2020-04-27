#lang racket/base

(provide lazy-ref lazy-ref? lazy-ref:weak lazy-ref:weak? lazy-ref-id lazy-ref-obj set-lazy-ref-obj! id->lazy-ref)

(define (mud-ref-print lr port mode)
  (case mode
    ; write mode or print mode
    [(#t) (write-string (format "#{~v~a}" (lazy-ref-id lr) (if (lazy-ref:weak? lr) "w" "")) port)]
    ; display mode
    [(#f) (write-string (format "(~a ~a ~a)" (if (lazy-ref:weak? lr) "lazy-ref:weak" "lazy-ref") (lazy-ref-id lr) (lazy-ref-obj lr)) port)]
    ; print mode
    [else  (write-string (format "(~a ~v ~a)" (if (lazy-ref:weak? lr) "lazy-ref:weak" "lazy-ref") (lazy-ref-id lr) (lazy-ref-obj lr)) port)]))

(define (saved-object=? a b)
  (and (lazy-ref? b)
       (eqv? (lazy-ref-id a)
             (lazy-ref-id b))))

(define (id->lazy-ref id)
  (lazy-ref id #f))

(struct lazy-ref (id [obj #:mutable]) #:transparent
  #:methods gen:custom-write
  [(define write-proc mud-ref-print)]
  #:methods gen:equal+hash
  [(define equal-proc (lambda (a b r?) (saved-object=? a b)))
   (define hash-proc  (lambda (l hash-code) (hash-code (lazy-ref-id l))))
   (define hash2-proc (lambda (l hash-code) (hash-code (lazy-ref-id l))))])

(struct lazy-ref:weak lazy-ref ()
  #:methods gen:custom-write
  [(define write-proc mud-ref-print)]
  #:methods gen:equal+hash
  [(define equal-proc (lambda (a b r?) (saved-object=? a b)))
   (define hash-proc  (lambda (l hash-code) (hash-code (lazy-ref-id l))))
   (define hash2-proc (lambda (l hash-code) (hash-code (lazy-ref-id l))))])
