#lang racket


(require json)

(define MSDP_VAR 1)
(define MSDP_VAL 2)
(define MSDP_TABLE_OPEN 3)
(define MSDP_TABLE_CLOSE 4)
(define MSDP_ARRAY_OPEN 5)
(define MSDP_ARRAY_CLOSE 6)

(provide read-msdp write-msdp msdp->bytes bytes->msdp)


; (read-msdp i) reads an MSDP Variable . Value pair from port i
; read-msdp : Input-Port -> (pairof Sym MSDP-Value)
; requires: port i contains a valid MSDP Variable . Value pair

(define (read-msdp i)
  ; (get-var) reads a variable name from port i
  ; get-var:  -> Sym
  ; requires: next byte in i is MSDP_VAR followed by valid MSDP data
  (define (get-var)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv MSDP_VAR)) (error 'read-msdp "Expected MSDP_VAR but saw ~v isntead" bv))
    (let loop ([acc (open-output-bytes)])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading VAR name")]
            [(memv b (list MSDP_TABLE_OPEN MSDP_TABLE_CLOSE MSDP_ARRAY_OPEN MSDP_ARRAY_CLOSE MSDP_VAR )) (error 'read-msdp "Unexpected value: ~v, acc=~a" b (get-output-string acc))]
            [(= b MSDP_VAL) (string->symbol (bytes->string/utf-8 (get-output-bytes acc)))]
            [else (write-byte (read-byte i) acc) (loop acc)])))
  ; (get-val) reads a MSDP value from port i
  ; get-val: -> MSDP-Value
  ; requires: next byte in i is MSDP_VAL followed by valid MSDP data
  (define (get-val)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv MSDP_VAL)) (error 'read-msdp "Expected MSDP_VAL but saw ~v instead" bv))
    (define b (peek-byte i))
    (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading VAL")]
          [(= b MSDP_TABLE_OPEN) (get-table)]
          [(= b MSDP_ARRAY_OPEN) (get-array)]
          [(memv b (list MSDP_TABLE_CLOSE MSDP_ARRAY_CLOSE MSDP_VAR MSDP_VAL)) (error 'read-msdp "Expected value, MSDP_TABLE_OPEN, or MSSDP_ARRAY_OPEN, but saw ~v" b)]
          [else (get-atomic)]))
  ; (get-table) reads a MSDP table from port i
  ; get-table: -> (hashof Sym MSDP-Value)
  ; requires: next byte in i is MSDP_OPEN_TABLE followed by valid MSDP data
  (define (get-table)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv MSDP_TABLE_OPEN)) (error 'read-msdp "Expected MSDP_TABLE_OPEN but saw ~v instead" bv))
    (let loop ([acc empty])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading table")]
            [(= b MSDP_VAR) (loop (cons (read-msdp i) acc))]
            [(= b MSDP_TABLE_CLOSE)
             (read-byte i) ; chomp the table_close
             (make-immutable-hasheq acc)] 
            [else (error 'read-msdp "Expected additional MSDP_VAR or MSDP_TABLE_CLOSE while reading table, but saw ~v, acc = ~v" b acc)])))
  ; (get-array) reads a MSDP array from port i
  ; get-array: -> (listof MSDP-Value)
  ; requires: next byte in i is MSDP_OPEN_ARRAY followed by valid MSDP data
  (define (get-array)
    (define bv (read-byte i))
    (unless (and (not (eof-object? bv)) (= bv MSDP_ARRAY_OPEN)) (error 'read-msdp "Expected MSDP_ARRAY_OPEN but saw ~v instead" bv))
    (let loop ([acc empty])
      (define b (peek-byte i))
      (cond [(eof-object? b) (error 'read-msdp "unexpected EOM reading array")]
            [(= b MSDP_VAL) (loop (cons (get-val) acc))]
            [(= b MSDP_ARRAY_CLOSE)
             (read-byte i) ; chomp the array_close
             (reverse acc)]
            [else (error 'read-msdp "Expected additional MSDP_VAL or MSDP_ARRAY_CLOSE while reading array, but saw ~v" b)])))
  ; (get-atomic) reads an atomic MSDP value
  ; get-atomic: -> (anyof Num Bool Str)
  ; requires: next byte in i is NOT MSDP_ARRAY_OPEN or MSDP_TABLE_OPEN
  (define (get-atomic)
    (let loop ([acc (open-output-bytes)])
      (define b (peek-byte i))
      (cond [(memv b (list eof MSDP_VAR MSDP_VAL MSDP_ARRAY_CLOSE MSDP_TABLE_CLOSE)) (string->value (get-output-string acc))]
            [(memv b (list MSDP_ARRAY_OPEN MSDP_TABLE_OPEN)) (error 'read-msdp "Unexpected MSDP_~a_OPEN token while reading string" (if (= b MSDP_ARRAY_OPEN) "ARRAY" "TABLE"))]
            [else (write-byte (read-byte i) acc) (loop acc)])))

  (cons (get-var) (get-val)))


(define (string->value s)
  (define n (string->number s))
  (cond [n n]
        [(string=? s "true") #t]
        [(string=? s "false") #f]
        [else s]))
        
    

(define (bytes->msdp bytes)
  (define i (open-input-bytes bytes))
  (define result (read-msdp i))
  (close-input-port i)
  result)


(define (write-msdp* msdp-pair o)
  (define (write-msdp*/val val)
    (write-byte MSDP_VAL o)
    (cond [(hash? val)
           (write-byte MSDP_TABLE_OPEN o)
           (hash-for-each val (λ (var val) (write-msdp* (cons var val) o)))
           (write-byte MSDP_TABLE_CLOSE o)]
          [(list? val)
           (write-byte MSDP_ARRAY_OPEN o)
           (for-each (λ (val) (write-msdp*/val val)) val)
           (write-byte MSDP_ARRAY_CLOSE o)]
          [(boolean? val)
           (write-bytes (string->bytes/utf-8 (if val "true" "false")) o)]
          [(string? val)
           (write-bytes (string->bytes/utf-8 val) o)]
          [(number? val)
           (write-bytes (string->bytes/utf-8 (number->string val)) o)]))
  (define val (cdr msdp-pair))
  (write-byte MSDP_VAR o)
  (write-bytes (string->bytes/utf-8 (symbol->string (car msdp-pair))) o)
  (write-msdp*/val val))

(define (write-msdp msdp-pair o)
  (unless (and (cons? msdp-pair)
               (symbol? (car msdp-pair))
               (jsexpr? (cdr msdp-pair)))
    (raise-argument-error 'msdp->bytes "msdp-pair?" msdp-pair))
  (write-msdp* msdp-pair o))

(define (msdp->bytes msdp-pair)
  (unless (and (cons? msdp-pair)
               (symbol? (car msdp-pair))
               (jsexpr? (cdr msdp-pair)))
    (raise-argument-error 'msdp->bytes "msdp-pair?" msdp-pair))
  (define result (open-output-bytes))
  (write-msdp* msdp-pair result)
  (get-output-bytes result))



(module+ test
  (define test-bytes (list->bytes (append (list MSDP_VAR)
                                          (bytes->list #"ROOM")
                                          (list MSDP_VAL MSDP_TABLE_OPEN MSDP_VAR)
                                          (bytes->list #"VNUM")
                                          (list MSDP_VAL)
                                          (bytes->list #"6008")
                                          (list MSDP_VAR)
                                          (bytes->list #"FOO")
                                          (list MSDP_VAL MSDP_ARRAY_OPEN MSDP_VAL)
                                          (bytes->list #"true")
                                          (list MSDP_VAL)
                                          (bytes->list #"6.67e-11")
                                          (list MSDP_VAL MSDP_TABLE_OPEN)
                                          (list MSDP_VAR)
                                          (bytes->list #"X")
                                          (list MSDP_VAL)
                                          (bytes->list #"X")
                                          (list MSDP_TABLE_CLOSE MSDP_ARRAY_CLOSE MSDP_TABLE_CLOSE))))
  (define m (bytes->msdp test-bytes))
  m
  (msdp->bytes m)
  test-bytes
  (bytes->msdp (msdp->bytes m))
  )