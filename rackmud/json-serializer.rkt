#lang racket/base

(require racket/set racket/undefined racket/vector racket/match racket/list json gregor net/base64 versioned-box "lazy-ref.rkt")

(provide value->jsexpr jsexpr->value)

(define (set-code st)
  (string-append
   (if (immutable? st) "" "m")
   (if (set-eq? st) "q" (if (set-eqv? st) "v" "e"))))

(define (hash-code ht)
  (string-append
   (if (immutable? ht) "" "m")
   (if (hash-eq? ht) "q" (if (hash-eqv? ht) "v" "e"))))

(define (value->jsexpr v)
  (cond
    [(or (boolean? v)
         (exact-integer? v)
         (and (real? v) (rational? v))
         (eq? v (json-null)))
     v]
    [(list? v) (map value->jsexpr v)]
    [(box? v)
     `#hasheq((|(RKT)| . "box")
              (value . ,(value->jsexpr (unbox v))))]
    [(vbox? v)
     `#hasheq((|(RKT)| . "vbox")
              (value . ,(value->jsexpr (vbox-ref v))))]
    [(lazy-ref? v)
     `#hasheq((|(RKT)| . "lazy-ref")
              (value . (lazy-ref-id v)))]
    [(bytes? v)
     (string-append "#b" (bytes->string/utf-8 (base64-encode v "")))]
    [(char? v)
     `#hasheq((|(RKT)| . "char")
              (value . ,(char->integer v)))]
    [(symbol? v) (symbol->string v)]
    [(void? v) "#void"]
    [(eq? undefined v) "#undef"]
    [(string? v) (string-append "#s" v)]
    [(moment? v) (string-append "#m" (moment->iso8601/tzid v))]
    [(or (set? v) (set-mutable? v))
     `#hasheq((|(RKT)| . "set")
              (type . ,(set-code v))
              (value . ,(map value->jsexpr (set->list v))))]
    [(vector? v)
     `#hasheq((|(RKT)| . "vector")
              (value . ,(vector->list (vector-map value->jsexpr v))))]
    [(prefab-struct-key v)
     `#hasheq((|(RKT)| . "struct")
              (key . `(value->jsexpr (prefab-struct-key v)))
              (value . `(map value->jsexpr (rest (vector->list (struct->vector v))))))]
    [(and (hash? v)
          (andmap symbol? (hash-keys v)))
     (make-hasheq (cons (cons '|(HT)| (hash-code v))
                        (hash-map v (lambda (k v)
                                      (cons k (value->jsexpr v))))))]
    [(hash? v)
     `#hasheq((|(RKT)| . "hash")
              (|(HT)| . ,(hash-code v))
              (value . ,(hash-map v (lambda (k v)
                                      `#hasheq((key . ,(value->jsexpr k))
                                               (value . ,(value->jsexpr v)))))))]))







(define set-constructors (hash "e" set "q" seteq "v" seteqv
                               "me" mutable-set "mq" mutable-seteq "mv" mutable-seteqv))



(define hash-constructors (hash "e" hash "q" hasheq "v" hasheqv
                                "me" make-hash "mq" make-hasheq "mv" make-hasheqv))

(define (code->hash c)
  (hash-ref hash-constructors c))

(define (code->set c)
  (hash-ref set-constructors c))

(define (jsexpr->value jse)
  (match jse
    [(or (? boolean?) (? number?) (== (json-null) eq?)) jse]
    [(pregexp #px"^#")
     (case (string-ref jse 0)
       [(#\v) (void)]
       [(#\u) undefined])
       [(#\s) (substring jse 2)]
       [(#\m) (iso8601/tzid->moment (substring jse 2))]
       [(#\b) (base64-decode (string->bytes/utf-8 (substring jse 2)))]]
    [(? string?) (string->symbol jse)]
    [(? cons?) (map jsexpr->value jse)]
    [(? hash?)  (jsexpr->hash jse)]))
        
(define (jsexpr->hash v)
  (case (string->symbol (hash-ref v '|(RKT)| ""))
    [(||)
     (let ([constructor (code->hash (hash-ref v '|(HT)|))])
       (if (memq constructor (list hash hasheq hasheqv))
           (let loop ([acc (constructor)]
                      [lst (hash->list v)])
             (if (empty? lst)
                 acc
                 (loop (if (eq? (caar lst) '|(HT)|)
                           acc
                           (hash-set acc
                                     (caar lst) 
                                     (jsexpr->value (cdar lst))))
                       (cdr lst))))
           (let ([result (constructor)])
             (hash-for-each v (λ (k v) (unless (eq? k '|HT|)
                                         (hash-set! result k (jsexpr->value v)))))
             result)))]
    [(struct)
     (apply make-prefab-struct (jsexpr->value (hash-ref v 'key)) (map jsexpr->value (hash-ref v 'value)))]
    [(lazy-ref)
     (lazy-ref (hash-ref v 'value) #f)]
    [(set)
     (apply (code->set (hash-ref v 'type))
            (map jsexpr->value (hash-ref v 'value)))]
    [(vector)
     (apply vector (map jsexpr->value (hash-ref v 'value)))]
    [(box)
     (box (jsexpr->value (hash-ref v 'value)))]
    [(vbox)
     (make-vbox (jsexpr->value (hash-ref v 'value)))]
    [(char)
     (integer->char (hash-ref v 'value))]
    [(hash)
     (let ([constructor (code->hash (hash-ref v '|(HT)|))])
       (if (memq constructor (list hash hasheq hasheqv))
           ; immutable hashes
           (let loop ([acc (constructor)]
                      [lst (hash-ref v 'value)])
             (if (empty? lst)
                 acc
                 (loop
                  (let ([ht (car lst)])
                    (hash-set acc
                              (jsexpr->value (hash-ref ht 'key))
                              (jsexpr->value (hash-ref ht 'value))))
                  (cdr lst))))
           ; mutable hashes
           (constructor (map (lambda (ht) (cons (jsexpr->value (hash-ref ht 'key))
                                                (jsexpr->value (hash-ref ht 'value))))
                             (hash-ref v 'value)))))]))
