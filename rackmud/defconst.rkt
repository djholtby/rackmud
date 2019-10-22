#lang racket/base

(require racket/undefined (for-syntax racket/base))
(provide define-constants define-constant constant-value constant-name get-constant-names) 

(define constant-name-hash (make-hasheq))
(define constant-value-hash (make-hasheq))

(define (get-constant-names category [no-alias? #f])
  (if no-alias?
      (hash-values (hash-ref constant-value-hash category (λ () (hasheq))))
      (hash-keys (hash-ref constant-name-hash category (λ () (hasheq))))))

(define-syntax (define-constant stx)
  (syntax-case stx ()
    [(_ category name value)
     (with-syntax ([token (datum->syntax stx (string->symbol (string-append
                                                              (symbol->string (syntax->datum #'category))
                                                              ":"
                                                              (symbol->string (syntax->datum #'name)))))])
       (syntax/loc stx
         (begin
           (define token value)
           (hash-set! (hash-ref! constant-name-hash 'category (lambda () (make-hasheq)))
                      'name value)
           (hash-set! (hash-ref! constant-value-hash 'category (lambda () (make-hasheqv)))
                      value 'name))))]))

;(define-syntax (define-constants/provide stx)
;  (define-constants* stx #t))

;(define-syntax (define-constants stx)
;  (define-constants* stx #f))

(define-for-syntax (tokenize orig-stx cstr name-stx)
  (datum->syntax orig-stx (string->symbol
                           (string-append cstr ":" (symbol->string (syntax->datum name-stx))))))

;(define-for-syntax (define-constants* stx provide?)
(define-syntax (define-constants stx)
  (syntax-case stx ()
    [(_ category nvp ...)
     (let* ([cstr (symbol->string (syntax->datum #'category))]
            [ctab-name (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum #'category)) "-table")))]
            [body (let loop ([lostx (syntax->list #'(nvp ...))]
                             [provide? #f]
                             [define-alias? #f]
                             [defines '()]
                             [val-name-map '()]
                             [name-val-map '()]
                             [provides '()])
                    (cond [(null? lostx)
                           (list defines
                                 (with-syntax ([(vn ...) val-name-map])
                                   #'(hash-set! constant-value-hash 'category (hasheqv vn ...)))
                                 (with-syntax ([(nv ...) name-val-map])
                                   #'(hash-set! constant-name-hash 'category (hasheq nv ...)))
                                 provides)]
                          [else
                           (syntax-case (car lostx) ()
                             [(name names ... value)
                              (begin
                                (unless (symbol? (syntax-e #'name))
                                  (raise-syntax-error 'define-constants "expected identifier" #'name))
                                (for-each (lambda (n)
                                            (unless (symbol? (syntax-e n))
                                              (raise-syntax-error 'define-constants "expected identifier" n)))
                                          (syntax->list #'(names ...)))
                                (unless (and (number? (syntax-e #'value))
                                             (exact? (syntax-e #'value)))
                                             (raise-syntax-error 'define-constants "expected exact number" #'value))
                                (loop
                                 (cdr lostx)
                                 provide? define-alias?
                                 (if define-alias?
                                     (append (map (lambda (n)
                                                    (with-syntax ([token (tokenize (car lostx) cstr n)])
                                                      (syntax/loc (car lostx)
                                                        (define token value))))
                                                  (cons #'name (syntax->list #'(names ...))))
                                             defines)
                                     (cons (with-syntax ([token (tokenize (car lostx) cstr #'name)])
                                             (syntax/loc (car lostx)
                                               (define token value)))
                                           defines))
                                 (cons (syntax/loc (car lostx) value)
                                       (cons (syntax/loc (car lostx) 'name)
                                             val-name-map))
                                 (append
                                  (foldl append '()
                                         (map (lambda (n)
                                                (with-syntax ([n n])
                                                  (list #''n #'value)))
                                              (cons #'name (syntax->list #'(names ...)))))
                                  name-val-map)
                                 (if provide?
                                     (if define-alias?
                                         (append
                                          (map (lambda (n)
                                                 (with-syntax ([n (tokenize (car lostx) cstr n)])
                                                   #'n))
                                               (cons #'name (syntax->list #'(names ...))))
                                          provides)
                                         (cons (with-syntax ([n (tokenize (car lostx) cstr #'name)])
                                                 #'n)
                                               provides))
                                     provides)))]
                             [maybe-keyword
                              (case (syntax-e #'maybe-keyword)
                                [(#:provide) (loop (cdr lostx) #t define-alias? defines val-name-map name-val-map provides)]
                                [(#:define-alias) (loop (cdr lostx) provide? #t defines val-name-map name-val-map provides)]
                                [else (raise-syntax-error 'define-constants
                                                          (if (keyword? (syntax-e #'maybe-keyword))
                                                              "invalid keyword"
                                                              "expected keyword or constant-definiton clause")
                                                              (car lostx))])]
                             )]))])
     #|
            [body (foldl (lambda (pair acc)
                          (syntax-case pair ()
                            [(names ... value)
                             (with-syntax ([name (car (syntax->list #'(names ...)))])
                               (with-syntax ([token (datum->syntax pair (string->symbol
                                                                         (string-append cstr ":"
                                                                                        (symbol->string (syntax->datum #'name)))))])
                                 (let [(name-symbols (map syntax->datum #'(names ...)))]
                               
                                (cons (syntax/loc pair
                                        (define token value)) (car acc))
                                (cons (syntax/loc pair
                                        (
                                (cons (syntax/loc pair
                                        (hash-set! (hash-ref! constant-name-hash 'category (lambda () (make-hasheq)))
                                                   'name value))
                                      (cadr acc))

                                (cons (syntax/loc pair
                                        (hash-set! (hash-ref! constant-value-hash 'category (lambda () (make-hasheq)))
                                                   value 'name))
                                      (caddr acc))
                                (if provide?
                                    (cons (syntax/loc pair
                                            (provide token)) (cadddr acc))
                                    (cadddr acc)))))]))
       '(() () () ()) (syntax->list #'(nvp ...)))])|#
       
       (with-syntax ([(define-stx ...) (car body)]
                     [name-stx (cadr body)]
                     [value-stx (caddr body)]
                     [(provide-stx ...) (cadddr body)])
         (if (not (null? (cadddr body)))
             (syntax/loc stx (begin (provide provide-stx ...) define-stx ... name-stx value-stx))
             (syntax/loc stx (begin define-stx ... name-stx value-stx)))))]))


(define (constant-value category symbol)
  (hash-ref (hash-ref constant-name-hash category (lambda () (hasheq))) symbol undefined))

(define (constant-name category value)
  (hash-ref (hash-ref constant-value-hash category (lambda () (hasheqv))) value #f))
