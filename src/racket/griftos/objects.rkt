#lang racket
(require "mudlib-util.rkt")

(provide mud-object% define-mud-class* define-mud-class lazy-ref lazy-ref? lazy-ref-id set-lazy-ref-id! define-mud-struct classlist register-class)

(define-struct lazy-ref (id) #:transparent #:mutable)

(define classlist (make-hasheq))
(define lib-path (let-values ([(here _ __) (split-path (current-contract-region))]) here))

(define (oref o)
  (unless (is-a? o mud-object%)
    (raise-argument-error 'oref "mud-object%" o))
  (lazy-ref (get-field id o)))

(define (register-class classname path)
  (unless (symbol? classname) (raise-argument-error 'register-class "symbol?" classname))
  (hash-set! classlist classname path))

  
(register-class 'mud-object% #f) ;; this is automatically loaded

(define mud-object% (class object% 
                      (super-new)
                      (init-field [id (void)])
                      (field      [live? #t])

                      (define/public (database-key)
                        (list #"object" id))
  
                      ; (save) for a mud object produces a hash that maps field identifier symbols to values
                      (define/public (save)
                        (make-hasheq (list (cons 'id id))))
  
                      ; (load flds) initializes each field with the values in the given hash map
                      (define/public (load flds)
                        (set! id (hash-ref flds 'id void)))))

(define-syntax (call stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ...)
     #'(send (let [(o obj-expr)]
               (unless (lazy-ref? o) (raise-argument-error 'call "lazy-ref?" o))
               (let [(ob (lazy-deref o))]
                 (when (void? ob) (raise-argument-error 'call "lazy-ref to valid object" o))
                 ob)) method-id arg ...)]))

;; (define-mud-class name super-expression (interface-expr ...) (field-id ...) body ...) defines a new mud class named name, descended from super-expression,
;;   implementing the interfaces (interface-expr ...) and with
;;   saved fields (field-id ...) and the consumed body expression and definitions
;; define-mud-class: identifier mud-object% (identifier ...) defn-or-expr ...)
;; requires: all saved fields must be serializable!
;;           all fields listed as saved must actually be fields of the class!  (declaring them as saved does not declare them!)
;; note: private fields *CAN* be saved

(define-syntax (define-mud-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...) (field-id ...) body ...)
     (with-syntax ([orig-stx stx])
       #'(define-mud-class/priv orig-stx
           name
           super-expression
           (interface-expr ...)
           (field-id ...)
           body ...))]))

;; (define-mud-class name super-expression (field-id ...) body ...) is equivalent to (define-mud-class* name super-expression () (field-id ...) body ...)

(define-syntax (define-mud-class stx)
  (syntax-case stx ()
    [(_ name super-expression (field-id ...) body ...)
     (with-syntax ([orig-stx stx])
       #'(define-mud-class/priv orig-stx
           name
           super-expression
           ()
           (field-id ...)
           body ...))]))


(define-syntax (define-mud-struct stx)
  (syntax-case stx ()
    [(_ name (field-id ...) props ...)
     #'(define-struct name (field-id ...)
         #:transparent
         props ...)]))


#|(define-syntax (define-mud-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) (field-id ...) body ...)
     (let* ([list-of-fields (eval #'(list #'field-id ...))]
            [list-of-inface (cons #'externalizable<%> (eval #'(list #'interface-expr ...)))]
            [field-values (map (lambda (s) (list 'cons (list 'quote (syntax->datum s)) (list 'deref-mobject s))) list-of-fields)]
            [externalize-datum
             (list (list 'define/override '(externalize)
                         (append (list 'append '(super externalize))
                                 (list (cons 'list field-values)))))]
            [field-cond-block (map (lambda (s)
                                     ;[(symbol=? (syntax->datum s) (unsafe-car fv)) (set! s (unsafe-cdr fv))]
                                     (list 
                                      (list 'quote (syntax->datum s))
                                      (list 'set! s '(unsafe-cdr fv))))
                                   list-of-fields)]
            [internalize-field-datum
             (list (list 'define/override '(internalize/field fv)
                         (append (cons 'case (cons '(unsafe-car fv) field-cond-block))
                                 '([else (super internalize/field fv)]))))]
            [internalize-datum
             '((define/override (internalize lst)
                 (for-each (lambda (fv) (internalize/field fv)) lst)))]
            [myheader (syntax->datum #'(define-serializable-class* name super-expression (externalizable<%>)))]
            [mybody (syntax->datum #'(body ...))]
            [class-def (append myheader
                               mybody
                               externalize-datum
                               internalize-field-datum
                               internalize-datum)]
            )
       (datum->syntax #'orig-stx class-def))]))
|#



(define-syntax (define-mud-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) (field-id ...) body ...)
     (let* ([list-of-fields (syntax->list #'(field-id ...))]
            [field-save-list (map (λ (fid) (with-syntax ([fid fid])
                                             #'(hash-set! result 'fid fid)))
                                  list-of-fields)]
            [field-load-list (map (λ (fid) (with-syntax ([fid fid])
                                             #'(set! fid (hash-ref flds 'fid void))))
                                  list-of-fields)])
       (with-syntax ([(field-save ...) field-save-list]
                     [(field-load ...) field-load-list])
         #'(begin
             (define name (class* super-expression (interface-expr ...)
                            body ...
                            (define/override (save)
                              (let ([result (super save)])
                                field-save ...
                                result))

                            (define/override (load flds)
                              field-load ...
                              (super load flds))))
             (register-class 'name (find-relative-path lib-path (filepath)))
             (provide name)
             )))]))





;(module+ test
  (define-mud-struct posn (x y))
  (define-mud-class foo mud-object% (a b c)
    (super-new)
    (field [a (void)] [b (void)] [c (void)]))
  (define x (new foo [id 'x]))
  (define y (new foo))

  (set-field! a x (make-hash (list (cons 'x (oref x)))))
  (set-field! b x (list (list (vector (oref x)) 1) 'hello (posn 1 '((((4)))))))
  (set-field! c x 42)
  (send y load (send x save))

  ;(set-field! c x (posn (oref x) (oref x)))
  (get-field id y)
  (get-field a y)
  (get-field b y)
  (get-field c y)
  ;)