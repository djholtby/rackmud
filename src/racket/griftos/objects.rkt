#lang racket
(require "mudlib-util.rkt")

(provide mud-object% define-mud-class* define-mud-class lazy-ref lazy-ref? lazy-ref-id set-lazy-ref-id! define-mud-struct classlist register-class)

(define-struct lazy-ref (id) #:transparent #:mutable)

(define classlist (make-hasheq))

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
                        (make-hasheq `((id . ,id))))
  
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


;; (define-mud-class name super-expression mud-defn-or-expr ...) defines a mud-class descended from super-expression
;; mud-defn-or-expr: as the regular class defn-or-expr, but with nosave varieties for all fields (e.g. "define" has the "define/nosave" variant)

(define-syntax (define-mud-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)  body ...)
     (with-syntax ([orig-stx stx])
       #'(define-mud-class/priv orig-stx
           name
           super-expression
           (interface-expr ...)
           body ...))]))

;; (define-mud-class name super-expression body ...) is equivalent to (define-mud-class* name super-expression () body ...)

(define-syntax (define-mud-class stx)
  (syntax-case stx ()
    [(_ name super-expression body ...)
     (with-syntax ([orig-stx stx])
       #'(define-mud-class/priv orig-stx
           name
           super-expression
           ()
           body ...))]))

(define-syntax (define-mud-struct stx)
  (syntax-case stx ()
    [(_ name (field-id ...) props ...)
     #'(define-struct name (field-id ...)
         #:transparent
         props ...)]))

(define-syntax (define-mud-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) body ...)
     (letrec ([saved-class-vars '()]
              [def-or-exprs (syntax->list #'(body ...))]
              ;; (wrap-def-or-expr doe) logs the (internal) id(s) if doe is a field definition (i.e. define, define-values, field, init-field) and leaves doe alone
              ;;                        converts doe to a regular field definition if it is a nosave variant (i.e. define/nosave, define-values/nosave etc.) (does not log id in this case)
              ;;                        recurses into subclauses if doe is a (begin ...) expression
              ;;                        and otherwise leaves doe alone (i.e. if it is an expression or method definition)
              [wrap-def-or-expr (λ (doe)
                                  (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave init-field  init-field/nosave begin)
                                    [(define  id expr) (begin (set! saved-class-vars (cons #'id saved-class-vars)) #'(define id expr))]
                                    [(define/nosave  id expr) #'(define id expr)]
                                    [(define-values (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! saved-class-vars (cons #'id saved-class-vars))))
                                                                                    (syntax->list #'(id ...)))
                                                                          #'(define-values (id ...) expr))]
                                    [(define-values/nosave (id ...) expr) #'(define-values (id ...) expr)]
                                    [(field field-decl ...) (begin
                                                              (for-each (λ (field-decl)
                                                                          (syntax-case field-decl ()
                                                                            [((internal-id external-id) default-value-expr)
                                                                             (begin
                                                                               (set! saved-class-vars (cons #'internal-id saved-class-vars))
                                                                               #'((internal-id external-id) default-value-expr))]
                                                                            [(id default-value-expr)
                                                                             (begin
                                                                               (set! saved-class-vars (cons #'id saved-class-vars))
                                                                               #'(id default-value-expr))]
                                                                            [(default ...) #'(default ...)]))
                                                                        (syntax->list #'(field-decl ...)))
                                                              #'(field field-decl ...))]
                                    [(init-field field-decl ...) (begin
                                                                   (for-each (λ (field-decl)
                                                                               (syntax-case field-decl ()
                                                                                 [((internal-id external-id) default-value-expr)
                                                                                  (begin
                                                                                    (set! saved-class-vars (cons #'internal-id saved-class-vars))
                                                                                    #'((internal-id external-id) default-value-expr))]
                                                                                 [(id default-value-expr)
                                                                                  (begin
                                                                                    (set! saved-class-vars (cons #'id saved-class-vars))
                                                                                    #'(id default-value-expr))]
                                                                                 [(default ...) #'(default ...)]))
                                                                             (syntax->list #'(field-decl ...)))
                                                                   #'(init-field field-decl ...))]
                                         
                                    [(field/nosave field-decl ...) #'(field field-decl ...)]
                                    [(init-field/nosave init-decl ...) #'(init-field init-decl ...)]
                                    [(begin clause ...)
                                     (let ([wrapped-subclauses (map wrap-def-or-expr (syntax->list #'(clause ...)))])
                                       (with-syntax ([(wrapped-clause ...) wrapped-subclauses])
                                         #'(begin wrapped-clause ...)))]
                                    [(default ...) #'(default ...)]))] ; it's none of the above, leave it alone
              ;; wrapped-def-or-exprs is the remapped set of def-or-expr in the class
              [wrapped-def-or-exprs (map wrap-def-or-expr
                                         def-or-exprs)]
              ;; List of syntaxes needed to save the saved-fields
              [save-list (map (λ (id) (with-syntax ([id id])
                                        #'(hash-set! result 'id id)))
                              saved-class-vars)]
              ;; List of syntaxes needed to load the saved-fields
              [load-list (map (λ (id) (with-syntax ([id id])
                                        #'(set! id (hash-ref vars 'id void))))
                              saved-class-vars)])
       (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                     [(var-save ...) save-list]
                     [(var-load ...) load-list])
         #'(begin
             (define name (class* super-expression (interface-expr ...)
                            (define/override (save)
                              (let ([result (super save)])
                                var-save ...
                                result))
                            (define/override (load vars)
                              var-load ...
                              (super load vars))
                            def-or-exp ...))
             (register-class 'name (relative-module-path (filepath)))
             (provide name)
             )))]))





(module+ test
  (define-mud-struct posn (x y))

  (define-mud-class foo% mud-object% 
    (super-new)
    (field [a (void)] [b (void)] [c (void)]))
  (define x (new foo% [id 'x]))
  (define y (new foo%))

  (set-field! a x (make-hash (list (cons 'x (oref x)))))
  (set-field! b x (list (list (vector (oref x)) 1) 'hello (posn 1 '((((4)))))))
  (set-field! c x 42)
  (send y load (send x save))

  ;(set-field! c x (posn (oref x) (oref x)))
  (get-field id y)
  (get-field a y)
  (get-field b y)
  (get-field c y)
  classlist
  )