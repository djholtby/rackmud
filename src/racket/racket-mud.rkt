#lang racket
(require racket/unsafe/ops)
(require racket/serialize)
(require racket/undefined)
(provide mud-object% define-mud-class* define-mud-class lazy-ref lazy-ref? lazy-ref-id set-lazy-ref-id!)

(serializable-struct lazy-ref (id) #:transparent #:mutable);
(define (oref o)
  (unless (is-a? o mud-object%)
    (raise-argument-error 'oref "mud-object%" o))
  (lazy-ref (get-field id o)))


(define-serializable-class* mud-object% object% (externalizable<%>)
  (super-new)
  (init-field [id (void)])
  (field      [live? #t])
  
  ; (externalize) for a mud object produces a list of symbol - value pairs (the symbols are field identifiers)
  (define/public (externalize)
    (list (cons 'id id)))
  
  ; (internalize/field fv) sets the field identified by (car fv) to the value (cdr fv) if the given identifier is a field of this object,
  ;   otherwise fv is passed to the super method to try (in this case there is no superclass with this method so this is the end)
  (define/public (internalize/field fv)
    (case (unsafe-car fv)
      ['id (set! id (unsafe-cdr fv))]))
  
  ; (internalize lst) takes each field-id value pair and set!s  
  (define/public (internalize lst)
    (for-each (λ (fv)
                (internalize/field fv))
              lst)))

#| not quite done

(define-syntax (call stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ...)
     #'(send (let [(o obj-expr)]
               (if (not (lazy-ref? o)) (raise-argument-error 'call "lazy-ref?" o))
               (let [(ob (lazy-ref-id o))]
                 (if (number? ob) (begin
                                    (set! ob (db-lookup ob))
                                    (set-box! o ob))
                     (void))
                 (if (void? ob) (raise-argument-error 'call "(lazy-ref (anyof mud-object%? number?))" o) (void))
                 (if (not (get-field ob live?))
                     (begin
                       (set-lazy-ref-id! o (void))
                       (raise-argument-error 'call "live mud-object" ob))
                     (void))
                 
                 ob)) method-id arg ...)]))
|#

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
     #'(define-serializable-struct name (field-id ...)
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

(define (safe-save v)
  (if (serializable? v) v undefined))


(define-syntax (define-mud-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) (field-id ...) body ...)
     (let* ([list-of-fields (syntax->list #'(field-id ...))]
            [field-values (map (λ (fid) (with-syntax ([fid fid])
                                          #'(cons 'fid (safe-save fid))))
                               list-of-fields)]
            [field-clauses (map (λ (fid) (with-syntax ([fid fid])
                                           #'['fid (set! fid (unsafe-cdr fv))]))
                                list-of-fields)])
       (with-syntax ([(field-value ...) field-values]
                     [(field-clause ...) field-clauses])
         #'(define-serializable-class* name super-expression (externalizable<%> interface-expr ...)
             body ...
             (define/override (externalize)
               (append (super externalize)
                       (list field-value ...)))
             (define/override (internalize/field fv)
               (case (unsafe-car fv) field-clause ... [else (super internalize/field fv)]))
             (define/override (internalize lst)
               (for-each (λ (fv) (internalize/field fv)) lst)))))]))




(define object-table (make-hasheq empty))

;; (get-object id) produces the object with the ID id, or #<void> if none is currently loaded
;; get-object: Lazy-Ref -> (anyof Mud-Object% Void)

(define (get-object id)
  (hash-ref object-table (lazy-ref-id id) void))

(define (lazy-deref lr)
  (unless (lazy-ref? lr) (raise-argument-error 'lazy-deref "lazy-ref?" lr))
  (define id (lazy-ref-id lr))
  (define o  (get-object id))
  (if (void? o) (database-load id) o))

(define (database-load id)
  (define db-val (database-get-object id))
  (if (void? db-val)
      db-val
      (let ([o ((deserialize (read (open-input-string (bytes->string/utf-8 db-val)))))])
        (hash-set! object-table id o)
        o)))





;(serializable-struct posn (x y) #:transparent)
(define-mud-struct posn (x y))
(define-mud-class foo mud-object% (a b c)
  (super-new)
  (field [a (void)] [b (void)] [c (void)]))
(define x (new foo [id 'x]))
(set-field! a x (make-hash (list (cons 'x (oref x)))))
(set-field! b x (list (list (vector (oref x)) 1) 'hello (posn 1 '((((4)))))))
(set-field! c x (posn (oref x) (oref x)))
(define y (deserialize (serialize x)))
(get-field a y)
(get-field b y)
(get-field c y)



(require web-server/private/gzip)
(deserialize (read (open-input-string (bytes->string/utf-8 (gunzip/bytes (gzip/bytes (string->bytes/utf-8 (format "~s" (serialize x)))))))))

