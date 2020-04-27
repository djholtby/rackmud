#lang racket/base

(require racket/class racket/list racket/bool racket/string racket/match racket/local racket/set racket/contract
         (for-syntax racket/base racket/syntax) syntax/modresolve  racket/stxparam racket/splicing)

(require gregor versioned-box)

(require racket/rerequire)
(require racket/hash)
(require racket/undefined)
(require racket/struct)

(require "db.rkt" "lib-path.rkt" "lazy-ref.rkt" "json-serializer.rkt")

(provide saved-object% define-saved-class* define-saved-class 
         define-saved-mixin define-saved-class/mixin)

(provide temp-object%)

(provide trigger-reload! backtrace save-all-objects lazy-ref lazy-ref? touch! make-lazyref save-object get-singleton
         database-setup new/rackmud rackmud-mark-reloads
         instantiate/rackmud make-object/rackmud send/rackmud send*/rackmud get-field/rackmud set-field!/rackmud is-a?/rackmud is-a?/c/rackmud
         object?/rackmud object=?/rackmud object-or-false=?/rackmud object->vector/rackmud object-interface/rackmud
         object-method-arity-includes?/rackmud field-names/rackmud object-info/rackmud dynamic-send/rackmud
         send/keyword-apply/rackmud send/apply/rackmud dynamic-get-field/rackmud dynamic-set-field!/rackmud field-bound?/rackmud
         class-field-accessor/rackmud class-field-mutator/rackmud this/rackmud)


(define class-dep-sema (make-semaphore 1))
(define cid->paths (make-hasheqv))
(define path->cids (make-hash))



(define save/k (generate-member-key))
(define load/k (generate-member-key))
(define set-id!/k (generate-member-key))
(define copy-live-state/k (generate-member-key))
(define load-live-state/k (generate-member-key))
(define get-cid/k (generate-member-key))
(define get-self/k (generate-member-key))

(define-member-name save save/k)
(define-member-name load load/k)
(define-member-name set-id! set-id!/k)
(define-member-name copy-live-state copy-live-state/k)
(define-member-name load-live-state load-live-state/k)
(define-member-name get-cid get-cid/k)
(define-member-name get-self get-self/k)  









;; (touch! lr) ensures that lr points to a loaded object (loading from the database if needed)

(define (touch! lr)
  (void (lazy-deref lr)))

;; (lazy-deref lr) produces the object pointed to by lr (from the object table if possible, the database otherwise)
;;  produces #f if the object cannot be found in the o-table or the database (i.e. the object has been deleted)
;;  If the object was found, updates its last-access field to the current time
;; lazy-deref: Lazy-Ref -> (U (Instance Mud-Object%) #f)

(define (lazy-deref lr)
  (define result (lazy-deref/no-keepalive lr))
  (when result (set-field! last-access result (now/moment/utc)))
  result)

;; (lazy-deref/no-keepalive lr) is the same as lazy-deref except the object's last-access field is not updated
;;    Use this to query an object without making it seem "still in use"
(define (lazy-deref/no-keepalive lr)
  (unless (lazy-ref? lr) (raise-argument-error 'lazy-deref "lazy-ref?" lr))
  (define id (lazy-ref-id lr))
  (define obj (lazy-ref-obj lr))
  (when (and obj (lazy-ref:weak? lr))
    (set! obj (weak-box-value obj)))
  (if obj (unbox obj)
      (let ([o (or (get-loaded-object id) (get-unloaded-object id))])
        (if o (begin
                (if (lazy-ref:weak? lr)
                    (set-lazy-ref-obj! lr (make-weak-box o))
                    (set-lazy-ref-obj! lr o))
                (unbox o))
            #f))))

(define object-table/semaphore (make-semaphore 1))
(define object-table (make-hasheqv empty))
(define cid-to-object-table (make-hasheqv empty))

;; (get-loaded-object id) produces the object with the ID id, or #f if none is currently loaded
;; get-loaded-object: Nat -> (U (Box (InstanceOf Mud-Object%)) #f)

(define (get-loaded-object id)
  (semaphore-wait object-table/semaphore)
  (define o (object-record-obj (hash-ref object-table id null-record)))
  (begin0
    (if (and o (box? (weak-box-value o)))
        (weak-box-value o)
        (if o (begin
                (hash-remove! object-table id)
                #f)
            #f))
    (semaphore-post object-table/semaphore)))


(define (cid-list->objects lst-of-cid)
  (semaphore-wait object-table/semaphore)
  (begin0
    (foldl (λ (cid st)
             (set-union st (hash-ref cid-to-object-table cid '())))
           (seteq)
           lst-of-cid)
    (semaphore-post object-table/semaphore)))

(define (update-cid-record cid orec)
  (hash-update! cid-to-object-table
                cid
                (λ (st)
                  (set-add st orec))
                (λ () (seteq))))


(define (make-lazyref o [weak? #f])
  (cond [(lazy-ref:weak? o)
         (if weak? o (lazy-ref (lazy-ref-id o) (weak-box-value (lazy-ref-obj o))))]
        [(lazy-ref? o)
         (if weak? (lazy-ref:weak (lazy-ref-id o)
                                  (make-weak-box (lazy-ref-obj o))) o)]
        [(and (box? o) (is-a? (unbox o) saved-object%))
         (if weak?
             (lazy-ref:weak (send (unbox o) get-id)
                            (make-weak-box o))
             (lazy-ref (send (unbox o) get-id) o))]
        [(is-a? o saved-object%)
         (let ([id (send o get-id)]
               [orec (get-object-record o)])
           (if weak?
               (lazy-ref:weak id (object-record-obj orec))
               (lazy-ref id (weak-box-value (object-record-obj orec)))))]
        [else (raise-argument-error 'make-lazyref "(is-a?/c saved-object%)" o)]))
             
(struct object-record ([obj #:mutable] holds semaphore reload-semaphore [wants-reload? #:mutable]))

(define (make-object-record o)
  (object-record (make-weak-box (box o)) (mutable-seteq) (make-semaphore 1) (make-semaphore 1) #f))
(define null-record (object-record (make-weak-box #f) #f #f #f #f))

(define (get-object-record o)
  (semaphore-wait object-table/semaphore)
  (let ([orec (hash-ref! object-table
                         (if (lazy-ref? o) (lazy-ref-id o) (send o get-id))
                         (λ () (make-object-record o)))])
    (update-cid-record (send o get-cid) orec)
    (semaphore-post object-table/semaphore)
    (unless (box? (weak-box-value (object-record-obj orec)))
      (set-object-record-obj! orec  (make-weak-box (box o))))
    orec))

(define object-reload-channel (make-channel))
(define object-reload-thread
  (thread
   (λ ()
     (let loop ()
       (define record-set (thread-receive))
       (for ([orec (in-set record-set)])
         (semaphore-wait (object-record-reload-semaphore orec))
         (semaphore-wait (object-record-semaphore orec))
         (unless (set-empty? (object-record-holds orec))
           (set-object-record-wants-reload?! orec #t)
           (semaphore-post (object-record-semaphore orec))
           (sync object-reload-channel))
         (let ([object-box (weak-box-value (object-record-obj orec))])
           (when object-box
             (let* ([old-object (unbox object-box)]
                    [new-object (hot-reload old-object)])
               (set-object-record-wants-reload?! orec #f)
               (semaphore-post (object-record-semaphore orec))
               (semaphore-post (object-record-reload-semaphore orec)))))
         (loop))))))
         
(define (trigger-reload! o)
  (thread-send object-reload-thread (list (get-object-record o))))

(define (rackmud-mark-reloads changed-files)
  (thread-send object-reload-thread
               (cid-list->objects
                (foldl append empty
                       (map file->cids changed-files)))))

(define (add-thread-to-object orec)
  (semaphore-wait (object-record-semaphore orec))
  (if (set-member? (object-record-holds orec) (current-thread))
      (semaphore-post (object-record-semaphore orec))
      (begin
        (semaphore-post (object-record-semaphore orec))
        (semaphore-wait (object-record-reload-semaphore orec))
        (semaphore-wait (object-record-semaphore orec))
        (set-add! (object-record-holds orec) (current-thread))
        (semaphore-post (object-record-reload-semaphore orec))
        (semaphore-post (object-record-semaphore orec)))))

(define (remove-thread-from-object orec)
  (semaphore-wait (object-record-semaphore orec))
  (set-remove! (object-record-holds orec) (current-thread))
  (if (and (set-empty? (object-record-holds orec))
           (object-record-wants-reload? orec))
      (channel-put object-reload-channel orec)
      (semaphore-post (object-record-semaphore orec))))
 
(define temp-object%
  (class* object% (writable<%>)
    (super-new)
    (init-field [name "temp-object"])
    (field [id #f])
    (define/public (custom-write port)
      (write #f port))
    (define/public (custom-display port)
      (define out (open-output-string))
      (display this% out)
      (display (string-replace (get-output-string out) "class" "object") port))))

(define saveable<%> (interface () save save-index load on-create on-load on-hot-reload copy-live-state load-live-state updated
                      set-id! get-cid get-self get-id))

(define saved-object% (class* object% (writable<%> saveable<%> equal<%>)
                        (super-new)
                        (init [id (void)])
                        (define _id_ id)
                        (define _self_ #f)
                        (init-field  [name "object"])
                        (field      
                         ;; tags : (HashTable Symbol (Setof Symbol))
                         [last-access (now/moment/utc)]
                         [last-update (make-vbox #f)]
                         [saved (now/moment/utc)]
                         [created (now/moment/utc)]
                         [tags  (make-hasheq)])

                      
                        (field [loaded #f]) ; when it was loaded from the database
                      
                        ; (save) for a mud object produces a hash that maps field identifier symbols to values
                        (define/public (save)
                          (make-hasheq))

                        (define/public (save-index)
                          (make-hasheq))


                        (define/public (equal-to? other r?)
                          (cond [(lazy-ref? other) (= _id_ (lazy-ref-id other))]
                                [(is-a? other saved-object%) (= _id_ (send other get-id))]
                                [else #f]))

                        (define/public (equal-hash-code-of hash-code)
                          (hash-code _id_))

                        (define/public (equal-secondary-hash-code-of hash-code)
                          (hash-code _id_))
                        
                        (abstract get-cid)
                        (define/public (get-id) _id_)
                        (define/public (set-id! id) (set! _id_ id))
                        
                        (define/public (on-create) (on-load))
                        (define/public (on-load)
                          (set! loaded (now/moment/utc)))
                        
                        (define/public (on-hot-reload) (void))

                        (define/public (copy-live-state) (make-hasheq '()))
                        
                        (define/public (load-live-state flds) (void))

                        ;(define/public (set-self! self)
                        ;  (unless (lazy-ref? self)
                        ;    (raise-argument-error 'saved-object%:set-self! "lazy-ref?" self))
                        ;  (set! self))

                        (define/public (get-self)
                          (if _self_ _self_
                              (begin
                                (set! _self_ (make-lazyref this))
                                _self_)))
                        
                        ; (load flds) initializes each field with the values in the given hash map
                        (define/public (load flds) (void))

                        (define/public (custom-write port)
                          (write #\# port)
                          (write #\{ port)
                          (write _id_ port)
                          (write #\} port))
                          
                        (define/public (custom-display port)
                          (display  "#<saved-object ...>" port))

                        ;; (updated) notes that the object has been updated just now (call this any time you change the object outside of
                        ;;  set-field! which already does this)
                        (define/public (updated)
                          (vbox-set! last-update (now/moment/utc)))
                        
                        (will-register object-executor this save-object)))

(define-syntax-parameter this/rackmud/param
  (syntax-id-rules () [_ this]))

(define-syntax this/rackmud
  (make-parameter-rename-transformer #'this/rackmud/param))             

(define-for-syntax (do-object-action orig-stx the-thing/pre obj-stx the-thing/post)
  (if (eq? 'this (syntax-e obj-stx))
      (quasisyntax/loc orig-stx
        ((unsyntax-splicing the-thing/pre) this (unsyntax-splicing the-thing/post)))
      (quasisyntax/loc orig-stx
        (let ([o (unsyntax obj-stx)])
          (if (lazy-ref? o)
              (let ([started? #f]
                    [o2 (lazy-deref o)]
                    [orec (hash-ref object-table (lazy-ref-id o))])
                (dynamic-wind
                 (lambda () (if started?
                                (raise (make-exn:fail:contract:continuation "illegal jump into saved-object method after it already returned"
                                                                            (current-continuation-marks)))
                                (add-thread-to-object orec)))
                 (lambda () (set! started? #t) ((unsyntax-splicing the-thing/pre) o2 (unsyntax-splicing the-thing/post)))
                 (lambda () (remove-thread-from-object orec))))
              ((unsyntax-splicing the-thing/pre)  o (unsyntax-splicing the-thing/post)))))))

(define-syntax (send/rackmud stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ...)
     (do-object-action stx #'(send)  #'obj-expr #'(method-id arg ...))]
    [(_ obj-expr method-id arg ... . arglist)
     (do-object-action stx #'(send)  #'obj-expr #'(method-id arg ... . arglist))]))

(define-syntax (send/apply/rackmud stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ... arg-list-expr)
     (do-object-action stx #'(send/apply) #'obj-expr #'(method-id arg ... arg-list-expr))]))


(define-syntax (send/keyword-apply/rackmud stx)
  (syntax-case stx ()
    [(_ obj-expr method-id
        keyword-list-expr value-list-expr
        arg ... arg-list-expr)
     (do-object-action stx
                       #'(send/keyword-apply)
                       #'obj-expr
                       #'(keyword-list-expr value-list-expr arg ... arg-list-expr))]))

(define-syntax (dynamic-send/rackmud stx)
  (syntax-case stx ()
    [(_ obj-expr args ...)
     (do-object-action stx
                       #'(dynamic-send) #'obj-expr
                       #'(args ...))]))

(define-syntax (send*/rackmud stx)
  (syntax-case stx ()
    [(_ obj-expr msg0 msg1 ...)
     (do-object-action stx
                       #'(send*) #'obj-expr
                       #'(msg0 msg1 ...))]))

#|(define-syntax (with-method/rackmud stx)
  (syntax-case stx ()
    [(_ ([id (obj-expr name)] ...) body0 body1 ...)
     (let ([ids (syntax->list (syntax (id ...)))]
           [objs (syntax->list (syntax (obj-expr ...)))]
           [names (syntax->list (syntax (name ...)))])
       (with-syntax ([(binding-clause ...) (map (lambda (id obj-expr name)
                                                  (with-syntax ([id id]
                                                                [obj-expr obj-expr]
                                                                [name name])
                                                    (syntax [id ((maybe-lazy-deref obj-expr) name)])))
                                                ids
                                                objs
                                                names)])
         (syntax/loc stx (with-method (binding-clause ...) body0 body1 ...))))]))                
|#

(define (dynamic-get-field/rackmud field-name obj)
  (dynamic-get-field field-name (maybe-lazy-deref obj)))

(define (dynamic-set-field!/rackmud field-name obj v)
  (dynamic-set-field! field-name (maybe-lazy-deref obj) v))

(define-syntax (field-bound?/rackmud stx)
  (syntax-case stx ()
    [(_ id obj-expr)
     (syntax/loc stx (field-bound? id (maybe-lazy-deref obj-expr)))]))

(define-syntax (class-field-accessor/rackmud stx)
  (syntax-case stx ()
    [(_ class-expr field-id)
     (syntax/loc stx (let ([proc (class-field-accessor class-expr field-id)])
                       (lambda (o)
                         (proc (maybe-lazy-deref o)))))]))

(define-syntax (class-field-mutator/rackmud stx)
  (syntax-case stx ()
    [(_ class-expr field-id)
     (syntax/loc stx (let ([proc (class-field-mutator class-expr field-id)])
                       (lambda (o v)
                         (proc (maybe-lazy-deref o) v))))]))

(define-syntax (get-field/rackmud stx)
  (syntax-case stx ()
    [(_ field-id obj-expr)
     (if (eq? 'this (syntax-e #'obj-expr))
         #'(get-field field-id this)
         #'(get-field field-id (maybe-lazy-deref obj-expr)))]))

(define-syntax (set-field!/rackmud stx)
  (syntax-case stx ()
    [(_ field-id obj-expr value-expr)
     (if (eq? 'this (syntax-e #'obj-expr))
         #'(set-field! field-id this value-expr)
         #'(set-field! field-id 
                       (maybe-lazy-deref obj-expr)
                       value-expr))]))

(define (maybe-lazy-deref v)
  (if (lazy-ref? v) (lazy-deref v) v))

(define (is-a?/rackmud v type)
  (is-a? (maybe-lazy-deref v) type))


(define (object?/rackmud v)
  (or (object? v) (lazy-ref? v)))

(define (object=?/rackmud a b)
  (object=? (maybe-lazy-deref a)
            (maybe-lazy-deref b)))

(define (object-or-false=?/rackmud a b)
  (object-or-false=? (maybe-lazy-deref a)
                     (maybe-lazy-deref b)))

(define (object->vector/rackmud object [opaque-v #f])
  (object->vector (maybe-lazy-deref object) opaque-v))

(define (object-interface/rackmud object)
  (object-interface (maybe-lazy-deref object)))

(define (object-method-arity-includes?/rackmud object sym cnt)
  (object-method-arity-includes? (maybe-lazy-deref object) sym cnt))

(define (field-names/rackmud object)
  (field-names (maybe-lazy-deref object)))

(define (object-info/rackmud object)
  (object-info (maybe-lazy-deref object)))

(struct is-a?-ctc/rackmud (<%>)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
     (define <%> (is-a?-ctc/rackmud-<%> ctc))
     (define name (object-name <%>))
     (cond [name `(is-a?/c ,name)]
           [(class? <%>) '(is-a?/c unknown%)]
           [else '(is-a?/c unknown<%>)]))
   #:first-order
   (λ (ctc)
     (define <%> (is-a?-ctc/rackmud-<%> ctc))
     (λ (v)
       (is-a?/rackmud v <%>)))
   #:stronger
   (λ (this other)
     (define <%> (is-a?-ctc/rackmud-<%> this))
     (if (is-a?-ctc/rackmud other)
         (let ([other<%> (is-a?-ctc/rackmud-<%> other)])
           (cond [(and (class? <%>) (class? other<%>))
                  (subclass? <%> other<%>)]
                 [(and (class? <%>) (interface? other<%>))
                  (implementation? <%> other<%>)]
                 [(and (interface? <%>) (interface? other<%>))
                  (interface-extension? <%> other<%>)]
                 [else #f]))
         #f))
   #:equivalent
   (λ (this other)
     (and (is-a?-ctc/rackmud other)
          (equal? (is-a?-ctc/rackmud-<%> this)
                  (is-a?-ctc/rackmud-<%> other))))))
         
(define (is-a?/c/rackmud type)
  (unless (or (interface? type)
              (class? type))
    (raise-argument-error
     'is-a?/c
     ("or/c interface? class?")
     type))
  (is-a?-ctc/rackmud type))



;; (define-saved-class name super-expression mud-defn-or-expr ...) defines a saved-class descended from super-expression
;; mud-defn-or-expr: as the regular class defn-or-expr, but with nosave varieties for all fields 

(define-syntax (define-saved-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)  body ...)
     ;     (with-syntax ([orig-stx stx])
     (define-saved-class/priv stx 'define-saved-class* #'name #'super-expression (syntax->list #'(interface-expr ...)) (syntax->list #'(body ...)))]))

;; (define-saved-class name super-expression body ...) is equivalent to (define-saved-class* name super-expression () body ...)

(define-syntax (define-saved-class stx)
  (syntax-case stx ()
    [(_ name super-expression body ...)
     (define-saved-class/priv stx 'define-saved-class #'name #'super-expression '() (syntax->list #'(body ...)))]))

(define-syntax (define-saved-class/mixin stx)
  (syntax-case stx ()
    [(_ name super-expression mixin-expr ...)
     (syntax/loc stx
       (begin
         (define name undefined)
         (local [(define cid (database-get-cid! 'name (relative-module-path (filepath))))]
             
           (set! name ((compose1 (λ (%) (class % (super-new) (define/override (get-cid) cid))) mixin-expr ... )
                       super-expression)))
         (provide name)))]))


(define-for-syntax (get-type-info who type-decl)
  (let loop ([type-decl type-decl]
             [depth 0])
    (if (pair? type-decl)
        (loop (cadr type-decl)
              (add1 depth))
        (case type-decl
          [(symbol-table list)
           (cons type-decl depth)]
          [(vector set hash)
           (cons type-decl (add1 depth))]
          [(number string symbol boolean bytes uuid simple)
           (cons type-decl depth)]
          [else (cons type-decl (add1 depth))]))))

;; (wrap-saved-class-exprs def-or-exprs) converts all /nosave and /index variants to plain class syntax, and returns a list of the new syntaxes
;;   a list of all saved variables, and a list of all indexed variables

(define-for-syntax (wrap-saved-class-exprs who def-or-exprs)
  (let ([saved-class-vars '()]
        [indexed-class-vars (make-hasheq)]
        [unsaved-class-vars '()])
    (define (wrap-def-or-expr doe)
      (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave init-field
                         init-field/nosave index begin)
        [(index index-clause ...)
         (let loop ([lst-of-clause (syntax->list #'(index-clause ...))]
                    [index-data (hasheq)])
           (if (null? lst-of-clause)
               index-data
               (loop (cdr lst-of-clause)
                     (syntax-case (car lst-of-clause) ()
                       [[id type-decl]
                        (hash-set! indexed-class-vars
                                   (syntax-e #'id)
                                   (cons #'id (cons doe (get-type-info who (syntax->datum #'type-decl)))))]
                                   
                       [id
                        (hash-set! indexed-class-vars
                                   (syntax-e #'id)
                                   (cons #'id (cons doe (cons 'simple 0))))]))))]
        [(define  id expr) (begin (set! saved-class-vars (cons #'id saved-class-vars)) (syntax/loc doe (define id expr)))]
        [(define/nosave  id expr) (begin (set! unsaved-class-vars (cons #'id unsaved-class-vars)) (syntax/loc doe (define id expr)))]
        [(define-values (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! saved-class-vars (cons #'id saved-class-vars))))
                                                        (syntax->list #'(id ...)))
                                              (syntax/loc doe  (define-values (id ...) expr)))]
        [(define-values/nosave (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id])
                                                                         (set! unsaved-class-vars (cons #'id unsaved-class-vars))))
                                                               (syntax->list #'(id ...)))
                                                     (syntax/loc doe (define-values (id ...) expr)))]
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
                                  (syntax/loc doe (field field-decl ...)))]
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
                                       (syntax/loc doe (init-field field-decl ...)))]
        [(field/nosave field-decl ...) (begin
                                         (for-each (λ (field-decl)
                                                     (syntax-case field-decl ()
                                                       [((internal-id external-id) default-value-expr)
                                                        (begin
                                                          (set! unsaved-class-vars (cons #'internal-id unsaved-class-vars))
                                                          #'((internal-id external-id) default-value-expr))]
                                                       [(id default-value-expr)
                                                        (begin
                                                          (set! unsaved-class-vars (cons #'id unsaved-class-vars))
                                                          #'(id default-value-expr))]
                                                       [(default ...) #'(default ...)]))
                                                   (syntax->list #'(field-decl ...)))
                                         (syntax/loc doe (field field-decl ...)))]
        [(init-field/nosave field-decl ...) (begin
                                              (for-each (λ (field-decl)
                                                          (syntax-case field-decl ()
                                                            [((internal-id external-id) default-value-expr)
                                                             (begin
                                                               (set! unsaved-class-vars (cons #'internal-id unsaved-class-vars))
                                                               #'((internal-id external-id) default-value-expr))]
                                                            [(id default-value-expr)
                                                             (begin
                                                               (set! unsaved-class-vars (cons #'id unsaved-class-vars))
                                                               #'(id default-value-expr))]
                                                            [(default ...) #'(default ...)]))
                                                        (syntax->list #'(field-decl ...)))
                                              (syntax/loc doe (init-field field-decl ...)))]
        [(begin clause ...)
         (let ([wrapped-subclauses (map wrap-def-or-expr (syntax->list #'(clause ...)))])
           (with-syntax ([(wrapped-clause ...) wrapped-subclauses])
             (syntax/loc doe (begin wrapped-clause ...))))]
        [(_ ...) doe]
        [_ doe]))
    (values (map wrap-def-or-expr def-or-exprs)
            saved-class-vars
            indexed-class-vars
            unsaved-class-vars)))

(define-syntax (define-saved-mixin stx)
  (syntax-case stx ()
    [(_ name (from ...) (to ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                   (wrap-saved-class-exprs 'mixin/saved (syntax->list #'(body ...)))])
       (let ([cid-var (car (generate-temporaries '(cid)))]
             [local-field-names (generate-temporaries saved-class-vars)])
         (let  (
                ;; List of syntaxes needed to save the saved-fields
                [save-list (map (λ (id local-name) (with-syntax ([id id]
                                                                 [local-name local-name])
                                                     #'(hash-set! result local-name id)))
                                saved-class-vars local-field-names)]
                [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                  #'(hash-set! result 'id id)))
                                        unsaved-class-vars)]
                ;; List of syntaxes needed to load the saved-fields
                [load-list (map (λ (id local-name) (with-syntax ([id id]
                                                                 [local-name local-name])
                                                     #'(set! id (hash-ref vars local-name undefined))))
                                saved-class-vars)]
                [load-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                  #'(set! id (hash-ref vars 'id undefined))))
                                        unsaved-class-vars)])
           (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                         [(save-id ...) saved-class-vars]
                         [(var-save ...) save-list]
                         [(unsaved-save ...) save-unsaved-list]
                         [(var-load ...) load-list]
                         [(unsaved-load ...) load-unsaved-list]
                         [(local-name ...) local-field-names]
                         [cid cid-var])
             (syntax/loc stx
               (splicing-let ([cid (database-get-cid! 'name (relative-module-path (filepath)))])
                 (splicing-let-values ([(local-name ...) (apply values (database-get-field-index-ids cid '(save-id ...)))])
                   ;create-index ...
                   ;(define-values (search-name ...) (search-defn ...))
                   (define name
                     (syntax-parameterize ([this/rackmud/param (syntax-id-rules () [_ (send this get-self)])])
                       (mixin (saveable<%> from ...) (to ...)
                         (define/override (save)
                           (let ([result (super save)])
                             var-save ...
                             result))
                         (define/override (copy-live-state)
                           (let ([result (super copy-live-state)])
                             var-save ...
                             unsaved-save ...
                             result))
                         (define/override (load vars)
                           var-load ...
                           (super load vars))
                         (define/override (load-live-state vars)
                           var-load ...
                           unsaved-load ...
                           (super load-live-state vars))
                         def-or-exp ...))))))))))
     ]))

(define-for-syntax (define-saved-class/priv orig-stx who name super-expression interface-expr-list body-list)
  (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                (wrap-saved-class-exprs who body-list)])
    (let ([local-field-names (generate-temporaries saved-class-vars)]
          [cid-var (car (generate-temporaries '(cid)))])
      (let  (
             ;; List of syntaxes needed to save the saved-fields
             [lookup-by-id (make-hasheq (map (λ (id-stx local-identifier)
                                               (cons (syntax->datum id-stx) local-identifier))
                                             saved-class-vars local-field-names))]
             [save-list (map (λ (id local-name) (with-syntax ([id id]
                                                              [local-name local-name])
                                                  #'(hash-set! result local-name id)))
                             saved-class-vars local-field-names)]
              
             [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                               #'(hash-set! result 'id id)))
                                     unsaved-class-vars)]
             ;; List of syntaxes needed to load the saved-fields
             [load-list (map (λ (id local-name) (with-syntax ([id id]
                                                              [local-name local-name])
                                                  #'(set! id (hash-ref vars local-name void))))
                             saved-class-vars local-field-names)]
             [load-unsaved-list (map (λ (id) (with-syntax ([id id])
                                               #'(set! id (hash-ref vars 'id void))))
                                     unsaved-class-vars)])
        (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                      [(save-id ...) saved-class-vars]
                      [(var-save ...) save-list]
                      [(unsaved-save ...) save-unsaved-list]
                      [(var-load ...) load-list]
                      [(unsaved-load ...) load-unsaved-list]
                      [(local-name ...) local-field-names]
                      [(interface-expr ...) interface-expr-list]
                      [super-expression super-expression]
                      [name name]
                      [name/string (string-append "#<saved-object:" (symbol->string (syntax-e name)) ">")]
                      [(create-index ...)
                       (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                         
                         (let ([local-id-stx (hash-ref lookup-by-id id
                                                       (λ () (raise-syntax-error who
                                                                                 "unknown saved field" (cadr index-defn) (car index-defn))))])
                           (with-syntax ([id local-id-stx]
                                         [type (datum->syntax (car index-defn) (caddr index-defn))] 
                                         [depth (datum->syntax (car index-defn) (cdddr index-defn))])
                             (syntax/loc (car index-defn) (database-create-field-index id 'type depth)))))]
                      [(define-index-search ...)
                      (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                          (with-syntax ([id (hash-ref lookup-by-id id)] 
                                        [index-name (format-id orig-stx "find-~a-by-~a" (syntax-e name) id #:source (car index-defn))]
                                        [value-path (let loop ([depth (cdddr index-defn)] [acc '()])
                                                      (if (= 0 depth) acc (loop (sub1 depth) (cons "value" acc))))])
                            (quasisyntax/loc (car index-defn)
                              (splicing-let ([full-json-path (cons (symbol->string id) 'value-path)])
                                (unsyntax
                                 (case (caddr index-defn)
                                   [(simple string number boolean symbol bytes)
                                    (syntax/loc (car index-defn)
                                      (define (index-name value [operator '=])
                                        (map id->lazy-ref
                                             (field-search-op full-json-path operator (value->jsexpr value)))))]
                                   [(symbol-table)
                                    (syntax/loc (car index-defn)
                                      (define (index-name key [value #f] [mode 'has-key?])
                                        (map id->lazy-ref
                                             (case mode
                                               [(has-key?) (field-search-array full-json-path (symbol->string key))]
                                               [(has-all-keys?) (field-search-array/and full-json-path (map symbol->string key))]
                                               [(has-any-key?) (field-search-array/or full-json-path (map symbol->string key))]
                                               [(has-pair?) (field-search-table/key+value  full-json-path (symbol->string key) (value->jsexpr value))]
                                               [(has-pairs?) (field-search-table/key+value/list full-json-path
                                                                                                (map symbol->string key)
                                                                                                (map value-jsexpr value))]))))]

                                   [(hash)
                                    (syntax/loc (car index-defn)
                                      (define (index-name key [value #f] [mode 'has-key?])
                                        (map id->lazy-ref
                                             (case mode
                                               [(has-key?) (field-search-array full-json-path (hasheq 'key (value->jsexpr key)))]
                                               [(has-all-keys?) (field-search-array/and full-json-path (map (λ (k) (hasheq 'key (value->jsexpr k))) key))]
                                               [(has-any-key?) (field-search-array/or full-json-path (map (λ (k) (hasheq 'key (value->jsexpr k))) key))]
                                               [(has-pair?) (field-search-table/key+value full-json-path (value->jsexpr key) (value->jsexpr value) #:symbol-table? #f)]
                                               [(has-pairs?) (field-search-table/key+value/list full-json-path
                                                                                                (map jvalue->jsexpr key)
                                                                                                (map value->jsexpr  value) #:symbol-table? #f)]))))]
                                   [(set list vector)
                                    (syntax/loc (car index-defn)
                                      (define (index-name key-or-keys [mode 'contains?])
                                        (map id->lazy-ref
                                             (case mode
                                               [(contains?) (field-search-array full-json-path (value->jsexpr key-or-keys))]
                                               [(contains-all?) (field-search-array/and full-json-path (map value->jsexpr key-or-keys))]
                                               [(contains-any?) (field-search-array/or full-json-path (map value->jsexpr key-or-keys))]))))]
                                   [else (syntax/loc (car index-defn)
                                           (define (index-name value [operator '=])
                                             (map id->lazy-ref
                                                  (field-search-op full-json-path operator (hash-ref (value->jsexpr value) 'value)))))]
                                   
                                   ))))))]
                      
                      [cid cid-var])
          (syntax/loc orig-stx
            (splicing-let ([cid (database-get-cid! 'name (relative-module-path (filepath)))])
              (splicing-let-values ([(local-name ...) (apply values (database-get-field-index-ids cid '(save-id ...)))])
                create-index ...
                ;(define-values (search-name ...) (search-defn ...))
                define-index-search ...
                (provide name)
                (define name
                  (syntax-parameterize ([this/rackmud/param (syntax-id-rules () [_ (send this get-self)])])

                    (class* (let ([se super-expression])
                              (if (subclass? se saved-object%)
                                  se
                                  (raise-argument-error 'define-saved-class "(subclass?/c saved-object%)" se)))
                      (interface-expr ...)
                      (define/override (get-cid) cid)
                      (define/override (save)
                        (let ([result (super save)])
                          var-save ...
                          result))
                      (define/override (copy-live-state)
                        (let ([result (super copy-live-state)])
                          var-save ...
                          unsaved-save ...
                          result))
                      (define/override (load vars)
                        var-load ...
                        (super load vars))
                      (define/override (load-live-state vars)
                        var-load ...
                        unsaved-load ...
                        (super load-live-state vars))
                      (define/override (custom-display port)
                        (display name/string port))
                      def-or-exp ...)))))))))))

                       

#|||||||||||||||||||||||||||||||||||||||||||||||

                DATABASE STUFF

||||||||||||||||||||||||||||||||||||||||||||||||#

;; set! this to #f for DBMS that do not support dates properly (we'll convert it to a string I guess)

(define (save-all-objects)
  (semaphore-wait object-table/semaphore)
  (hash-for-each object-table
                 (λ (oid obj)
                   (let ([o (weak-box-value (object-record-obj obj))])
                     (when o (save-object (unbox o))))))
  (semaphore-post object-table/semaphore))


(define (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-database-connection! (make-rackmud-db-pool db-port db-sock db-srv db-db db-user db-pass)))
   




#|(error-display-handler
 (let ([edh (error-display-handler)])
   (λ (str maybe-exn)
     (when (database-connected?)
       (database-log 'error "racket" str (backtrace maybe-exn)))
     (edh str maybe-exn))))|#

(define (backtrace cms)
  (cond [(continuation-mark-set? cms)
         (let ([out (open-output-string)])
           (for ([context (in-list (continuation-mark-set->context cms))])
             (displayln (format "~a : ~a" (or (car context) "???") (if (srcloc? (cdr context)) (srcloc->string (cdr context))
                                                                       "No source information available")) out))
           (get-output-string out))]
        [(exn? cms)
         (backtrace (exn-continuation-marks cms))]
        [else #f]))




    

(define (hot-reload old-object)
  (let ([new-object (new (load-class (send old-object get-cid))
                         [id (send old-object get-id)]
                         [name (get-field name old-object)])])
    (send new-object load-live-state (send old-object copy-live-state))
    (send new-object on-hot-reload)
    new-object))
    
;; get-unloaded-object: Nat -> (U (Instanceof MudObject%) #f)
(define (get-unloaded-object id)
  ;(unless (database-connected?) (error "database not connected"))
  ;"SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
  (define-values
    [cid created saved fields/json]
    (database-get-object id))
    
  (if cid
      (let ([fields (jsexpr->value fields/json)]
            [new-object (new (load-class cid) [id id])])
        
        ;;; TODO:  This should recompile the changed files, get the timestamps from the changed files, and
        ;;         updated all of their cid's with the timestamp
        ;;; TODO (future): Somewhere in the system is a thread that looks for instances where saved < cid.saved, and does a save ->
        ;;         reload to them
        ;; (when (cons? changes) (eprintf "~v\n" changes))
        
        
        (send new-object load fields)
        
        (set-field! created new-object created)
        (set-field! saved new-object saved)
        (send new-object on-load)
        (define orec (get-object-record new-object))
        (weak-box-value (object-record-obj orec)))
      #f))
  

(define (save-object oref)
  (unless (or (object? oref) (lazy-ref? oref))
    (raise-argument-error 'save-object "(or/c object? lazy-ref?)" oref))
  (define o (if (lazy-ref? oref) (lazy-deref/no-keepalive oref) oref))
  (if o
      (let ([oid (send o get-id)]
            [fields
             (with-transaction #:mode read
               (value->jsexpr (send o save)))])
        
         (and (set-field! saved o (database-save-object oid fields)) #t))
         
      #f))

(define-syntax (get-singleton stx)
  (syntax-case stx()
    [(_ cls (_id arg) ...)
     #'(let ([c cls])
         (unless (subclass? c saved-object%) (raise-argument-error 'get-singleton "(subclass?/c saved-object%)" c))
         (let* ([o (new c (_id arg) ...)]
                [cid (send o get-cid)]
                [oid (database-get-singleton cid)])
           (if oid
               (lazy-ref oid #f)
               (begin
                 (rackmud-new-object o)
                 (database-make-singleton (send o get-id) cid)
                 (make-lazyref o)))))]))

(define-syntax (new/rackmud stx)
  (syntax-case stx ()
    [(_ cls-expr (id arg) ...)
     #'(let* ([cv cls-expr]
              [o (new cv (id arg) ...)])
         (when (subclass? cv saved-object%)
           (rackmud-new-object o))
         (if (subclass? cv saved-object%)  (make-lazyref o) o))]))


(define-syntax (instantiate/rackmud stx)
  (syntax-case stx ()
    [(_ cls-expr (by-pos-expr ...) (id by-name-expr) ...)
     #'(let ([cv cls-expr]
             [o (instantiate cv (by-pos-expr ...) (id by-name-expr) ...)])
         (when (subclass? cv saved-object%)
           (rackmud-new-object o))
         (if (subclass? cv saved-object%)  (make-lazyref o) o))]))


(define-syntax (make-object/rackmud stx)
  (syntax-case stx ()
    [(_ cls-expr by-pos-expr ...)
     #'(let ([cv cls-expr]
             [o (make-object cv by-pos-expr ...)])
         (when (subclass? cv saved-object%)
           (rackmud-new-object o))
         (if (subclass? cv saved-object%)  (make-lazyref o) o))]))

#|(define (find-indexed-objects field value)
  (define value/port (open-output-bytes))
  (write value value/port)
  (define value/bytes (get-output-bytes value/port))
  (close-output-port value/port)
  (map (λ (id) (lazy-ref (vector-ref id 0) #f))
       (database-find-indexed field value/bytes)))
  |#     
    


(define (rackmud-new-object o)
  (unless (is-a? o saved-object%)
    (raise-argument-error 'rackmud-new-object "(is-a?/c saved-object%)" o))
  (let-values ([(oid created) (database-new-object (send o get-cid))])
    (send o set-id! oid)
    (set-field! created o created)
    (send o on-create)
    (send o on-load)
    (save-object o)
    (make-lazyref o)))

#|||||||||||||||||||||||||||||||||||||||||||

  Localization

|||||||||||||||||||||||||||||||||||||||||||#
#|

(define (text-print txt port mode)
  (case mode
    ; write mode or print mode
    [(#t) (write-string (format "#@(~a ~v ~v)" (text-id txt) (text-lang txt) (text-enc txt)) port)]
    ; display mode
    [(#f) (write-string (format "(text ~a ~v ~a ~a)" (text-id txt) (text-default txt) (text-lang txt) (text-enc txt)) port)]
    ; print mode
    [else  (write-string (format "#L~a:~a~v" (text-lang txt) (text-enc txt) (text-default txt)) port)]))

(struct text (id [default #:mutable] lang enc) #:transparent
  #:methods gen:custom-write
  [(define write-proc text-print)])

(define text-cache (make-hash))

(define (make-text txt  [lang 'default-language] [enc 'default-encoding])
  (define lang1 (if (symbol=? lang 'default-language) default-language lang))
  (define enc1 (if (symbol=? enc 'default-encoding) default-encoding enc))
  (hash-ref! text-cache (list txt lang1 enc1)
             (λ ()
               (if (database-connected?)
                   (let ([result (query-maybe-value _dbc_ get-text-id-stmt (string-foldcase (symbol->string lang1)) (symbol->mib enc1) txt)])
                     (text (if result result (make-new-text txt lang1 enc1)) txt lang1 enc1))
                   (text #f txt lang1 enc1)))))
  
(define (make-new-text txt lang enc)
  (define result (query _dbc_ new-text-stmt (string-foldcase (symbol->string lang)) (symbol->mib enc) txt))
  (cond [(and (simple-result? result) (assoc 'insert-id (simple-result-info result)))
         (cdr (assoc 'insert-it (simple-result-info result)))]
        [(and (rows-result? result)
              (= 1 (length (rows-result-headers result)))
              (= 1 (length (rows-result-rows result))))
         (vector-ref (first (rows-result-rows result)) 0)]
        [else #f]))

(define (text->string t lang enc)
  (begin0
    (cond [(and (symbol=? lang (text-lang t))
                (symbol=? enc (text-enc t))
                (text-default t))
           (text-default t)]
          [(and (database-connected?) (text-id t))
           (or (and (symbol->mib enc) (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang))
                    (symbol->mib enc)))
               (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib 'UTF-8))
               (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib 'ASCII))
               (text-default t))]
          [else (text-default t)])
    (when (and (database-connected?) (text-id t) (not (text-default t)))
      (set-text-default! t (text->string t (text-lang t) (text-enc t))))))

(define (add-text-translation t lang enc txt)
  (when (and (database-connected?) (text-id t))
    (query-exec add-text-trans-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib enc) txt)))
|#
  
#||||||||||||||||||||||||||||||||||||||||||||

  Object Management Stuff

||||||||||||||||||||||||||||||||||||||||||||#


(define object-executor (make-will-executor))

(define executor-thread
  (thread (λ()
            (let loop ()
              (will-execute object-executor)
              (loop)))))


