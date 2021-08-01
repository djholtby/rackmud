#lang racket/base

(require racket/class racket/list racket/hash racket/struct racket/bool racket/string racket/match
         racket/local racket/set racket/contract
         (for-syntax racket/base racket/syntax) syntax/modresolve  racket/stxparam racket/splicing
         racket/rerequire racket/undefined
         racket/vector)

(require gregor versioned-box json net/base64)

(require "db.rkt" "lib-path.rkt" "backtrace.rkt" "auth.rkt") 

(provide saved-object% define-saved-class* define-saved-class 
         define-saved-mixin)

;(provide temp-object%)

(provide trigger-reload! save-all-objects lazy-ref lazy-ref? touch! make-lazyref save-object
         get-singleton database-setup new/rackmud rackmud-mark-reloads rebuild-channel
         instantiate/rackmud make-object/rackmud send/rackmud send*/rackmud get-field/rackmud
         set-field!/rackmud is-a?/rackmud is-a?/c/rackmud object?/rackmud object=?/rackmud
         object-or-false=?/rackmud object->vector/rackmud object-interface/rackmud
         object-method-arity-includes?/rackmud field-names/rackmud object-info/rackmud
         dynamic-send/rackmud send/keyword-apply/rackmud send/apply/rackmud dynamic-get-field/rackmud
         dynamic-set-field!/rackmud field-bound?/rackmud
         class-field-accessor/rackmud class-field-mutator/rackmud this/rackmud
         get-authorization make-authorization get-all-auth-tokens expire-auth-token
         expire-all-auth-tokens expire-auth-token-and-jwt find-objects-by-class
         get-rackmud-logs get-rackmud-log-topics get-rackmud-log-counts start-object-reload-thread!
         start-executor! stop-object-reload-thread! stop-executor!)


(define class-dep-sema (make-semaphore 1))
(define cid->paths (make-hasheqv))
(define path->cids (make-hash))
(define rebuild-channel (make-channel))


(define save/k (generate-member-key))
(define load/k (generate-member-key))
(define set-id!/k (generate-member-key))
(define copy-live-state/k (generate-member-key))
(define load-live-state/k (generate-member-key))
(define get-cid/k (generate-member-key))
(define get-all-cids/k (generate-member-key))
(define get-self/k (generate-member-key))

(define-member-name save save/k)
(define-member-name load load/k)
(define-member-name set-id! set-id!/k)
(define-member-name copy-live-state copy-live-state/k)
(define-member-name load-live-state load-live-state/k)
(define-member-name get-cid get-cid/k)
(define-member-name get-all-cids get-all-cids/k)
(define-member-name get-self get-self/k)  






#||||||||||||||||||||||||||||||||||
       LAZY REFERENCE STUFF
|||||||||||||||||||||||||||||||||||#

(define (mud-ref-print lr port mode)
  (case mode
    ; write mode or print mode
    [(#t) (write (lazy-deref lr) port)]
    ; display mode
    [(#f) (display (lazy-deref lr) port)]
    ; print mode
    [else  (print (lazy-deref lr) port mode)]))

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


;; (touch! lr) ensures that lr points to a loaded object (loading from the database if needed)

(define (touch! lr)
  (void (lazy-deref lr)))

;; (lazy-deref lr) produces the object pointed to by lr (from the object table if possible,
;;  the database otherwise).  produces #f if the object cannot be found in the o-table or the
;;  database (i.e. the object has been deleted)
;;  If the object was found, updates its last-access field to the current time
;; lazy-deref: Lazy-Ref -> (U (Instance Saved-Object%) #f)

(define (lazy-deref lr)
  (define result (lazy-deref/no-keepalive lr))
  (when result (set-field! last-access result (now/moment/utc)))
  result)

;; (lazy-deref/no-keepalive lr) is the same as lazy-deref except the object's last-access field is not
;;    updated.  Use this to query an object without making it seem "still in use"

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



#||||||||||||||||||||||||||||||||||
        JSON SERIALIZER
|||||||||||||||||||||||||||||||||||#

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
    [(number? v)
     (string-append "#n" (number->string v))]
    [(list? v) (map value->jsexpr v)]
    [(cons? v)
     `#hasheq((|(RKT)|  . "pair")
              (car . ,(value->jsexpr (car v)))
              (cdr . ,(value->jsexpr (cdr v))))]
    [(box? v)
     `#hasheq((|(RKT)| . "box")
              (value . ,(value->jsexpr (unbox v))))]
    [(vbox? v)
     `#hasheq((|(RKT)| . "vbox")
              (value . ,(value->jsexpr (vbox-ref v))))]
    [(lazy-ref? v)
     `#hasheq((|(RKT)| . "lazy-ref")
              (weak? . ,(lazy-ref:weak? v))
              (value . ,(lazy-ref-id v)))]
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
              (key . ,(value->jsexpr (prefab-struct-key v)))
              (value . ,(map value->jsexpr (rest (vector->list (struct->vector v))))))]
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
                                               (value . ,(value->jsexpr v)))))))]
    [else (error 'value->jsexpr "unknown value: ~v" v)]))







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
    [(pregexp #px"^#.")
     (case (string-ref jse 1)
       [(#\n) (string->number (substring jse 2))]
       [(#\v) (void)]
       [(#\u) undefined]
       [(#\s) (substring jse 2)]
       [(#\m) (iso8601/tzid->moment (substring jse 2))]
       [(#\b) (base64-decode (string->bytes/utf-8 (substring jse 2)))])]
    [(? string?) (string->symbol jse)]
    [(or (? cons?) (? null?)) (map jsexpr->value jse)]
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
     (apply make-prefab-struct (jsexpr->value (hash-ref v 'key)) (map jsexpr->value
                                                                      (hash-ref v 'value)))]
    [(pair)
     (cons
      (jsexpr->value (hash-ref v 'car))
      (jsexpr->value (hash-ref v 'cdr)))]
    [(lazy-ref)
     ((if (hash-ref v 'weak? #f)
         lazy-ref:weak lazy-ref)
      (hash-ref v 'value) #f)]
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



#||||||||||||||||||||||||||||||||||
          OBJECT TABLE
|||||||||||||||||||||||||||||||||||#


(define object-table/semaphore (make-semaphore 1))
(define object-table (make-hasheqv empty))

(define cid-tables/semaphore (make-semaphore 1))
(define cid-to-object-table (make-hasheqv empty))
(define class-to-cid-table (make-hasheq empty))
(define class-to-ancestors-table (make-hasheq empty))




(define object-executor (make-will-executor))
(define executor-thread #f)
(define (start-executor!)
  (set! executor-thread
        (thread (λ()
                  (let loop ()
                    (will-execute object-executor)
                    (loop))))))

(define (stop-executor!)
  (kill-thread executor-thread)
  (set! executor-thread #f))



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
             (set-union st (hash-ref cid-to-object-table cid seteq)))
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

;; get-object-record : SavedObject -> ObjectRecord

(define (get-object-record o)
  (semaphore-wait object-table/semaphore)
  (let ([orec (hash-ref! object-table
                         (send o get-id)
                         (λ () (make-object-record o)))])
    (update-cid-record (send o get-cid) orec)
    (semaphore-post object-table/semaphore)
    (unless (box? (weak-box-value (object-record-obj orec)))
      (set-object-record-obj! orec  (make-weak-box (box o))))
    orec))


#||||||||||||||||||||||||||||||||||
      OBJECT HOT RELOADING
|||||||||||||||||||||||||||||||||||#

(define object-reload-channel (make-channel))
(define object-reload-thread #f)
(define (start-object-reload-thread!)
  (set! object-reload-thread
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
                     (semaphore-post (object-record-reload-semaphore orec))))))
             (loop))))))

(define (stop-object-reload-thread!)
  (kill-thread object-reload-thread)
  (set! object-reload-thread #f))
         
(define (trigger-reload! o)
  (thread-send object-reload-thread (list (get-object-record (maybe-lazy-deref o)))))

(define (rackmud-mark-reloads changed-files)
  (define modified-class-ids
    (foldl append empty (map file->cids changed-files)))
  (log-message (current-logger)
               'info
               'rackmud
               "Reloading modified classes"
               #f #f)
  (define objects-needing-reload
    (cid-list->objects modified-class-ids))
  (log-message (current-logger)
               'debug
               'rackmud
               (format "Modified CIDs: ~v"
                       modified-class-ids))
  (log-message (current-logger)
               'debug
               'rackmud
               (format "Objects that need a reload: ~v"
                       objects-needing-reload))
  
  (thread-send object-reload-thread objects-needing-reload))

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


#||||||||||||||||||||||||||||||||||
            OBJECTS
|||||||||||||||||||||||||||||||||||#

(define saveable<%> (interface () save save-index load on-create on-load on-hot-reload copy-live-state
                      load-live-state updated set-id! get-cid get-all-cids get-self get-id))

(define saved-object% (class* object% (saveable<%> equal<%>)
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
                      
                        ; (save) produces a hash that maps field identifier symbols to values
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
                        (define/public (get-all-cids) '())
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

                      

                        ;; (updated) notes that the object has been updated just now
                        ;;  (call this any time you change the object outside of
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
                                (raise
                                 (make-exn:fail:contract:continuation
                                  "illegal jump into saved-object method after it already returned"
                                  (current-continuation-marks)))
                                (add-thread-to-object orec)))
                 (lambda () (set! started? #t) ((unsyntax-splicing the-thing/pre)
                                                o2
                                                (unsyntax-splicing the-thing/post)))
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



;; (define-saved-class name super-expression mud-defn-or-expr ...) defines a saved-class descended
;;   from super-expression
;; mud-defn-or-expr: as the regular class defn-or-expr, but with nosave varieties for all fields 

(define-syntax (define-saved-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)  body ...)
     ;     (with-syntax ([orig-stx stx])
     (define-saved-class/priv stx 'define-saved-class* #'name #'super-expression
       (syntax->list #'(interface-expr ...)) (syntax->list #'(body ...)))]))

;; (define-saved-class name super-expression body ...) is equivalent to
;;  (define-saved-class* name super-expression () body ...)

(define-syntax (define-saved-class stx)
  (syntax-case stx ()
    [(_ name super-expression body ...)
     (define-saved-class/priv stx 'define-saved-class #'name #'super-expression '()
       (syntax->list #'(body ...)))]))

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

(define-for-syntax (map-filter f lst)
  (let loop ([lst lst])
    (if (null? lst)
        null
        (let ([v (f (car lst))])
          (if v (cons v (loop (cdr lst))) (loop (cdr lst)))))))

;; (wrap-saved-class-exprs def-or-exprs) converts all /nosave and /index variants to plain class
;;   syntax, and returns a list of the new syntaxes
;;   a list of all saved variables, and a list of all indexed variables

(define-for-syntax (wrap-saved-class-exprs who def-or-exprs)
  (let ([saved-class-vars '()]
        [indexed-class-vars (make-hasheq)]
        [unsaved-class-vars '()])
    (define (wrap-def-or-expr doe)
      (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave
                         init-field init-field/nosave index begin)
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
                                   (cons (cons #'id #'id) (cons doe (get-type-info
                                                                     who
                                                                     (syntax->datum #'type-decl)))))]
                       [[id type-decl alt-id]
                        (hash-set! indexed-class-vars
                                   (syntax-e #'id)
                                   (cons (cons #'id #'alt-id) (cons doe
                                                                    (get-type-info
                                                                     who
                                                                     (syntax->datum #'type-decl)))))]
                                   
                       [id
                        (hash-set! indexed-class-vars
                                   (syntax-e #'id)
                                   (cons (cons #'id #'id) (cons doe (cons 'simple 0))))]))))
         #f]
        [(define  id expr)
         (begin (set! saved-class-vars (cons #'id saved-class-vars))
                (syntax/loc doe (define id expr)))]
        [(define/nosave  id expr)
         (begin (set! unsaved-class-vars (cons #'id unsaved-class-vars))
                (syntax/loc doe (define id expr)))]
        [(define-values (id ...) expr)
         (begin (for-each (λ (id) (with-syntax ([id id])
                                    (set! saved-class-vars (cons #'id saved-class-vars))))
                          (syntax->list #'(id ...)))
                (syntax/loc doe  (define-values (id ...) expr)))]
        [(define-values/nosave (id ...) expr)
         (begin (for-each (λ (id) (with-syntax ([id id])
                                    (set! unsaved-class-vars (cons #'id unsaved-class-vars))))
                          (syntax->list #'(id ...)))
                (syntax/loc doe (define-values (id ...) expr)))]
        [(field field-decl ...)
         (begin
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
        [(init-field field-decl ...)
         (begin
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
        [(field/nosave field-decl ...)
         (begin
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
        [(init-field/nosave field-decl ...)
         (begin
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
         (let ([wrapped-subclauses (map-filter wrap-def-or-expr (syntax->list #'(clause ...)))])
           (with-syntax ([(wrapped-clause ...) wrapped-subclauses])
             (syntax/loc doe (begin wrapped-clause ...))))]
        [(_ ...) doe]
        [_ doe]))
    (values (map-filter wrap-def-or-expr def-or-exprs)
            saved-class-vars
            indexed-class-vars
            unsaved-class-vars)))

(define-syntax (define-saved-mixin stx)
  (syntax-case stx ()
    [(_ name (from ...) (to ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                   (wrap-saved-class-exprs 'define-saved-mixin (syntax->list #'(body ...)))])
       (let ([cid-var (car (generate-temporaries '(cid)))]
             [local-field-names (generate-temporaries saved-class-vars)])
         (let  (
                ;; List of syntaxes needed to save the saved-fields
                [save-list (map (λ (id local-name)
                                  (with-syntax ([id id]
                                                [local-name local-name])
                                    #'(hash-set! result local-name id)))
                                saved-class-vars local-field-names)]
                [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                  #'(hash-set! result 'id id)))
                                        unsaved-class-vars)]
                ;; List of syntaxes needed to load the saved-fields
                [load-list (map (λ (id local-name)
                                  (with-syntax ([id id]
                                                [local-name local-name])
                                    #'(set! id (hash-ref vars local-name undefined))))
                                saved-class-vars local-field-names)]
                [load-unsaved-list (map (λ (id)
                                          (with-syntax ([id id])
                                            #'(set! id (hash-ref vars 'id undefined))))
                                        unsaved-class-vars)]
                [lookup-by-id (make-hasheq (map
                                            (λ (id-stx local-identifier)
                                              (cons (syntax->datum id-stx) local-identifier))
                                            saved-class-vars local-field-names))])
           (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                         [(save-id ...) saved-class-vars]
                         [(var-save ...) save-list]
                         [(unsaved-save ...) save-unsaved-list]
                         [(var-load ...) load-list]
                         [(unsaved-load ...) load-unsaved-list]
                         [(local-name ...) local-field-names]
                         [(create-index ...)
                          (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                         
                            (let ([local-id-stx
                                   (hash-ref lookup-by-id id
                                             (λ ()
                                               (raise-syntax-error
                                                'define-saved-mixin
                                                "unknown saved field"
                                                (cadr index-defn)
                                                (caar index-defn))))])
                              (with-syntax ([id local-id-stx]
                                            [type (datum->syntax (caar index-defn)
                                                                 (caddr index-defn))] 
                                            [depth (datum->syntax (caar index-defn)
                                                                  (cdddr index-defn))])
                                (syntax/loc (caar index-defn)
                                  (database-create-field-index id 'type depth)))))]
                         [(define-index-search ...)
                          (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                            (with-syntax ([id (hash-ref lookup-by-id id)] 
                                          [index-name (format-id stx "find-~a-by-~a"
                                                                 (syntax-e #'name)
                                                                 (syntax-e (cdar index-defn))
                                                                 #:source (caar index-defn))]
                                          [value-path (let loop ([depth (cdddr index-defn)] [acc '()])
                                                        (if (= 0 depth)
                                                            acc
                                                            (loop (sub1 depth) (cons "value" acc))))])
                              (quasisyntax/loc (caar index-defn)
                                (splicing-let ([full-json-path (cons (symbol->string id)
                                                                     'value-path)])
                                  (unsyntax
                                   (case (caddr index-defn)
                                     [(simple string number boolean symbol bytes)
                                      (syntax/loc (caar index-defn)
                                        (define (index-name value [operator '=])
                                          (map id->lazy-ref
                                               (field-search-op full-json-path operator
                                                                (value->jsexpr value)))))]
                                     [(symbol-table)
                                      (syntax/loc (caar index-defn)
                                        (define (index-name key [value #f] [mode 'has-key?])
                                          (map id->lazy-ref
                                               (case mode
                                                 [(has-key?)
                                                  (field-search-array full-json-path
                                                                      (symbol->string key))]
                                                 [(has-all-keys?)
                                                  (field-search-array/and full-json-path
                                                                          (map symbol->string key))]
                                                 [(has-any-key?)
                                                  (field-search-array/or full-json-path
                                                                         (map symbol->string key))]
                                                 [(has-pair?)
                                                  (field-search-table/key+value
                                                   full-json-path
                                                   (symbol->string key)
                                                   (value->jsexpr value))]
                                                 [(has-pairs?)
                                                  (field-search-table/key+value/list
                                                   full-json-path
                                                   (map symbol->string key)
                                                   (map value-jsexpr value))]))))]

                                     [(hash)
                                      (syntax/loc (caar index-defn)
                                        (define (index-name key [value #f] [mode 'has-key?])
                                          (map id->lazy-ref
                                               (case mode
                                                 [(has-key?)
                                                  (field-search-array
                                                   full-json-path
                                                   (hasheq 'key (value->jsexpr key)))]
                                                 [(has-all-keys?)
                                                  (field-search-array/and
                                                   full-json-path
                                                   (map (λ (k)
                                                          (hasheq 'key (value->jsexpr k))) key))]
                                                 [(has-any-key?)
                                                  (field-search-array/or
                                                   full-json-path
                                                   (map (λ (k)
                                                          (hasheq 'key (value->jsexpr k))) key))]
                                                 [(has-pair?)
                                                  (field-search-table/key+value
                                                   full-json-path
                                                   (value->jsexpr key)
                                                   (value->jsexpr value)
                                                   #:symbol-table? #f)]
                                                 [(has-pairs?)
                                                  (field-search-table/key+value/list
                                                   full-json-path
                                                   (map value->jsexpr key)
                                                   (map value->jsexpr  value)
                                                   #:symbol-table? #f)]))))]
                                     [(set list vector)
                                      (syntax/loc (caar index-defn)
                                        (define (index-name key-or-keys [mode 'contains?])
                                          (map id->lazy-ref
                                               (case mode
                                                 [(contains?)
                                                  (field-search-array
                                                   full-json-path
                                                   (value->jsexpr key-or-keys))]
                                                 [(contains-all?)
                                                  (field-search-array/and
                                                   full-json-path
                                                   (map value->jsexpr key-or-keys))]
                                                 [(contains-any?)
                                                  (field-search-array/or
                                                   full-json-path
                                                   (map value->jsexpr key-or-keys))]))))]
                                     [else (syntax/loc (caar index-defn)
                                             (define (index-name value [operator '=])
                                               (map id->lazy-ref
                                                    (field-search-op
                                                     full-json-path
                                                     operator
                                                     (hash-ref (value->jsexpr value) 'value)))))]
                                   
                                     ))))))]
                         [cid cid-var])
             (syntax/loc stx
               (splicing-let ([cid (database-get-cid! 'name (relative-module-path (filepath)))])
                 (splicing-let-values ([(local-name ...)
                                        (apply values
                                               (database-get-field-index-ids
                                                cid
                                                '(save-id ...)))])
                   create-index ...
                   define-index-search ...
                
                   (define name
                     (syntax-parameterize ([this/rackmud/param (syntax-id-rules ()
                                                                 [_ (send this get-self)])])
                       (let ([m
                              (mixin (saveable<%> from ...) (to ...)
                                (define/override (get-cid) cid)
                                (define/override (get-all-cids)
                                  (cons cid (super get-all-cids)))
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
                                 def-or-exp ...)])
                       (λ (%)
                         (define new% (m %))
                         (semaphore-wait cid-tables/semaphore)
                         (let ([ancestors (cons name
                                                (cons new%
                                                      (hash-ref class-to-ancestors-table % `(,%))))])
                           #|(for ([ancestor (in-list ancestors)])
                             (hash-update! class-to-descendants-table ancestor
                                           (λ (lst-of-desc)
                                             (cons new% lst-of-desc))
                                           null))|#
                           (hash-set! class-to-ancestors-table new% ancestors))
                         (semaphore-post cid-tables/semaphore)
                         new%))))
                   (semaphore-wait cid-tables/semaphore)
                   (hash-set! class-to-cid-table name cid)
                   (semaphore-post cid-tables/semaphore)
                   )))))))
     ]))

(define-for-syntax (define-saved-class/priv orig-stx who name super-expression interface-expr-list
                     body-list)
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
                      [name/string (string-append
                                    "#<saved-object:"
                                    (symbol->string (syntax-e name)) ">")]
                      [(create-index ...)
                       (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                         
                         (let ([local-id-stx
                                (hash-ref lookup-by-id id
                                          (λ ()
                                            (raise-syntax-error
                                             'define-saved-mixin
                                             "unknown saved field"
                                             (cadr index-defn)
                                             (caar index-defn))))])
                           (with-syntax ([id local-id-stx]
                                         [type (datum->syntax (caar index-defn) (caddr index-defn))] 
                                         [depth (datum->syntax (caar index-defn) (cdddr index-defn))])
                             (syntax/loc (caar index-defn)
                               (database-create-field-index id 'type depth)))))]
                      [(define-index-search ...)
                       (for/list ([(id index-defn) (in-hash indexed-class-vars)])
                         (with-syntax ([id (hash-ref lookup-by-id id)] 
                                       [index-name (format-id orig-stx "find-~a-by-~a"
                                                              (syntax-e name)
                                                              (syntax-e (cdar index-defn))
                                                              #:source (caar index-defn))]
                                       [value-path (let loop ([depth (cdddr index-defn)] [acc '()])
                                                     (if (= 0 depth)
                                                         acc
                                                         (loop (sub1 depth)
                                                               (cons "value" acc))))])
                           (quasisyntax/loc (caar index-defn)
                             (splicing-let ([full-json-path (cons (symbol->string id) 'value-path)])
                               (unsyntax
                                (case (caddr index-defn)
                                  [(simple string number boolean symbol bytes)
                                   (syntax/loc (caar index-defn)
                                     (define (index-name value [operator '=])
                                       (map id->lazy-ref
                                            (field-search-op full-json-path operator
                                                             (value->jsexpr value)))))]
                                  [(symbol-table)
                                   (syntax/loc (caar index-defn)
                                     (define (index-name key [value #f] [mode 'has-key?])
                                       (map id->lazy-ref
                                            (case mode
                                              [(has-key?)
                                               (field-search-array full-json-path
                                                                   (symbol->string key))]
                                              [(has-all-keys?)
                                               (field-search-array/and
                                                full-json-path
                                                (map symbol->string key))]
                                              [(has-any-key?)
                                               (field-search-array/or
                                                full-json-path
                                                (map symbol->string key))]
                                              [(has-pair?)
                                               (field-search-table/key+value
                                                full-json-path
                                                (symbol->string key)
                                                (value->jsexpr value))]
                                              [(has-pairs?)
                                               (field-search-table/key+value/list
                                                full-json-path
                                                (map symbol->string key)
                                                (map value-jsexpr value))]))))]

                                  [(hash)
                                   (syntax/loc (caar index-defn)
                                     (define (index-name key [value #f] [mode 'has-key?])
                                       (map id->lazy-ref
                                            (case mode
                                              [(has-key?)
                                               (field-search-array
                                                full-json-path
                                                (hasheq 'key (value->jsexpr key)))]
                                              [(has-all-keys?)
                                               (field-search-array/and
                                                full-json-path
                                                (map (λ (k)
                                                       (hasheq 'key (value->jsexpr k)))
                                                     key))]
                                              [(has-any-key?)
                                               (field-search-array/or
                                                full-json-path
                                                (map (λ (k)
                                                       (hasheq 'key (value->jsexpr k)))
                                                     key))]
                                              [(has-pair?)
                                               (field-search-table/key+value
                                                full-json-path
                                                (value->jsexpr key)
                                                (value->jsexpr value)
                                                #:symbol-table? #f)]
                                              [(has-pairs?)
                                               (field-search-table/key+value/list
                                                full-json-path
                                                (map value->jsexpr key)
                                                (map value->jsexpr value)
                                                #:symbol-table? #f)]))))]
                                  [(set list vector)
                                   (syntax/loc (caar index-defn)
                                     (define (index-name key-or-keys [mode 'contains?])
                                       (map id->lazy-ref
                                            (case mode
                                              [(contains?)
                                               (field-search-array
                                                full-json-path
                                                (value->jsexpr key-or-keys))]
                                              [(contains-all?)
                                               (field-search-array/and
                                                full-json-path
                                                (map value->jsexpr key-or-keys))]
                                              [(contains-any?)
                                               (field-search-array/or
                                                full-json-path
                                                (map value->jsexpr key-or-keys))]))))]
                                  [else (syntax/loc (caar index-defn)
                                          (define (index-name value [operator '=])
                                            (map id->lazy-ref
                                                 (field-search-op
                                                  full-json-path
                                                  operator
                                                  (hash-ref (value->jsexpr value) 'value)))))]
                                  ))))))]
                      [cid cid-var])
          (syntax/loc orig-stx
            (splicing-let ([cid (database-get-cid! 'name (relative-module-path (filepath)))]
                           [se super-expression])
              (unless
                  (subclass? se saved-object%)
                (raise-argument-error 'define-saved-class "(subclass?/c saved-object%)" se))
              (splicing-let-values ([(local-name ...)
                                     (apply values
                                            (database-get-field-index-ids cid '(save-id ...)))])
                create-index ...
                define-index-search ...
                (provide name)
                (define name
                  (syntax-parameterize ([this/rackmud/param
                                         (syntax-id-rules () [_ (send this get-self)])])

                    (class* se
                      (interface-expr ...)
                      (define/override (get-cid) cid)
                      (define/override (get-all-cids)
                        (cons cid (super get-all-cids)))
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
                      
                      def-or-exp ...)))
                (semaphore-wait cid-tables/semaphore)
                (hash-set! class-to-cid-table name cid)
                (let* ([ancestors (cons name (hash-ref class-to-ancestors-table se `(,se)))]
                       [ancestors/cid (filter (λ (x) x)
                                                (map (λ (%)
                                                       (hash-ref class-to-cid-table % #f))
                                                     ancestors))])
                  
#|                  (for ([ancestor (in-list ancestors)])
                    (hash-update! class-to-descendants-table ancestor
                                  (λ (lst-of-desc)
                                    (cons name lst-of-desc))
                                  null))|#
                  
                  (database-update-class-hierarchy cid ancestors/cid)
                  
                  (hash-set! class-to-ancestors-table name ancestors))
                (semaphore-post cid-tables/semaphore)
                ))))))))


#|||||||||||||||||||||||||||||||||||||||||||||||
                 WEBAUTH STUFF
||||||||||||||||||||||||||||||||||||||||||||||||#

(define (make-authorization obj [duration #f])
  (let ([duration/actual (if (symbol? duration) #f duration)]
        [session? (eq? duration 'session)])
    (values (make-auth-jwt (send/rackmud obj get-id))
            (database-make-token (send/rackmud obj get-id) duration/actual session?))))

(define (get-authorization jwt token)
  (let ([jwt-id (and (string? jwt) (verify-auth-jwt jwt))])
    (if jwt-id
        (values (lazy-ref jwt-id #f) #f #f #f)
        (let-values ([(oid duration new-jwt new-token)
                      (refresh-jwt token)])
          (values (and oid (lazy-ref oid #f)) duration new-jwt new-token)))))
        
(define (get-all-auth-tokens obj)
  (database-get-all-tokens (lazy-ref-id obj)))

(define (prune-auth-tokens)
  (database-prune-tokens))

(define (expire-auth-token obj seq)
  (database-expire-token (if (lazy-ref? obj) (lazy-ref-id obj) obj) seq))

(define (expire-auth-token-and-jwt obj seq jwt)
  (database-expire-token (if (lazy-ref? obj) (lazy-ref-id obj) obj) seq)
  (database-revoke-jwt jwt (auth-jwt-exp jwt) ))

(define (expire-all-auth-tokens obj)
  (database-expire-all-tokens (lazy-ref-id obj)))


#|||||||||||||||||||||||||||||||||||||||||||||||
                 DATABASE STUFF
||||||||||||||||||||||||||||||||||||||||||||||||#


(define (save-all-objects)
  (semaphore-wait object-table/semaphore)
  ;(displayln "Starting save transaction...")
  (database-start-transaction!)
  (with-handlers ([exn? (λ (e)
                          (eprintf "Save failed, error incoming ~a\n" e)
                          (database-commit-transaction!)
                          (raise e))])
    (for ([(oid obj) (in-hash object-table #f)])
      (when oid
        (let ([o (weak-box-value (object-record-obj obj))])
          (when o (save-object (unbox o)))))))
  (semaphore-post object-table/semaphore)

  #|(semaphore-wait cid-tables/semaphore)
  (for ([(% lst-of-desc) (in-hash class-to-descendants-table)])
    (let* ([cid (hash-ref class-to-cid-table % #f)]
           [lst-of-desc/cid (filter (λ (x) x)
                                   (map (λ (%)
                                          (hash-ref class-to-cid-table % #f))
                                   lst-of-desc))]
           [lst-of-ansc/cid (filter (λ (x) x)
                                    (map (λ (%)
                                            (hash-ref class-to-cid-table % #f))
                                         (hash-ref class-to-ancestors-table % (λ () (list %)))))])
      (when cid (database-update-class-hierarchy cid lst-of-desc/cid lst-of-ansc/cid))))
  (semaphore-post cid-tables/semaphore)|#
  (database-commit-transaction!)
  ;(displayln "Save transaction committed")
  )


(define (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-database-connection! (make-rackmud-db-pool db-port db-sock db-srv db-db db-user db-pass)))
   

(define (hot-reload old-object)
  ;(log-message (current-logger) 'debug 'rackmud (format "Hot Reload: ~v" old-object) #f #f)
  (let ([new-object (new (load-class (send old-object get-cid))
                         [id (send old-object get-id)]
                         [name (get-field name old-object)])])
    (send new-object load-live-state (send old-object copy-live-state))
    (send new-object on-hot-reload)
    new-object))
    
;; get-unloaded-object: Nat -> (U (Instanceof MudObject%) #f)
(define (get-unloaded-object id)
  (unless (exact-nonnegative-integer? id)
    (raise-argument-error 'database-load-object "exact-nonnegative-integer?" id))
  ;(unless (database-connected?) (error "database not connected"))
  ;"SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
  (define-values
    [cid created saved fields/json]
    (database-get-object id))
    
  (if cid
      (let ([fields (jsexpr->value fields/json)]
            [new-object (new (load-class cid) [id id])])
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
  (when (database-connected?)
    (define o (if (lazy-ref? oref) (lazy-deref/no-keepalive oref) oref))
    (if o
        (let ([oid (send o get-id)]
              [fields
               (with-transaction #:mode read
                 (value->jsexpr (send o save)))])
          (and (exact-nonnegative-integer? oid)
               (set-field! saved o (database-save-object oid fields)) #t))
        #f)))

(define-syntax (get-singleton stx)
  (syntax-case stx()
    [(_ cls (_id arg) ...)
     #'(let ([c cls])
         (unless (subclass? c saved-object%) (raise-argument-error
                                              'get-singleton
                                              "(subclass?/c saved-object%)" c))
         (let* ([cid (hash-ref class-to-cid-table c #f)]
                [oid (and cid (database-get-singleton cid))])
           (if cid
               (if oid
                   (lazy-ref oid #f)
                   (let ([o (new c [_id arg] ...)])
                     (rackmud-new-object o)
                     (database-make-singleton (send o get-id) cid)
                     (make-lazyref o)))
               (error 'get-singleton "could not find class identifier for class ~v" c))))]))


(define (find-objects-by-class % #:include-ancestor? [include-ancestor? #f])
  (semaphore-wait cid-tables/semaphore)
  (define cid (hash-ref class-to-cid-table % #f))
  (semaphore-post cid-tables/semaphore)
  (define oids
    (if include-ancestor?
        (database-find-objects-by-ancestor-class cid)
        (database-find-objects-by-class cid)))
  (map (λ (oid)
         (lazy-ref oid #f))
       oids))



(define (get-rackmud-logs #:subjects [subjects #f]
                          #:levels [levels #f]
                          #:date-start [date-start #f]
                          #:date-end [date-end #f]
                          #:limit [limit #f] #:offset [offset #f] #:text-search [text-search #f]
                          #:asc? [asc? #f])
  (database-get-logs (and levels (map log-level->int levels))
                     subjects date-start date-end text-search limit offset asc?))

(define (get-rackmud-log-counts #:subjects [subjects #f] #:levels [levels #f]
                                #:date-start [date-start #f] #:date-end [date-end #f]
                                #:text-search [text-search #f])
  (database-get-log-counts
   (and levels (map log-level->int levels)) subjects date-start date-end text-search))

(define (get-rackmud-log-topics)
  (database-get-log-topics))
