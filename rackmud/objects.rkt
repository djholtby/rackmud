#lang racket/base

(require racket/class racket/list racket/bool racket/string racket/path racket/match racket/local racket/set racket/contract racket/logging
         (for-syntax racket/base racket/path) syntax/modresolve racket/splicing racket/stxparam)

(require db db/util/datetime gregor versioned-box)

(require racket/rerequire)
(require racket/fasl)
(require racket/hash)
(require racket/undefined)
(require racket/struct)
(require uuid)
(require "db.rkt")

(provide saved-object% define-saved-class* define-saved-class 
         mixin/saved define-saved-class/mixin void-reader)

(provide temp-object%)

(provide trigger-reload! backtrace save-all-objects saved-object=? lazy-ref lazy-ref? touch! get-object oref save-object get-singleton
         new/rackmud rackmud-mark-reloads
         instantiate/rackmud make-object/rackmud send/rackmud send*/rackmud get-field/rackmud set-field!/rackmud is-a?/rackmud is-a?/c/rackmud
         object?/rackmud object=?/rackmud object-or-false=?/rackmud object->vector/rackmud object-interface/rackmud
         object-method-arity-includes?/rackmud field-names/rackmud object-info/rackmud dynamic-send/rackmud
         send/keyword-apply/rackmud send/apply/rackmud dynamic-get-field/rackmud dynamic-set-field!/rackmud field-bound?/rackmud
         class-field-accessor/rackmud class-field-mutator/rackmud this/rackmud)

         
(provide database-setup database-disconnect database-find-indexed)

(provide cid->paths path->cids)
(provide lib-path  path-begins-with? filepath relative-module-path set-lib-path!)
(provide database-log)

(provide database-make-token database-verify-token database-get-all-tokens database-expire-token database-expire-all-tokens)

(define class-dep-sema (make-semaphore 1))
(define cid->paths (make-hasheqv))
(define path->cids (make-hash))

#|(define (register-class-deps cid import-map)
  (semaphore-wait class-dep-sema)
  (define old-deps (hash-ref cid->paths cid (cons #f (make-hash))))
  (for ([path (in-hash-values (cdr old-deps))])
    (set-remove! (hash-ref path->cids path) cid))
  (hash-set! cid->paths cid (cons (now/moment/utc) import-map))
  (for ([path (in-hash-values import-map)])
    (set-add! (hash-ref! path->cids path mutable-set) cid))
  (semaphore-post class-dep-sema))|#
  

(define (path-begins-with? path dir-path)
  (define-values (base name must-be-dir?) (split-path path))
  (cond [(equal? path dir-path) #t]
        [(not base) #f]
        [else (path-begins-with? base dir-path)]))

(define (module-path mpi parent)
  (define-values (name mpi/2) (module-path-index-split mpi))
  (let ([path (if (or name mpi/2) (resolve-module-path-index mpi  parent) #f)])
    (if (path-string? path) path #f)))

(define (read-unreadable ignore port . args)
  (define char-buffer (open-output-string))
  (let loop ([pound? #f]
             [counter 1])
    (define next-char (read-char port))
    (cond [(and (= counter 1) (char=? #\> next-char))
           (define in-buffer (open-input-string (get-output-string char-buffer)))
           (define word (read in-buffer))
           (begin0
             (case word
               [(void) (void)]
               [(undefined) undefined]
               [(mutable-set:) (apply mutable-set (let loop ([acc '()]) (define v (read in-buffer)) (if (eof-object? v) acc (loop
                                                                                                                             (cons v acc)))))]
               [(set:) (apply set (let loop ([acc '()]) (define v (read in-buffer)) (if (eof-object? v) acc (loop (cons v acc)))))]
               [(moment)
                (define moment-info (read-line in-buffer))
                (iso8601/tzid->moment (string-trim moment-info))]
               [else (error 'read "unreadable value encountered : #<~a>" word)])
             (close-output-port char-buffer)
             (close-input-port in-buffer))]
          [else (write-char next-char char-buffer)
                (loop (char=? next-char #\#) (if (char=? next-char #\>) (sub1 counter) (if (and pound? (char=? next-char #\<)) (add1 counter)
                                                                                           counter)))])))

(define (read-object ignore port . args)
  (let loop ([acc empty])
    (define next-char (read-char port))
    (if (char=? #\} next-char)
        (if (and (cons? acc) (char=? (first acc) #\w))
            (lazy-ref:weak (or (string->number (list->string (reverse acc)))
                               (error 'read "unreadable value encountered: #{~a}" (list->string (reverse acc)))) #f)
            (lazy-ref (or (string->number (list->string (reverse acc)))
                          (error 'read "unreadable value encountered: #{~a}" (list->string (reverse acc)))) #f))
        (loop (cons next-char acc)))))


(define void-reader (make-readtable (current-readtable)
                                    #\v 'dispatch-macro read-vbox
                                    #\< 'dispatch-macro read-unreadable
                                    #\{ 'dispatch-macro read-object))

(define lib-path #f)

(define (set-lib-path! p)
  (unless (or (path? p)
              (path-string? p)) (raise-argument-error 'set-lib-path! "(or/c path? path-string?)" p))
  (set! lib-path (path->directory-path (simplify-path (if (path? p) p (string->path p))))))

(define-syntax-rule (filepath)
  (variable-reference->resolved-module-path (#%variable-reference)))

(define-syntax-rule (filepath/index)
  (variable-reference->module-path-index (#%variable-reference)))

(define (relative-module-path mod-path)
  (unless lib-path
    (error 'define-saved-class "lib-path not set when evaluating ~a" mod-path))
  (let ([mpath (resolved-module-path-name mod-path)])
    (cond [(path? mpath) (find-relative-path lib-path mpath)]
          [(cons? mpath)  (cons (find-relative-path lib-path (car mpath)) (cdr mpath))])))

(define save/k (generate-member-key))
(define save-index/k (generate-member-key))
(define load/k (generate-member-key))
(define set-id!/k (generate-member-key))
(define copy-live-state/k (generate-member-key))
(define load-live-state/k (generate-member-key))
(define get-cid/k (generate-member-key))
(define get-self/k (generate-member-key))

(define-member-name save save/k)
(define-member-name save-index save-index/k)
(define-member-name load load/k)
(define-member-name set-id! set-id!/k)
(define-member-name copy-live-state copy-live-state/k)
(define-member-name load-live-state load-live-state/k)
(define-member-name get-cid get-cid/k)
(define-member-name get-self get-self/k)  








(define (mud-ref-print lr port mode)
  (case mode
    ; write mode or print mode
    [(#t) (write-string (format "#{~v~a}" (lazy-ref-id lr) (if (lazy-ref:weak? lr) "w" "")) port)]
    ; display mode
    [(#f) (write-string (format "(~a ~a ~a)" (if (lazy-ref:weak? lr) "lazy-ref:weak" "lazy-ref") (lazy-ref-id lr) (lazy-ref-obj lr)) port)]
    ; print mode
    [else  (write-string (format "(~a ~v ~a)" (if (lazy-ref:weak? lr) "lazy-ref:weak" "lazy-ref") (lazy-ref-id lr) (lazy-ref-obj lr)) port)]))





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

(define (oref o [weak? #f])
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
        [else (raise-argument-error 'oref "(is-a?/c saved-object%)" o)]))
             
(struct object-record ([obj #:mutable] holds semaphore reload-semaphore [wants-reload? #:mutable]))

(define (make-object-record o)
  (object-record (make-weak-box (box o)) (mutable-seteq) (make-semaphore 1) (make-semaphore 1) #f))
(define null-record (object-record (make-weak-box #f) #f #f #f #f))


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
 




(define (saved-object=? a b)
  (let ([a-id (if (lazy-ref? a) (lazy-ref-id a) (send a get-id))]
        [b-id (cond [(lazy-ref? b) (lazy-ref-id b)]
                    [(and (object? b) (is-a? b saved-object%)) (send b get-id)]
                    [else #f])])
    (and b-id (= a-id b-id))))

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
                                (set! _self_ (oref this))
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
                        
                        (will-register object-executor this database-save-object)))

(define-syntax-parameter this/rackmud/param
  (syntax-id-rules () [_ this]))

(define-syntax this/rackmud
  (make-parameter-rename-transformer #'this/rackmud/param))


(define-syntax define/provide-context-keyword
  (syntax-rules ()
    [(_ (id param-id) ...)
     (begin
       (begin
         (provide id)
         (define-syntax-parameter param-id 
           (make-set!-transformer not-in-a-class))
         (define-syntax id
           (make-parameter-rename-transformer #'param-id)))
       ...)]))
                  

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

;; (define-saved-class name super-expression mud-defn-or-expr ...) defines a saved-class descended from super-expression
;; mud-defn-or-expr: as the regular class defn-or-expr, but with nosave varieties for all fields 

(define-syntax (define-saved-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)  body ...)
     ;     (with-syntax ([orig-stx stx])
     (define-saved-class/priv stx #'name #'super-expression (syntax->list #'(interface-expr ...)) (syntax->list #'(body ...)))]))
;       (syntax/loc stx (define-saved-class/priv orig-stx
;                         name
;                         super-expression
;                         (interface-expr ...)
;                         body ...)))]))

;; (define-saved-class name super-expression body ...) is equivalent to (define-saved-class* name super-expression () body ...)

(define-syntax (define-saved-class stx)
  (syntax-case stx ()
    [(_ name super-expression body ...)
     ;(with-syntax ([orig-stx stx])
     (define-saved-class/priv stx #'name #'super-expression '() (syntax->list #'(body ...)))]))
;(syntax/loc stx (define-saved-class/priv orig-stx
;                  name
;                  super-expression
;                  () 
;                  body ...)))]))



(define-syntax (define-saved-class/mixin stx)
  (syntax-case stx ()
    [(_ name super-expression mixin-expr ...)
     (with-syntax ([name/cid (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum #'name)) "/cid")))])
       (syntax/loc stx
         (begin
           (define name undefined)
           (local [(define name/cid (database-get-cid! 'name (relative-module-path (filepath))))]
             
             (set! name ((compose1 (λ (%) (class % (super-new) (define/override (get-cid) name/cid))) mixin-expr ... )
                         super-expression)))
           (provide name))))]))
               

;; (wrap-saved-class-exprs def-or-exprs) converts all /nosave and /index variants to plain class syntax, and returns a list of the new syntaxes
;;   a list of all saved variables, and a list of all indexed variables

(define-for-syntax (wrap-saved-class-exprs def-or-exprs)
  (let ([saved-class-vars '()]
        [indexed-class-vars '()]
        [unsaved-class-vars '()])
    (define (wrap-def-or-expr doe)
      (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave init-field
                         init-field/nosave define-values/index field/index init-field/index define/index begin)
        [(define  id expr) (begin (set! saved-class-vars (cons #'id saved-class-vars)) #'(define id expr))]
        [(define/nosave  id expr) (begin (set! unsaved-class-vars (cons #'id unsaved-class-vars)) #'(define id expr))]
        [(define/index   id expr) (begin (set! indexed-class-vars (cons #'id indexed-class-vars)) #'(define id expr))]
        [(define-values (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! saved-class-vars (cons #'id saved-class-vars))))
                                                        (syntax->list #'(id ...)))
                                              #'(define-values (id ...) expr))]
        [(define-values/index (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! indexed-class-vars
                                                                                                   (cons #'id indexed-class-vars))))
                                                              (syntax->list #'(id ...)))
                                                    #'(define-values (id ...) expr))]
        [(define-values/nosave (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id])
                                                                         (set! unsaved-class-vars (cons #'id unsaved-class-vars))))
                                                               (syntax->list #'(id ...)))
                                                     #'(define-values (id ...) expr))]
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
        [(field/index field-decl ...) (begin
                                        (for-each (λ (field-decl)
                                                    (syntax-case field-decl ()
                                                      [((internal-id external-id) default-value-expr)
                                                       (begin
                                                         (set! indexed-class-vars (cons #'internal-id indexed-class-vars))
                                                         #'((internal-id external-id) default-value-expr))]
                                                      [(id default-value-expr)
                                                       (begin
                                                         (set! indexed-class-vars (cons #'id indexed-class-vars))
                                                         #'(id default-value-expr))]
                                                      [(default ...) #'(default ...)]))
                                                  (syntax->list #'(field-decl ...)))
                                        #'(field field-decl ...))]
        [(init-field/index field-decl ...) (begin
                                             (for-each (λ (field-decl)
                                                         (syntax-case field-decl ()
                                                           [((internal-id external-id) default-value-expr)
                                                            (begin
                                                              (set! indexed-class-vars (cons #'internal-id indexed-class-vars))
                                                              #'((internal-id external-id) default-value-expr))]
                                                           [(id default-value-expr)
                                                            (begin
                                                              (set! indexed-class-vars (cons #'id indexed-class-vars))
                                                              #'(id default-value-expr))]
                                                           [(default ...) #'(default ...)]))
                                                       (syntax->list #'(field-decl ...)))
                                             #'(init-field field-decl ...))]
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
                                         #'(init-field field-decl ...))]
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
                                              #'(init-field field-decl ...))]
        [(begin clause ...)
         (let ([wrapped-subclauses (map wrap-def-or-expr (syntax->list #'(clause ...)))])
           (with-syntax ([(wrapped-clause ...) wrapped-subclauses])
             #'(begin wrapped-clause ...)))]
        [(default ...) #'(default ...)]))
    (values (map wrap-def-or-expr def-or-exprs)
            saved-class-vars
            indexed-class-vars
            unsaved-class-vars)))



(define-syntax (mixin/saved stx)
  (syntax-case stx ()
    [(_ (from ...) (to ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                   (wrap-saved-class-exprs (syntax->list #'(body ...)))])
       (let  (
              ;; List of syntaxes needed to save the saved-fields
              [save-list (map (λ (id) (with-syntax ([id id])
                                        #'(hash-set! result 'id id)))
                              saved-class-vars)]
              [save-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(hash-set! result 'id id)))
                                    indexed-class-vars)]
              [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                #'(hash-set! result 'id id)))
                                      unsaved-class-vars)]
              ;; List of syntaxes needed to load the saved-fields
              [load-list (map (λ (id) (with-syntax ([id id])
                                        #'(set! id (hash-ref vars 'id void))))
                              saved-class-vars)]
              [load-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(set! id (hash-ref vars 'id void))))
                                    indexed-class-vars)]
              [load-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                #'(set! id (hash-ref vars 'id void))))
                                      unsaved-class-vars)])
         (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                       [(var-save ...) save-list]
                       [(index-save ...) save-index-list]
                       [(unsaved-save ...) save-unsaved-list]
                       [(var-load ...) load-list]
                       [(index-load ...) load-index-list]
                       [(unsaved-load ...) load-unsaved-list])
           (syntax/loc stx
             (splicing-syntax-parameterize ([this/rackmud/param (syntax-id-rules () [_ (send this get-self)])])
               (mixin (saveable<%> from ...) (to ...)
                 (define/override (save)
                   (let ([result (super save)])
                     var-save ...
                     result))
                 (define/override (save-index)
                   (let ([result (super save-index)])
                     index-save ...
                     result))
                 (define/override (copy-live-state)
                   (let ([result (super copy-live-state)])
                     var-save ...
                     index-save ...
                     unsaved-save ...
                     result))
                 (define/override (load vars)
                   var-load ...
                   index-load ...
                   (super load vars))
                 (define/override (load-live-state vars)
                   var-load ...
                   index-load ...
                   unsaved-load ...
                   (super load-live-state vars))
                 def-or-exp ...))))))
     ]))
           

                             


(define-for-syntax (define-saved-class/priv orig-stx name super-expression interface-expr-list body-list)
  (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                (wrap-saved-class-exprs body-list)])
    (let* (;; List of syntaxes needed to save the saved-fields
           [save-list (map (λ (id) (with-syntax ([id id])
                                     #'(hash-set! result 'id id)))
                           saved-class-vars)]
           [save-index-list (map (λ (id) (with-syntax ([id id])
                                           #'(hash-set! result 'id id)))
                                 indexed-class-vars)]
           [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                             #'(hash-set! result 'id id)))
                                   unsaved-class-vars)]
           ;; List of syntaxes needed to load the saved-fields
           [load-list (map (λ (id) (with-syntax ([id id])
                                     #'(set! id (hash-ref vars 'id void))))
                           saved-class-vars)]
           [load-index-list (map (λ (id) (with-syntax ([id id])
                                           #'(set! id (hash-ref vars 'id void))))
                                 indexed-class-vars)]
           [load-unsaved-list (map (λ (id) (with-syntax ([id id])
                                             #'(set! id (hash-ref vars 'id void))))
                                   unsaved-class-vars)])
      (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                    [(var-save ...) save-list]
                    [(index-save ...) save-index-list]
                    [(unsaved-save ...) save-unsaved-list]
                    [(var-load ...) load-list]
                    [(index-load ...) load-index-list]
                    [(unsaved-load ...) load-unsaved-list]
                    [(interface-expr ...) interface-expr-list]
                    [super-expression super-expression]
                    [name name]
                    [name/string (string-append "#<saved-object:" (symbol->string (syntax-e name)) ">")])
        (syntax/loc orig-stx
          (begin
            (define name undefined)
            (splicing-syntax-parameterize ([this/rackmud/param (syntax-id-rules () [_ (send this get-self)])])
              (let ([ name/cid (database-get-cid! 'name (relative-module-path (filepath)))])
                (set! name
                      (class* (let ([se super-expression])
                                (if (subclass? se saved-object%)
                                    se
                                    (raise-argument-error 'define-saved-class "(subclass?/c saved-object%)" se)))

                        (interface-expr ...)
                        (define/override (get-cid) name/cid)
                        (define/override (save)
                          (let ([result (super save)])
                            var-save ...
                            result))
                        (define/override (save-index)
                          (let ([result (super save-index)])
                            index-save ...
                            result))
                        (define/override (copy-live-state)
                          (let ([result (super copy-live-state)])
                            var-save ...
                            index-save ...
                            unsaved-save ...
                            result))
                        (define/override (load vars)
                          index-load ...
                          var-load ...
                          (super load vars))
                        (define/override (load-live-state vars)
                          var-load ...
                          index-load ...
                          unsaved-load ...
                          (super load-live-state vars))
                        (define/override (custom-display port)
                          (display name/string port))
                        ;   (splicing-syntax-parameterize ([this/rackmud (syntax-id-rules () [_ (get-self)])])
                        def-or-exp ...))))
            (provide name)))))))

                       
#|
(define-syntax (define-saved-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars unsaved-class-vars)
                   (wrap-saved-class-exprs (syntax->list #'(body ...)))])
       (let* (;; List of syntaxes needed to save the saved-fields
              [save-list (map (λ (id) (with-syntax ([id id])
                                        #'(hash-set! result 'id id)))
                              saved-class-vars)]
              [save-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(hash-set! result 'id id)))
                                    indexed-class-vars)]
              [save-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                #'(hash-set! result 'id id)))
                                      unsaved-class-vars)]
              ;; List of syntaxes needed to load the saved-fields
              [load-list (map (λ (id) (with-syntax ([id id])
                                        #'(set! id (hash-ref vars 'id void))))
                              saved-class-vars)]
              [load-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(set! id (hash-ref vars 'id void))))
                                    indexed-class-vars)]
              [load-unsaved-list (map (λ (id) (with-syntax ([id id])
                                                #'(set! id (hash-ref vars 'id void))))
                                      unsaved-class-vars)])
         (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                       [(var-save ...) save-list]
                       [(index-save ...) save-index-list]
                       [(unsaved-save ...) save-unsaved-list]
                       [(var-load ...) load-list]
                       [(index-load ...) load-index-list]
                       [(unsaved-load ...) load-unsaved-list]
                       [name/string (string-append "#<saved-object:" (symbol->string (syntax->datum #'name)) ">")])
           (syntax/loc stx
             (begin
               (define name undefined)
               (let ([ name/cid (database-get-cid! 'name (relative-module-path (filepath)))])
                 (set! name
                       (class* (let ([se super-expression])
                                 (if (subclass? se saved-object%)
                                     se
                                     (raise-argument-error 'define-saved-class "(subclass?/c saved-object%)" se)))

                         (interface-expr ...)
                         (inherit get-self)
                         (define/override (get-cid) name/cid)
                         (define/override (save)
                           (let ([result (super save)])
                             var-save ...
                             result))
                         (define/override (save-index)
                           (let ([result (super save-index)])
                             index-save ...
                             result))
                         (define/override (copy-live-state)
                           (let ([result (super copy-live-state)])
                             var-save ...
                             index-save ...
                             unsaved-save ...
                             result))
                         (define/override (load vars)
                           index-load ...
                           var-load ...
                           (super load vars))
                         (define/override (load-live-state vars)
                           var-load ...
                           index-load ...
                           unsaved-load ...
                           (super load-live-state vars))
                         (define/override (custom-display port)
                           (display name/string port))
                         ;(splicing-syntax-parameterize ([this (syntax-id-rules () [_ (get-self)])])
                           def-or-exp ...)));)
               (provide name))))))]))
|#



#|||||||||||||||||||||||||||||||||||||||||||||||

                DATABASE STUFF

||||||||||||||||||||||||||||||||||||||||||||||||#

;; set! this to #f for DBMS that do not support dates properly (we'll convert it to a string I guess)
(define database-date-supported #t)

(define (gregor->system-time-string g)
  (~t (adjust-timezone g (current-timezone)) "YYYY/MM/dd HH:m:s.SSS"))

(define (datetime->moment dt tz)
  (moment (->year dt)
          (->month dt)
          (->day dt)
          (->hours dt)
          (->minutes dt)
          (->seconds dt)
          (->nanoseconds dt)
          #:tz tz))

(define (sql->gregor s)
  (match-define
    (sql-timestamp year month day hour minute second nanosecond tz) s)
  (moment year month day hour minute second nanosecond #:tz (or tz (current-timezone))))

(define (gregor->sql g)
  (sql-timestamp (->year g)
                 (->month g)
                 (->day g)
                 (->hours g)
                 (->minutes g)
                 (->seconds g)
                 (->nanoseconds g)
                 (->utc-offset g)))

;; dbtime->srfi-date: (U SQL-TimeStamp Real) -> Date*
(define (dbtime->moment v)
  (if (sql-timestamp? v)
      (sql->gregor v)
      (datetime->moment (posix->datetime v) UTC)))

(define (moment->dbtime d)
  (if database-date-supported 
      (gregor->sql d)
      (->posix d)))
  
(define object-table/semaphore (make-semaphore 1))
(define object-table (make-hasheqv empty))
(define cid-to-object-table (make-hasheqv empty))

(define (save-all-objects)
  (semaphore-wait object-table/semaphore)
  (hash-for-each object-table
                 (λ (oid obj)
                   (let ([o (weak-box-value (object-record-obj obj))])
                     (when o (save-object (unbox o))))))
  (semaphore-post object-table/semaphore))

;; (get-object id) produces the object with the ID id, or #f if none is currently loaded
;; get-object: Lazy-Ref -> (U (Box (Instance Mud-Object%)) #f)

(define (get-object id)
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
      (let ([o (or (get-object id) (database-get-object id))])
        (if o (begin
                (if (lazy-ref:weak? lr)
                    (set-lazy-ref-obj! lr (make-weak-box o))
                    (set-lazy-ref-obj! lr o))
                (unbox o))
            #f))))

(define _dbc_ #f)

(define (dbsys->config dbsys)
  (case (dbsystem-name dbsys)
    [(postgresql) (values #t #t)]
    [(mysql) (values #f #t)]
    [(sqlite3) (values #f #f)]
    [else (error "Unsupported DB System")]))

(define object-load-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = $1"]
                [else "SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ?"]))))

(define class-load-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT classname, module FROM classes WHERE cid = $1"]
                [else "SELECT classname, module FROM classes WHERE cid = ?"]))))
                              
(define get-classid-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT cid FROM classes WHERE classname = $1 AND module = $2"]
                [else "SELECT cid FROM classes WHERE classname = ? AND module = ?"]))))

(define create-classid-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO classes (classname,module) VALUES ($1 , $2) RETURNING cid"]
                [else "INSERT INTO classes (classname,module) VALUES (? , ?)"]))))
    
(define delete-fields-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "DELETE FROM indexed_fields WHERE oid = $1"]
                [else "DELETE FROM indexed_fields WHERE oid = ?"]))))
(define delete-tags-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "DELETE FROM tags WHERE oid = $1"]
                [else "DELETE FROM tags WHERE oid = ?"]))))
(define new-object-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO objects (cid, created, name, deleted) VALUES ($1, $2, $3, false) RETURNING oid"]
                [(mysql) "INSERT INTO objects (cid, created, name, deleted) VALUES (?, ?, ?, false)"]
                [else "INSERT INTO objects (cid, created, name, deleted) VALUES (?, ?, ?, 0)"]))))



(define save-object-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "UPDATE objects SET name=$1, saved=$2, fields=$3 WHERE oid = $4"]
                [else "UPDATE objects SET name=?, saved=?, fields=? WHERE oid = ?"]))))

(define save-tag-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO tags (oid, category, tag) values (~a, ~a, ~a)"]
                [else "INSERT INTO tags (oid, category, tag) values (?, ?, ?)"]))))

(define save-field-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO indexed_fields (oid, field, value) values ($1, $2, $3)"]
                [else "INSERT INTO indexed_fields (oid, field, value) values (?, ?, ?)"]))))

(define load-tags-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT category, tag FROM tags WHERE oid = $1"]
                [else "SELECT category, tag FROM tags WHERE oid = ?"]))))

(define load-fields-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql)  "SELECT field, value FROM indexed_fields WHERE oid = $1"]
                [else  "SELECT field, value FROM indexed_fields WHERE oid = ?"]))))

(define search-tags-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT oid FROM tags WHERE category = $1 AND tag = $2"]
                [else "SELECT oid FROM tags WHERE category = ? AND tag = ?"]))))

(define search-index-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT oid FROM indexed_fields WHERE field = $1 AND value = $2"]
                [else "SELECT oid FROM indexed_fields WHERE field = ? AND value = ?"]))))
                                             
(define get-singleton-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT oid FROM singletons WHERE cid = $1"]
                [else "SELECT oid FROM singletons WHERE cid = ?"]))))

(define new-singleton-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO singletons (oid, cid) values ($1, $2)"]
                [else "INSERT INTO singletons (oid, cid) values (?, ?)"]))))
                                                
(define log-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO logfile (level, module, description, backtrace) values ($1, $2, $3, $4)"]
                [else "INSERT INTO logfile (level, module, description, backtrace) values (?, ?, ?, ?)"]))))

(define new-token-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "INSERT INTO auth (seq, token, oid, expires) values ($1, $2, $3, $4)"]
                [else "INSERT INTO auth (seq, token, oid, expires) values (?, ?, ?, ?)"]))))

(define verify-auth-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT token, expires FROM auth WHERE seq = $1 and oid = $2"]
                [else "SELECT token, expires FROM auth WHERE seq = ? and oid = ?"]))))

(define tokens-for-oid-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT seq FROM auth where oid = $1"]
                [else "SELECT seq FROM auth where oid = ?"]))))

(define expire-token-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql)  "DELETE FROM auth WHERE seq = $1"]
                [else "DELETE FROM auth WHERE seq = ?"]))))

(define expire-all-tokens-stmt
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql)  "DELETE FROM auth WHERE oid = $1"]
                [else "DELETE FROM auth WHERE oid = ?"]))))

(define module-to-cid-query
  (virtual-statement
   (λ (dbsys) (case (dbsystem-name dbsys)
                [(postgresql) "SELECT cid from classes where module = $1"]
                [else "SELECT cid from classes where module = ?"]))))
  

(define logger-thread #f)


(define (database-connected?)
  (connection? _dbc_))

(define (database-disconnect)
  (when (connection? _dbc_)
    (disconnect _dbc_)
    (set! _dbc_ #f)))

(define (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-database-connection! db-type
                            (make-rackmud-db-pool db-type db-port db-sock db-srv db-db db-user db-pass)))
   

(define (set-database-connection! system pool)
  (unless (connection? pool) (raise-argument-error 'set-database-connection! "connection?" pool))
  (if (symbol=? system 'sqlite3) ;; TODO, does anything else not support timestamp???
      (set! database-date-supported #f)
      (set! database-date-supported #t))
  
  (when _dbc_ (disconnect _dbc_))
  (set! _dbc_ pool)

  (when (thread? logger-thread)
    (kill-thread logger-thread))

  (set! logger-thread
        (thread
         (λ ()
           (let loop ()
             (match (sync rackmud-log-rec)
               [(vector level msg data topic)
                (when (string? msg) (database-log level (or topic "racket") msg (backtrace data)))])
             (loop))))))

(define (log-level->int ll)
  (or (index-of '(none fatal error warning info debug) ll eq?) 0))

(define rackmud-logger (make-logger #f (current-logger) 'info #f))
(define rackmud-log-rec (make-log-receiver (current-logger) 'debug 'rackmud 'debug 'grapevine 'warning))
(current-logger rackmud-logger)

(error-display-handler
 (let ([edh (error-display-handler)])
   (λ (str maybe-exn)
     (when (database-connected?)
       (database-log 'error "racket" str (backtrace maybe-exn)))
     (edh str maybe-exn))))

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

(define (database-log level module code description)
  ;(-> log-level/c (or/c string? false/c) (or/c exact-nonnegative-integer? false/c) string? void?)
  (query-exec _dbc_ log-stmt
              (log-level->int level)
              (if module (if (string? module) module (format "~a" module)) sql-null)
              (or code sql-null)
              (or description sql-null)))
              

(define (database-disconnect!)
  (when (database-connected?)
    (disconnect _dbc_)
    (set! _dbc_ #f)))



(define cid-map (make-hash))

#|! 
(database-get-cid! class-name module-name) retrieves the classid for the named class variable defined in the named module
  if the database does not contain this key, a new row is inserted

database-get-cid! : Symbol Path -> Nat
|#
(define (database-get-cid! class-name module-name)
  (when (not (database-connected?)) (error 'database-get-cid "database not connected"))
  (define cid (hash-ref cid-map (cons class-name module-name) (lambda () #f)))
  (unless cid
    (define class-string (symbol->string class-name))
    (define module-string (path->string module-name))
    (set! cid (query-maybe-value _dbc_ get-classid-stmt class-string module-string))
    (hash-set! cid-map (cons class-name module-name) cid)
    (unless cid
      (query-exec _dbc_ create-classid-stmt class-string module-string)
      (set! cid (query-maybe-value _dbc_ get-classid-stmt class-string module-string))
      (hash-set! cid-map (cons class-name module-name) cid)))
  (unless cid (error 'database-get-cid! "could not find or create new classid"))
  cid)


(define (file->cids module-name)
  (when (not (database-connected?)) (error 'file->cids "database not connected"))
  (query-list _dbc_ module-to-cid-query (path->string (relative-module-path (make-resolved-module-path module-name)))))


(define (load-class cid)
  (let* ([classinfo/v (query-row _dbc_ class-load-stmt cid)]
         [classname (string->symbol (vector-ref classinfo/v 0))]
         [classfile (string->path (vector-ref classinfo/v 1))]
         [classfile/resolved (build-path lib-path classfile)]
         [changes (dynamic-rerequire classfile/resolved #:verbosity 'none)] ;; TODO: Mark changed mudlib source files
         [% (dynamic-require classfile/resolved classname)])
    ;(mark-needed-reloads changes)
    %))
    

(define (hot-reload old-object)
  (let ([new-object (new (load-class (send old-object get-cid))
                         [id (send old-object get-id)]
                         [name (get-field name old-object)])])
    (send new-object load-live-state (send old-object copy-live-state))
    (send new-object on-hot-reload)
    new-object))
    
;; database-get-object: Nat -> (U (Instanceof MudObject%) #f)
(define (database-get-object id)
  (unless (database-connected?) (error "database not connected"))
  ;"SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
  (start-transaction _dbc_ #:isolation 'read-committed)
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  (define tags (and obj (query-rows _dbc_ load-tags-stmt id)))
  (define indexed-fields (and obj (make-hasheq (map (λ (v)
                                                      (cons (string->symbol (vector-ref v 0))
                                                            (parameterize ([current-readtable void-reader])
                                                              (read (open-input-bytes (vector-ref v 1))))))
                                                    (query-rows _dbc_ load-fields-stmt id)))))
  (commit-transaction _dbc_)
    
  (if (and obj (or (false? (vector-ref obj 4))
                   (zero? (vector-ref obj 4))))
      (let ([fields (parameterize ([current-readtable void-reader]) (read (open-input-bytes (vector-ref obj 5))))]
            [new-object (new (load-class (vector-ref obj 0)) [id id] [name (vector-ref obj 3)])]
            [created (dbtime->moment (vector-ref obj 1))]
            [saved (dbtime->moment (vector-ref obj 2))])
        (let ([tag-hash (get-field tags new-object)])

          ;;; TODO:  This should recompile the changed files, get the timestamps from the changed files, and
          ;;         updated all of their cid's with the timestamp
          ;;; TODO (future): Somewhere in the system is a thread that looks for instances where saved < cid.saved, and does a save ->
          ;;         reload to them
          ;; (when (cons? changes) (eprintf "~v\n" changes))
 
          (hash-union! indexed-fields fields)
          (send new-object load indexed-fields)
          (set-field! created new-object created)
          (set-field! saved new-object saved)
          (for ([row (in-list tags)])
            (local [(define category (vector-ref row 0))
                    (define tag      (string->symbol (vector-ref row 1)))]
              (if (hash-has-key? tag-hash category)
                  (set-add! (hash-ref tag-hash category) tag)
                  (hash-set! tag-hash category (mutable-seteq tag)))))
                
          (send new-object on-load)
          (define orec (get-object-record new-object))
          (weak-box-value (object-record-obj orec))))
      #f))
  

(define (save-object oref)
  (unless (or (object? oref) (lazy-ref? oref))
    (raise-argument-error 'save-object "(or/c object? lazy-ref?)" oref))
  (define o (if (lazy-ref? oref) (lazy-deref/no-keepalive oref) oref))
  (if o (database-save-object o) #f))

;; database-save-object: (InstanceOf MudObject%) -> Bool

(define (database-save-object o)
  (unless (database-connected?) (error "database not connected"))
  (unless (is-a? o saved-object%)
    (raise-argument-error 'database-save-object "(is-a?/c saved-object%)" o))
  (let-values ([(oid) (send o get-id)]
               [(name) (get-field name o)]
               [(indexed-fields saved old-saved fields)
                (with-transaction #:mode read
                  (values (send o save-index)
                          (now/moment/utc)
                          (get-field saved o)
                          (string->bytes/utf-8 (format "~s" (send o save)))))])
    (set-field! saved o saved)
    (start-transaction _dbc_)
    ;; TODO: log these issues
    (with-handlers ([(λ (e) #t) (λ (e)
                                  (set-field! saved o old-saved)
                                  (rollback-transaction _dbc_)
                                  #f)])
      ; Remove any existing indexed fields and tags
      (query-exec _dbc_ delete-fields-stmt oid)
      (query-exec _dbc_ delete-tags-stmt oid)
      ; Save / Update the object itself
      (query-exec _dbc_ save-object-stmt name (moment->dbtime saved) fields oid)
      ; Save the indexed fields
      (for ([(field value) (in-hash indexed-fields)])
        (define value/port (open-output-bytes))
        (write value value/port)
        (query-exec _dbc_ save-field-stmt oid (symbol->string field) (get-output-bytes value/port))
        (close-output-port value/port))
      ; Save the tags
      (for* ([(category tags)
              (in-hash (get-field tags o))]
             [tag (in-set tags)])
        (query-exec _dbc_ save-tag-stmt oid category tag))
      ; That's all, folks
      (with-handlers ([exn:fail:sql?
                       (λ (e)
                         (match (exn:fail:sql-sqlstate e)
                           [#rx"^40...$" (database-save-object o)]
                           ['done (database-save-object o)]
                           [else (raise e)]))])
        (commit-transaction _dbc_)
        #t))))

(define-syntax (get-singleton stx)
  (syntax-case stx()
    [(_ cls (_id arg) ...)
     #'(let ([c cls])
         (unless (subclass? c saved-object%) (raise-argument-error 'get-singleton "(subclass?/c saved-object%)" c))
         (let* ([o (new c (_id arg) ...)]
                [cid (send o get-cid)]
                [oid (query-maybe-value _dbc_ get-singleton-stmt cid)])
           (if oid
               (lazy-ref oid #f)
               (begin
                 (database-new-object o)
                 (query-exec _dbc_ new-singleton-stmt (send o get-id) cid)
                 (oref o)))))]))

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
           (database-new-object o))
         (if (subclass? cv saved-object%)  (oref o) o))]))


(define-syntax (instantiate/rackmud stx)
  (syntax-case stx ()
    [(_ cls-expr (by-pos-expr ...) (id by-name-expr) ...)
     #'(let ([cv cls-expr]
             [o (instantiate cv (by-pos-expr ...) (id by-name-expr) ...)])
         (when (subclass? cv saved-object%)
           (database-new-object o))
         (if (subclass? cv saved-object%)  (oref o) o))]))


(define-syntax (make-object/rackmud stx)
  (syntax-case stx ()
    [(_ cls-expr by-pos-expr ...)
     #'(let ([cv cls-expr]
             [o (make-object cv by-pos-expr ...)])
         (when (subclass? cv saved-object%)
           (database-new-object o))
         (if (subclass? cv saved-object%)  (oref o) o))]))

(define (database-find-indexed field value)
  (define value/port (open-output-bytes))
  (write value value/port)
  (define value/bytes (get-output-bytes value/port))
  (close-output-port value/port)
  (map (λ (id) (lazy-ref (vector-ref id 0) #f))
       (query-rows _dbc_ search-index-stmt field value/bytes)))

(define (database-make-token o #:expires [expires "9999-01-01T00:00:00Z"])
  (define oid (lazy-ref-id o))
  (define seq (uuid-string))
  (define token (uuid-string))
  (query-exec _dbc_ new-token-stmt seq (sha256-bytes (string->bytes/utf-8 token)) oid expires)
  (string-append oid ":" seq ":" token))

(define (database-verify-token text)
  (match-define
    (list oid seq token)
    (string-split text ":"))
  (define result (query-rows _dbc_ verify-auth-stmt seq oid))
  (and (cons? result)
       (moment<? (now/moment/utc)
                 (iso8601->moment (vector-ref result 1)))
       (bytes=? (string->bytes/utf-8 (vector-ref result 0))
                (sha256-bytes (string->bytes/utf-8 token)))
       (lazy-ref oid #f)))

(define (database-get-all-tokens o)
  (define oid (lazy-ref-id o))
  (query-list _dbc_ tokens-for-oid-stmt oid))

(define (database-expire-token seq)
  (query-exec _dbc_ expire-token-stmt seq))

(define (database-expire-all-tokens o)
  (define oid (lazy-ref-id o))
  (query-exec _dbc_ expire-all-tokens-stmt oid))
    


(define (database-new-object o)
  (unless (is-a? o saved-object%)
    (raise-argument-error 'database-new-object "(is-a?/c saved-object%)" o))
  (define cid (send o get-cid))
  (define name (get-field name o))
  (define created (get-field created o))
  (send o on-create)
  ;"INSERT INTO objects (class, created, name, deleted) VALUES (~v, ~v, ~v, false)
  (define new-obj-result (query _dbc_ new-object-stmt cid (moment->dbtime created) name))
  (cond [(and (simple-result? new-obj-result) (assoc 'insert-id (simple-result-info new-obj-result)))
         (send o set-id! (cdr (assoc 'insert-id (simple-result-info new-obj-result))))]
        [(and (rows-result? new-obj-result)
              (= 1 (length (rows-result-headers new-obj-result)))
              (= 1 (length (rows-result-rows new-obj-result))))
         (send o set-id! (vector-ref (first (rows-result-rows new-obj-result)) 0))]
        [else (error 'new "unexpected database result (~v) when instantiating new object" new-obj-result)])
  
  (send o on-load)
  (database-save-object o)
  (oref o))
;(define orec (get-object-reord o))
;(lazy-ref (send o get-id) orec))

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


