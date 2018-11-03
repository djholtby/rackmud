#lang racket/base

(require racket/class racket/list racket/bool racket/string racket/path racket/match racket/local racket/set racket/contract racket/logging
         (for-syntax racket/base racket/path) syntax/modresolve)
(require db db/util/datetime gregor)
(require racket/rerequire)
(require racket/fasl)
(require racket/hash)
(require racket/undefined)
(require racket/struct)
(require "charset.rkt")

(provide saved-object% define-saved-class* define-saved-class 
         mixin/saved define-saved-class/mixin)

(provide temp-object%)

(provide object-table object-table/semaphore saved-object=? lazy-ref lazy-ref?  get-object oref save-object get-singleton new/griftos
         instantiate/griftos make-object/griftos send/griftos get-field/griftos set-field!/griftos is-a?/griftos is-a?/c/griftos)

(provide database-setup database-disconnect database-find-indexed)

(provide cid->paths path->cids)
(provide lib-path  path-begins-with? filepath relative-module-path set-lib-path!)
(provide log-level? database-log)

(provide text text? make-text text->string add-text-translation)



(define class-dep-sema (make-semaphore 1))
(define cid->paths (make-hasheqv))
(define path->cids (make-hash))

(define (register-class-deps cid import-map)
  (semaphore-wait class-dep-sema)
  (define old-deps (hash-ref cid->paths cid (cons #f (make-hash))))
  (for ([path (in-hash-values (cdr old-deps))])
    (set-remove! (hash-ref path->cids path) cid))
  (hash-set! cid->paths cid (cons (now/moment/utc) import-map))
  (for ([path (in-hash-values import-map)])
    (set-add! (hash-ref! path->cids path mutable-set) cid))
  (semaphore-post class-dep-sema))
  

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
  


;; TODO?
(define (read-inline-struct ignore port . args)
  (define lst (read port))
  (cond [(list? lst)
         (define cid (car lst))
         (define fields (cdr lst))
         (define classinfo/v (query-row _dbc_ class-load-stmt cid))
         (define classname (string->symbol (vector-ref classinfo/v 0)))
         (define classfile (string->path (vector-ref classinfo/v 1)))
         (define classfile/resolved (build-path lib-path classfile))
         (define changes (dynamic-rerequire classfile/resolved))
         (define struct-maker (dynamic-require classfile/resolved classname))
         (apply struct-maker fields)]
        [else #f]))

(define void-reader (make-readtable #f
                                    #\< 'dispatch-macro read-unreadable
                                    #\{ 'dispatch-macro read-object
                                    #\@ 'dispatch-macro read-inline-struct))



(define lib-path #f)

(define (set-lib-path! p)
  (unless (path-string? p) (raise-argument-error 'set-lib-path! "path-string?" p))
  (set! lib-path (path->directory-path (simplify-path (string->path p)))))

(define-syntax-rule (filepath)
  (variable-reference->resolved-module-path (#%variable-reference)))

(define-syntax-rule (filepath/index)
  (variable-reference->module-path-index (#%variable-reference)))

(define (relative-module-path mod-path)
  (let ([mpath (resolved-module-path-name mod-path)])
    (cond [(path? mpath) (find-relative-path lib-path mpath)]
          [(cons? mpath)  (cons (find-relative-path lib-path (car mpath)) (cdr mpath))])))





(define (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-database-connection!
   (case db-type
     [(mysql) (mysql-connect
               #:user db-user
               #:database db-db
               #:server (if db-srv db-srv "localhost")
               #:port (if db-port db-port 3306)
               #:socket db-sock
               #:password db-pass)]
     [(postgres) (postgresql-connect
                  #:user db-user
                  #:database db-db
                  #:server (if db-srv db-srv "localhost")
                  #:port (if db-port db-port 5432)
                  #:socket db-sock
                  #:password db-pass)]
     [(sqlite) (sqlite3-connect
                #:database db-db)]
     [(odbc) (odbc-connect
              #:user db-user
              #:dsn db-db
              #:password db-pass)]
     [else (raise-argument-error 'database-setup "dbtype?" db-type)])))
  

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
   (define hash-proc  (lambda (l hash-code) (hash-code (cons 'lazy-ref (lazy-ref-id l)))))
   (define hash2-proc (lambda (l hash-code) (hash-code (cons 'lazy-ref (lazy-ref-id l)))))])

(struct lazy-ref:weak lazy-ref ()
  #:methods gen:custom-write
  [(define write-proc mud-ref-print)]
  #:methods gen:equal+hash
  [(define equal-proc (lambda (a b r?) (saved-object=? a b)))
   (define hash-proc  (lambda (l hash-code) (hash-code (cons 'lazy-ref (lazy-ref-id l)))))
   (define hash2-proc (lambda (l hash-code) (hash-code (cons 'lazy-ref (lazy-ref-id l)))))])

(define (oref o [weak? #f])
  (unless (is-a? o saved-object%)
    (raise-argument-error 'oref "(is-a?/c saved-object%)" o))
  ((if weak? lazy-ref:weak lazy-ref) (get-field id o) (if weak? (make-weak-box o) o)))

(define (saved-object=? a b)
  (let ([a-id (if (lazy-ref? a) (lazy-ref-id a) (get-field id a))]
        [b-id (cond [(lazy-ref? b) (lazy-ref-id b)]
                    [(and (object? b) (is-a? b saved-object%)) (get-field id b)]
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


(define saveable<%> (interface () save save-index load on-create on-load lock! unlock! updated))

(define saved-object% (class* object% (writable<%> saveable<%> equal<%>)
                        (super-new)
                        (init-field [id (void)]
                                    [name "object"])
                        (field      
                         ;; tags : (HashTable Symbol (Setof Symbol))
                         [last-access (now/moment/utc)]
                         [last-update #f]
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
                          (cond [(lazy-ref? other) (= id (lazy-ref-id other))]
                                [(is-a? other saved-object%) (= id (get-field id other))]
                                [else #f]))

                        (define/public (equal-hash-code-of hash-code)
                          (hash-code id))

                        (define/public (equal-secondary-hash-code-of hash-code)
                          (hash-code id))
                        
                        (abstract get-cid)

                        (define/public (on-create) (set-field! loaded this (now/moment/utc)))
                        (define/public (on-load) (set-field! loaded this (now/moment/utc)))
                      
                        ; (load flds) initializes each field with the values in the given hash map
                        (define/public (load flds) (void))

                        (define/public (custom-write port)
                          (write #\# port)
                          (write #\{ port)
                          (write id port)
                          (write #\} port))
                          
                        (define/public (custom-display port)
                          (display  "#<saved-object ...>" port))

                        ;; (updated) notes that the object has been updated just now (call this any time you change the object outside of
                        ;;  set-field! which already does this)
                        (define/public (updated)
                          (set! last-update (now/moment/utc)))

                        (define lock-sema (make-semaphore 1))
                        (define/public (lock!)
                          (semaphore-wait lock-sema))

                        (define/public (unlock!)
                          (semaphore-post lock-sema))
                        
                        
                        (will-register object-executor this database-save-object)))


(define-syntax (send/griftos stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ...)
     #'(send (let [(o obj-expr)]
               (if (lazy-ref? o) (lazy-deref o) o))
             method-id arg ...)]))

#| TODO:

send/apply
send/keyword-apply
dynamic-send
send*
with-method

dynamic-get-field
dynamic-set-field!
field-bound?
class-field-accessor
class-field-mutator
|#

(define-syntax (get-field/griftos stx)
  (syntax-case stx ()
    [(_ field-id obj-expr)
     #'(get-field field-id (let [(o obj-expr)]
                             (if (lazy-ref? o) (lazy-deref o) o)))]))

(define-syntax (set-field!/griftos stx)
  (syntax-case stx ()
    [(_ field-id obj-expr value-expr)
     #'(begin
         (set-field! field-id (let [(o obj-expr)]
                                (if (lazy-ref? o) (lazy-deref o) o))
                     value-expr)
         (when (lazy-ref? o)
           (send (lazy-deref o) updated)))]))

;; (define-saved-class name super-expression mud-defn-or-expr ...) defines a saved-class descended from super-expression
;; mud-defn-or-expr: as the regular class defn-or-expr, but with nosave varieties for all fields 

(define-syntax (define-saved-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)  body ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (define-saved-class/priv orig-stx
                         name
                         super-expression
                         (interface-expr ...)
                         body ...)))]))

;; (define-saved-class name super-expression body ...) is equivalent to (define-saved-class* name super-expression () body ...)

(define-syntax (define-saved-class stx)
  (syntax-case stx ()
    [(_ name super-expression body ...)
     (with-syntax ([orig-stx stx])
       (syntax/loc stx (define-saved-class/priv orig-stx
                         name
                         super-expression
                         () 
                         body ...)))]))



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
        [indexed-class-vars '()])
    (define (wrap-def-or-expr doe)
      (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave init-field
                         init-field/nosave define-values/index field/index init-field/index define/index begin)
        [(define  id expr) (begin (set! saved-class-vars (cons #'id saved-class-vars)) #'(define id expr))]
        [(define/nosave  id expr) #'(define id expr)]
        [(define/index   id expr) (begin (set! indexed-class-vars (cons #'id indexed-class-vars)) #'(define id expr))]
        [(define-values (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! saved-class-vars (cons #'id saved-class-vars))))
                                                        (syntax->list #'(id ...)))
                                              #'(define-values (id ...) expr))]
        [(define-values/index (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! indexed-class-vars
                                                                                                   (cons #'id indexed-class-vars))))
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
        [(field/nosave field-decl ...) #'(field field-decl ...)]
        [(init-field/nosave init-decl ...) #'(init-field init-decl ...)]
        [(begin clause ...)
         (let ([wrapped-subclauses (map wrap-def-or-expr (syntax->list #'(clause ...)))])
           (with-syntax ([(wrapped-clause ...) wrapped-subclauses])
             #'(begin wrapped-clause ...)))]
        [(default ...) #'(default ...)]))
    (values (map wrap-def-or-expr def-or-exprs)
            saved-class-vars
            indexed-class-vars)))



(define-syntax (mixin/saved stx)
  (syntax-case stx ()
    [(_ (from ...) (to ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars)
                   (wrap-saved-class-exprs (syntax->list #'(body ...)))])
       (let  (
              ;; List of syntaxes needed to save the saved-fields
              [save-list (map (λ (id) (with-syntax ([id id])
                                        #'(hash-set! result 'id id)))
                              saved-class-vars)]
              [save-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(hash-set! result 'id id)))
                                    indexed-class-vars)]
              ;; List of syntaxes needed to load the saved-fields
              [load-list (map (λ (id) (with-syntax ([id id])
                                        #'(set! id (hash-ref vars 'id void))))
                              (append indexed-class-vars saved-class-vars))])
         (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                       [(var-save ...) save-list]
                       [(index-save ...) save-index-list]
                       [(var-load ...) load-list])
           #'(mixin (saveable<%> from ...) (to ...)
               (define/override (save)
                 (let ([result (super save)])
                   var-save ...
                   result))
               (define/override (save-index)
                 (let ([result (super save-index)])
                   index-save ...
                   result))
               (define/override (load vars)
                 var-load ...
                 (super load vars))
               def-or-exp ...))))]))
           

                             



                       

(define-syntax (define-saved-class/priv stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...) body ...)
     (let-values ([(wrapped-def-or-exprs saved-class-vars indexed-class-vars)
                   (wrap-saved-class-exprs (syntax->list #'(body ...)))])
       (let* (;; List of syntaxes needed to save the saved-fields
              [save-list (map (λ (id) (with-syntax ([id id])
                                        #'(hash-set! result 'id id)))
                              saved-class-vars)]
              [save-index-list (map (λ (id) (with-syntax ([id id])
                                              #'(hash-set! result 'id id)))
                                    indexed-class-vars)]
              ;; List of syntaxes needed to load the saved-fields
              [load-list (map (λ (id) (with-syntax ([id id])
                                        #'(set! id (hash-ref vars 'id void))))
                              (append indexed-class-vars saved-class-vars))])
         (with-syntax ([(def-or-exp ... ) wrapped-def-or-exprs]
                       [(var-save ...) save-list]
                       [(index-save ...) save-index-list]
                       [(var-load ...) load-list]
                       [name/string (string-append "#<saved-object:" (symbol->string (syntax->datum #'name)) ">")])
           (syntax/loc stx
             (begin
               (define name undefined)
               (local [(define name/cid (database-get-cid! 'name (relative-module-path (filepath))))]
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
                         (define/override (load vars)
                           var-load ...
                           (super load vars))
                         (define/override (custom-display port)
                           (display name/string port))
                         def-or-exp ...)))
               (provide name))))))]))


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
  (moment year month day hour minute second nanosecond #:tz (if tz tz (current-timezone))))

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

;; (get-object id) produces the object with the ID id, or #f if none is currently loaded
;; get-object: Lazy-Ref -> (U (Instance Mud-Object%) #f)

(define (get-object id)
  (semaphore-wait object-table/semaphore)
  (define o (hash-ref object-table id #f))
  (begin0
    (if (and o (weak-box-value o))
        (weak-box-value o)
        (if o (begin
                (hash-remove! object-table id)
                #f)
            #f))
    (semaphore-post object-table/semaphore)))

;; (lazy-deref) produces the object pointed to by lazy-ref (from the object table if possible, the database otherwise)
;;  produces #f if the object cannot be found in the otable or the database (i.e. the object has been deleted)
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
  (when(and obj (lazy-ref:weak? lr))
    (set! obj (weak-box-value obj)))
  (if obj obj
      (let ([o (or (get-object id) (database-load id))])
        (if (lazy-ref:weak? lr)
            (set-lazy-ref-obj! lr (make-weak-box o))
            (set-lazy-ref-obj! lr o))
        o)))

(define db/semaphore (make-semaphore 1))
(define _dbc_ #f)

(define class-load-stmt #f)
(define object-load-stmt #f)
(define get-classid-stmt #f)
(define create-classid-stmt #f)
(define delete-fields-stmt #f)
(define delete-tags-stmt #f)
(define save-object-stmt #f)
(define new-object-stmt #f)
(define save-tag-stmt #f)
(define save-field-stmt #f)
(define load-tags-stmt #f)
(define load-fields-stmt #f)
(define search-tags-stmt #f)
(define search-index-stmt #f)
(define get-singleton-stmt #f)
(define new-singleton-stmt #f)
(define new-text-stmt #f)
(define get-text-stmt #f)
(define get-text-id-stmt #f)
(define add-text-trans-stmt #f)
(define log-stmt #f)
(define logger-thread #f)


(define (database-connected?)
  (and (connection? _dbc_) (connected? _dbc_)))

(define (database-disconnect)
  (when (connection? _dbc_)
    (disconnect _dbc_)
    (set! _dbc_ #f)))

(define (set-database-connection! conn)
  (unless (connection? conn) (raise-argument-error 'set-database-connection! "connection?" conn))
  (if (symbol=? (dbsystem-name (connection-dbsystem conn)) 'sqlite3) ;; TODO, does anything else not support timestamp???
      (set! database-date-supported #f)
      (set! database-date-supported #t))
  
  (local ([define is-postgres? (symbol=? 'postgresql (dbsystem-name (connection-dbsystem conn)))]
          [define has-bool? (memq (dbsystem-name (connection-dbsystem conn)) '(postgresql mysql odbc))])
    (when _dbc_ (disconnect _dbc_))
    (set! _dbc_ conn)
    (set! object-load-stmt (prepare conn (format "SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
                                                 (if is-postgres? "$1" "?"))))
    (set! class-load-stmt (prepare conn (format "SELECT classname, module FROM classes WHERE cid = ~a"
                                                (if is-postgres? "$1" "?"))))
    (set! get-classid-stmt (prepare conn (format "SELECT cid FROM classes WHERE classname = ~a AND module = ~a"
                                                 (if is-postgres? "$1" "?")
                                                 (if is-postgres? "$2" "?"))))

    (set! create-classid-stmt (prepare conn (format "INSERT INTO classes (classname,module) VALUES (~a , ~a)"
                                                    (if is-postgres? "$1" "?")
                                                    (if is-postgres? "$2" "?"))))
    
    (set! delete-fields-stmt (prepare conn (format "DELETE FROM indexed_fields WHERE oid = ~a"
                                                   (if is-postgres? "$1" "?"))))
    (set! delete-tags-stmt (prepare conn (format "DELETE FROM tags WHERE oid = ~a"
                                                 (if is-postgres? "$1" "?"))))

    (set! new-object-stmt (prepare conn (format "INSERT INTO objects (cid, created, name, deleted) VALUES (~a, ~a, ~a, ~a) ~a"
                                                (if is-postgres? "$1" "?")
                                                (if is-postgres? "$2" "?")
                                                (if is-postgres? "$3" "?")
                                                (if has-bool? "false" "0")
                                                (if is-postgres? "RETURNING oid" ""))))

    (set! save-object-stmt (prepare conn (format "UPDATE objects SET name=~a, saved=~a, fields=~a WHERE oid = ~a"
                                                 (if is-postgres? "$1" "?")
                                                 (if is-postgres? "$2" "?")
                                                 (if is-postgres? "$3" "?")
                                                 (if is-postgres? "$4" "?"))))

    (set! save-tag-stmt (prepare conn (format "INSERT INTO tags (oid, category, tag) values (~a, ~a, ~a)"
                                              (if is-postgres? "$1" "?")
                                              (if is-postgres? "$2" "?")
                                              (if is-postgres? "$3" "?"))))

    (set! save-field-stmt (prepare conn (format "INSERT INTO indexed_fields (oid, field, value) values (~a, ~a, ~a)"
                                                (if is-postgres? "$1" "?")
                                                (if is-postgres? "$2" "?")
                                                (if is-postgres? "$3" "?"))))

    (set! load-tags-stmt (prepare conn (format "SELECT category, tag FROM tags WHERE oid = ~a"
                                               (if is-postgres? "$1" "?"))))

    (set! load-fields-stmt (prepare conn (format "SELECT field, value FROM indexed_fields WHERE oid = ~a"
                                                 (if is-postgres? "$1" "?"))))

    (set! search-tags-stmt (prepare conn (format "SELECT oid FROM tags WHERE category = ~a AND tag = ~a"
                                                 (if is-postgres? "$1" "?")
                                                 (if is-postgres? "$2" "?"))))

    (set! search-index-stmt (prepare conn (format "SELECT oid FROM indexed_fields WHERE field = ~a AND value = ~a"
                                                  (if is-postgres? "$1" "?")
                                                  (if is-postgres? "$2" "?"))))

                                              
    (set! get-singleton-stmt (prepare conn (format "SELECT oid FROM singletons WHERE cid = ~a"
                                                   (if is-postgres? "$1" "?"))))

    (set! new-singleton-stmt (prepare conn (format "INSERT INTO singletons (oid, cid) values (~a, ~a)"
                                                   (if is-postgres? "$1" "?")
                                                   (if is-postgres? "$2" "?"))))
                                                
    (set! log-stmt (prepare conn (format "INSERT INTO logfile (level, module, description, backtrace) values (~a, ~a, ~a, ~a)"
                                         (if is-postgres? "$1" "?")
                                         (if is-postgres? "$2" "?")
                                         (if is-postgres? "$3" "?")
                                         (if is-postgres? "$4" "?"))))

    (set! new-text-stmt (prepare conn (format "INSERT INTO localization (lang, enc, txt) values (~a, ~a, ~a) ~a"
                                              (if is-postgres? "$1" "?")
                                              (if is-postgres? "$2" "?")
                                              (if is-postgres? "$3" "?")
                                              (if is-postgres? "RETURNING tid" ""))))

    (set! get-text-id-stmt (prepare conn (format "SELECT tid from LOCALIZATION where lang=~a AND enc=~a AND txt=~a"
                                    (if is-postgres? "$1" "?")
                                    (if is-postgres? "$2" "?")
                                    (if is-postgres? "$3" "?"))))
    

    (set! get-text-stmt (prepare conn (format "SELECT txt from LOCALIZATION WHERE tid=~a AND lang=~a AND enc=~a"
                                 (if is-postgres? "$1" "?")
                                 (if is-postgres? "$2" "?")
                                 (if is-postgres? "$3" "?"))))
                                              
    (set! add-text-trans-stmt (prepare conn (format "INSERT INTO localization (tid, lang, enc, txt) values (~a, ~a, ~a, ~a)"
                                                    (if is-postgres? "$1" "?")
                                                    (if is-postgres? "$2" "?")
                                                    (if is-postgres? "$3" "?")
                                                    (if is-postgres? "$4" "?"))))
                                       

    (when (thread? logger-thread)
      (kill-thread logger-thread))

    (set! logger-thread
          (thread
           (λ ()
             (let loop ()
               (match (sync griftos-log-rec)
                 [(vector level msg data topic)
                  (database-log level (or topic "racket") msg (backtrace data))])
               (loop)))))))

(define (log-level->int ll)
  (or (index-of '(none fatal error warning info debug) ll eq?) 0))

(define griftos-logger (make-logger #f (current-logger) 'info #f))
                                    
(current-logger griftos-logger)
                           
(define griftos-log-rec (make-log-receiver griftos-logger 'warning #f 'debug 'griftos))
                           
(define (backtrace cms)
  (if (continuation-mark-set? cms)
      (let ([out (open-output-string)])
        (for ([context (in-list (continuation-mark-set->context cms))])
          (displayln (format "~a : ~a" (or (car context) "???") (if (srcloc? (cdr context)) (srcloc->string (cdr context))
                                                                    "No source information available")) out))
        (get-output-string out))
      #f))

(define (database-log level module code description)
  ;(-> log-level/c (or/c string? false/c) (or/c exact-nonnegative-integer? false/c) string? void?)
  (query-exec _dbc_ log-stmt
              (log-level->int level)
              (if module module sql-null)
              (if code code sql-null)
              description))
              

(define (database-disconnect!)
  (when (database-connected?)
    (disconnect _dbc_)
    (set! _dbc_ #f)))


;(read (open-input-string (gzip/bytes (bytes->string/utf-8 (_db_load id)))))))

(define (database-load id)
  (define db-val (database-get-object id))
  (when db-val
    (semaphore-wait object-table/semaphore)
    (hash-set! object-table id (make-weak-box db-val))
    (semaphore-post object-table/semaphore))
  db-val)
        



(define cid-map (make-hash))

#|! 
(database-get-cid! class-name module-name) retrieves the classid for the named class variable defined in the named module
  if the database does not contain this key, a new row is inserted

database-get-cid! : Symbol Path -> Nat
|#
(define (database-get-cid! class-name module-name)
  (when (not (database-connected?)) (error "database not connected"))
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




;; database-get-object: Nat -> (U (Instanceof MudObject%) #f)
(define (database-get-object id)
  (unless (database-connected?) (error "database not connected"))
  ;"SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
  (semaphore-wait db/semaphore)
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  (define tags (query-rows _dbc_ load-tags-stmt id))
  (define indexed-fields (make-hasheq (map (λ (v)
                                             (cons (string->symbol (vector-ref v 0))
                                                   (parameterize ([current-readtable void-reader]) (read (open-input-bytes (vector-ref v 1))))))
                                           (query-rows _dbc_ load-fields-stmt id))))
  (semaphore-post db/semaphore)
  
  (if (and obj (or (false? (vector-ref obj 4))
                   (zero? (vector-ref obj 4))))
      (local [(define classinfo/v (query-row _dbc_ class-load-stmt (vector-ref obj 0)))
              (define fields (parameterize ([current-readtable void-reader]) (read (open-input-bytes (vector-ref obj 5)))))
              (define classname (string->symbol (vector-ref classinfo/v 0)))
              (define classfile (string->path (vector-ref classinfo/v 1)))
              (define classfile/resolved (build-path lib-path classfile))
              (define changes (dynamic-rerequire classfile/resolved)) ;; TODO: Mark changed mudlib source files
              (define class (dynamic-require classfile/resolved classname))
              (define new-object (new class [id id]))
              (define new-object/tags (get-field tags new-object))
              (define created (dbtime->moment (vector-ref obj 1)))
              (define saved (dbtime->moment (vector-ref obj 2)))
              ]

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
            (if (hash-has-key? new-object/tags category)
                (set-add! (hash-ref new-object/tags category) tag)
                (hash-set! new-object/tags category (mutable-seteq tag)))))
                
        (send new-object on-load)
        (semaphore-wait object-table/semaphore)
        (hash-set! object-table id (make-weak-box new-object))
        (semaphore-post object-table/semaphore)
        new-object)
      #f))
  

(define (save-object oref)
  (unless (or (object? oref) (lazy-ref? oref))
    (raise-argument-error 'save-object "(or/c object? lazy-ref?)" oref))
  (define o (if (lazy-ref? oref) (get-object (lazy-ref-id oref)) oref))
  (if o (database-save-object o) #f))

;; database-save-object: (InstanceOf MudObject%) -> Bool

(define (database-save-object o)
  (unless (database-connected?) (error "database not connected"))
  (define oid (get-field id o))
  (define name (get-field name o))
  
  ;(eprintf "fields=~a\n" fields)
  (define indexed-fields (send o save-index))
  (define saved  (now/moment/utc))
  (define old-saved (get-field saved o))

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
    (define fields/port (open-output-bytes))
    (write (send o save) fields/port)
    (query-exec _dbc_ save-object-stmt name (moment->dbtime saved) (get-output-bytes fields/port) oid)
    (close-output-port fields/port)
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
    (commit-transaction _dbc_)
    #t))


(define-syntax (get-singleton stx)
  (syntax-case stx()
    [(_ cls (_id arg) ...)
     #'(let ([c cls])
         (unless (subclass? c saved-object%) (raise-argument-error 'get-singleton "(subclass?/c saved-object)" c))
         (let* ([o (new c (_id arg) ...)]
                [cid (send o get-cid)]
                [oid (query-maybe-value _dbc_ get-singleton-stmt cid)])
           (if oid
               (lazy-ref oid #f)
               (begin
                 (database-new-object o)
                 (query-exec _dbc_ new-singleton-stmt (get-field id o) cid)
                 (lazy-ref (get-field id o) o)))))]))

(define-syntax (is-a?/griftos stx)
  (syntax-case stx ()
    [(_ v-expr type)
     #'(let ([v v-expr])
         (is-a? (if (lazy-ref? v) (lazy-deref v) v) type))]))


(struct is-a?-ctc/griftos (<%>)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
     (define <%> (is-a?-ctc/griftos-<%> ctc))
     (define name (object-name <%>))
     (cond [name `(is-a?/c ,name)]
           [(class? <%>) '(is-a?/c unknown%)]
           [else '(is-a?/c unknown<%>)]))
   #:first-order
   (λ (ctc)
     (define <%> (is-a?-ctc/griftos-<%> ctc))
     (λ (v)
       (is-a?/griftos v <%>)))
   #:stronger
   (λ (this other)
     (define <%> (is-a?-ctc/griftos-<%> this))
     (if (is-a?-ctc/griftos other)
         (let ([other<%> (is-a?-ctc/griftos-<%> other)])
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
     (and (is-a?-ctc/griftos other)
          (equal? (is-a?-ctc/griftos-<%> this)
                  (is-a?-ctc/griftos-<%> other))))))
         
(define (is-a?/c/griftos type)
  (unless (or (interface? type)
              (class? type))
    (raise-argument-error
     'is-a?/c
     ("or/c interface? class?")
     type))
  (is-a?-ctc/griftos type))

  


(define-syntax (new/griftos stx)
  (syntax-case stx ()
    [(_ cls-expr (id arg) ...)
     #'(let* ([cv cls-expr]
              [o (new cv (id arg) ...)])
         (when (subclass? cv saved-object%)
           (database-new-object o))
         (if (subclass? cv saved-object%)  (oref o) o))]))


(define-syntax (instantiate/griftos stx)
  (syntax-case stx ()
    [(_ cls-expr (by-pos-expr ...) (id by-name-expr) ...)
     #'(let ([cv cls-expr]
             [o (instantiate cv (by-pos-expr ...) (id by-name-expr) ...)])
         (when (subclass? cv saved-object%)
           (database-new-object o))
         (if (subclass? cv saved-object%)  (oref o) o))]))


(define-syntax (make-object/griftos stx)
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
  

(define (database-new-object o)
  (define cid (send o get-cid))
  (define name (get-field name o))
  (define created (get-field created o))
  (send o on-create)
  ;"INSERT INTO objects (class, created, name, deleted) VALUES (~v, ~v, ~v, false)
  (define new-obj-result (query _dbc_ new-object-stmt cid (moment->dbtime created) name))
  (cond [(and (simple-result? new-obj-result) (assoc 'insert-id (simple-result-info new-obj-result)))
         (set-field! id o (cdr (assoc 'insert-id (simple-result-info new-obj-result))))]
        [(and (rows-result? new-obj-result)
              (= 1 (length (rows-result-headers new-obj-result)))
              (= 1 (length (rows-result-rows new-obj-result))))
         (set-field! id o (vector-ref (first (rows-result-rows new-obj-result)) 0))]
        [else (error 'new "unexpected database result (~v) when instantiating new object" new-obj-result)])
  
  (send o on-load)
  (database-save-object o)
  (semaphore-wait object-table/semaphore)
  (hash-set! object-table (get-field id o) (make-weak-box o))
  (semaphore-post object-table/semaphore)
  (oref o))

#|||||||||||||||||||||||||||||||||||||||||||

  Localization

|||||||||||||||||||||||||||||||||||||||||||#

(struct text (id default) #:transparent)

(define (make-text txt  [lang 'eng] [enc 'ASCII])
  (if (database-connected?)
      (let ([result (query-maybe-value _dbc_ get-text-id-stmt (string-foldcase (symbol->string lang)) (symbol->mib enc) txt)])
        (text (if result result (make-new-text txt lang enc)) txt))
      (text #f txt)))
  
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
  (if (and (database-connected?) (text-id t))
      (or (and (symbol->mib enc) (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib enc)))
          (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib 'UTF-8))
          (query-maybe-value _dbc_ get-text-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib 'ASCII))
          (text-default t))
      (text-default t)))

(define (add-text-translation t lang enc txt)
  (when (and (database-connected?) (text-id t))
    (query-exec add-text-trans-stmt (text-id t) (string-foldcase (symbol->string lang)) (symbol->mib enc) txt)))

#||||||||||||||||||||||||||||||||||||||||||||

  Object Management Stuff

||||||||||||||||||||||||||||||||||||||||||||#


(define object-executor (make-will-executor))

(define executor-thread
  (thread (λ()
            (let loop ()
              (will-execute object-executor)
              (loop)))))


