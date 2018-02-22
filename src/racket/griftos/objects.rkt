#lang racket

(provide mud-object% define-mud-class* define-mud-class define-mud-struct mud-class mud-class? mud-class-cid mud-class-class subclass?/mud subclass?/mud/c implementation?/mud implementation?/mud/c is-a?/mud/c)
(provide object-table lazy-ref lazy-ref? lazy-ref/evt? lazy-ref/evt-evt get-object oref save-object get-singleton new/griftos send/griftos get-field/griftos set-field!/griftos)
(provide database-setup)

(provide tickable<%> tickable? listener<%> listener?)

;(provide database-connected? set-database-connection!)
(require racket/class)
(require db srfi/19 db/util/datetime)
(require racket/rerequire)
(require racket/fasl)
(require racket/hash)
(require racket/undefined)

(define (read-unreadable ignore port . args)
  (define (read-unreadable/acc chars)
    (define next-char (read-char port))
    (cond [(char=? #\> next-char)
           (case (string->symbol (list->string (reverse chars)))
             [(void) (void)]
             [(undefined) undefined]
             [else (error 'read "unreadable value encountered : #<~a>" (string->symbol (list->string (reverse chars))))])]
          [else (read-unreadable/acc (cons next-char chars))]))
  (read-unreadable/acc empty))

(define void-reader (make-readtable #f #\< 'dispatch-macro read-unreadable))

(provide lib-path filepath relative-module-path set-lib-path!)

(define lib-path #f)

(define (set-lib-path! p)
  (unless (path-string? p) (raise-argument-error 'set-lib-path! "path-string?" p))
  (set! lib-path p))

(define-syntax-rule (filepath)
  (variable-reference->resolved-module-path (#%variable-reference)))

;(make-relative-module-path lib-path resolved-module-path)

(define (relative-module-path mod-path)
  (let ([mpath (resolved-module-path-name mod-path)])
    (cond [(path? mpath) (find-relative-path lib-path mpath)]
          [(cons? mpath)  (cons (find-relative-path lib-path (car mpath)) (cdr mpath))])))





(define (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
  (set-database-connection!
   (case db-type
     [('mysql) (mysql-connect
                #:user db-user
                #:database db-db
                #:server (if db-srv db-srv "localhost")
                #:port (if db-port db-port 3306)
                #:socket db-sock
                #:password db-pass)]
     [('postgres) (postgresql-connect
                   #:user db-user
                   #:database db-db
                   #:server (if db-srv db-srv "localhost")
                   #:port (if db-port db-port 5432)
                   #:socket db-sock
                   #:password db-pass)]
     [('sqlite) (sqlite3-connect
                 #:database db-db)]
     [('odbc) (odbc-connect
               #:user db-user
               #:dsn db-db
               #:password db-pass)]
     [else (raise-argument-error 'database-setup "dbtype?" db-type)])))
  


(struct lazy-ref (id) #:transparent)

(struct lazy-ref/evt lazy-ref (evt) #:transparent
  #:property prop:evt (struct-field-index evt))

(define (oref o)
  (unless (is-a? o mud-object%)
    (raise-argument-error 'oref "(is-a? o mud-object%)" o))
  (lazy-ref (get-field id o)))
  

#| This ended up being a very dumb idea

(define (register-class classname path)
  (unless (symbol? classname) (raise-argument-error 'register-class "symbol?" classname))
  (hash-set! classlist classname path))

  
(register-class 'mud-object% #f) ;; this is automatically loaded
|#

(struct mud-class (cid class))

(define (subclass?/mud class super-class)
  (unless (mud-class? super-class) (raise-argument-error 'subclass?/mud "mud-class?" super-class))
  (and
   (mud-class? class)
   (subclass?
    (mud-class-class class)
    (mud-class-class super-class))))


(define (subclass?/mud/c super-class)
  (unless (mud-class? super-class) (raise-argument-error 'subclass?/mud/c "mud-class?" super-class))
  (define superc (mud-class-class super-class))
  (lambda (v)
    (and (mud-class? v)
         (subclass? (mud-class-class v)) superc)))
  
(define (implementation?/mud v intf)
  (unless (interface? intf) (raise-argument-error 'implementation?/mud "interface?" intf))
  (and (mud-class? v)
       (implementation? (mud-class-class v) intf)))

(define (implementation?/mud/c intf)
  (unless (interface? intf) (raise-argument-error 'implementation?/mud/c "interface?" intf))
  (lambda (v)
    (and (mud-class? v)
         (implementation? (mud-class-class v) intf))))

(define (is-a?/mud v type)
  (unless (or (interface? type) (mud-class? type)) (raise-argument-error 'is-a?/mud "(or/c interface? mud-class?)" type))
  (cond [(not (lazy-ref? v)) #f]
        [(interface? type) (is-a? (lazy-deref v) type)]
        [else (is-a? (lazy-deref v) (mud-class-class type))]))

(define (is-a?/mud/c type)
  (unless (or (interface? type) (mud-class? type)) (raise-argument-error 'is-a?/mud "(or/c interface? mud-class?)" type))
  (lambda (v)
    (cond [(not (lazy-ref? v)) #f]
          [(interface? type) (is-a? (lazy-deref v) type)]
          [else (is-a? (lazy-deref v) (mud-class-class type))])))

(define mud-object% (class object% 
                      (super-new)
                      (init-field [id (void)]
                                  [name "object"])
                      (field      
                       ;; tags : (HashTable Symbol (Setof Symbol))
                       [last-accessed (current-date)]
                       [saved (current-date)]
                       [created (current-date)]
                       [tags  (make-hasheq)])

                      
                      (field [loaded #f]) ; when it was loaded from the database
                      
                      ; (save) for a mud object produces a hash that maps field identifier symbols to values
                      (define/public (save)
                        (make-hasheq))

                      (define/public (save-index)
                        (make-hasheq))

                      (abstract get-cid)

                      (define/public (on-create) (set-field! loaded this (current-date)))
                      (define/public (on-load) (set-field! loaded this (current-date)))
                      
                      ; (load flds) initializes each field with the values in the given hash map
                      (define/public (load flds) (void))))


(define-syntax (send/griftos stx)
  (syntax-case stx ()
    [(_ obj-expr method-id arg ...)
     #'(send (let [(o obj-expr)]
               (unless (lazy-ref? o) (raise-argument-error 'send "lazy-ref?" o))
               (let [(ob (lazy-deref o))]
                 (when (void? ob) (raise-argument-error 'send "lazy-ref to valid object" o))
                 ob)) method-id arg ...)]))


(define-syntax (get-field/griftos stx)
  (syntax-case stx ()
    [(_ field-id obj-expr)
     #'(get-field field-id (let [(o obj-expr)]
                             (unless (lazy-ref? o) (raise-argument-error 'get-field "lazy-ref?" o))
                             (let [(ob (lazy-deref o))]
                               (unless ob (raise-argument-error 'get-field "lazy-ref to valid object"))
                               ob)))]))

(define-syntax (set-field!/griftos stx)
  (syntax-case stx ()
    [(_ field-id obj-expr value-expr)
     #'(set-field! field-id (let [(o obj-expr)]
                              (unless (lazy-ref? o) (raise-argument-error 'set-field! "lazy-ref?" o))
                              (let [(ob (lazy-deref o))]
                                (unless ob (raise-argument-error 'set-field! "lazy-ref to valid object"))
                                ob)) value-expr)]))

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
     (letrec (
              [saved-class-vars '()]
              [indexed-class-vars '()]
              [def-or-exprs (syntax->list #'(body ...))]
              ;; (wrap-def-or-expr doe) logs the (internal) id(s) if doe is a field definition (i.e. define, define-values, field, init-field) and leaves doe alone
              ;;                        converts doe to a regular field definition if it is a nosave variant (i.e. define/nosave, define-values/nosave etc.) (does not log id in this case)
              ;;                        recurses into subclauses if doe is a (begin ...) expression
              ;;                        and otherwise leaves doe alone (i.e. if it is an expression or method definition)
              [wrap-def-or-expr (λ (doe)
                                  (syntax-case doe (define define/nosave define-values define-values/nosave field field/nosave init-field init-field/nosave define-values/index field/index init-field/index define/index begin)
                                    [(define  id expr) (begin (set! saved-class-vars (cons #'id saved-class-vars)) #'(define id expr))]
                                    [(define/nosave  id expr) #'(define id expr)]
                                    [(define/index   id expr) (begin (set! indexed-class-vars (cons #'id indexed-class-vars)) #'(define id expr))]
                                    [(define-values (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! saved-class-vars (cons #'id saved-class-vars))))
                                                                                    (syntax->list #'(id ...)))
                                                                          #'(define-values (id ...) expr))]
                                    [(define-values/index (id ...) expr) (begin (for-each (λ (id) (with-syntax ([id id]) (set! indexed-class-vars (cons #'id indexed-class-vars))))
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
                                    [(default ...) #'(default ...)]))] ; it's none of the above, leave it alone
              ;; wrapped-def-or-exprs is the remapped set of def-or-expr in the class
              [wrapped-def-or-exprs (map wrap-def-or-expr
                                         def-or-exprs)]
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
                     [(var-load ...) load-list]
                     [name/cid (datum->syntax #'orig-stx (string->symbol (string-append (symbol->string (syntax->datum #'name)) "/cid")))]
                     )
         #'(begin
             (define name/cid (database-get-cid! 'name (relative-module-path (filepath))))
             (define name
               (mud-class name/cid
                          (class* super-expression (interface-expr ...)
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
                            def-or-exp ...)))
             

             (provide name))))]))

#|||||||||||||||||||||||||||||||||||||||||||||||

                CLASS INTERFACES

||||||||||||||||||||||||||||||||||||||||||||||||#



;; guard-for-prop:tickable:  (Vector (X -> Real Real) (X -> Void))
(define (guard-for-prop:tickable v si)
  (unless (and (vector? v)
               (= 2 (vector-length v))
               (procedure? (vector-ref v 0))
               (procedure-arity-includes? (vector-ref v 0) 1)
               (procedure? (vector-ref v 1))
               (procedure-arity-includes? (vector-ref v 1) 1))
    (raise-argument-error 'guard-for-prop:tickable "(vector procedure? procedure?)" v))
  v)
(define-values (prop:tickable tickable? tickable-ref) (make-struct-type-property 'tickable guard-for-prop:tickable))

(define (tickable->evt t)
  (unless (tickable? t) (raise-argument-error 'tickable->evt "tickable?" t))
  (define tick-value (tickable-ref t))
  (define tick (vector-ref tick-value 0))
  (define tock (vector-ref tick-value 1))
  (wrap-evt (alarm-evt (tick t))
            (λ (e) (tock t))))



(define tickable<%> (interface* ()
                                ([prop:tickable
                                  (vector (λ (o)
                                            (define-values (tick offset) (send o get-tick))
                                            (+ (* tick (floor (/ (+ offset (current-inexact-milliseconds)) tick))) tick))
                                          (λ (o) (send o on-tock)))])
                                get-tick on-tock))


  
;; guard-for-prop:listener: (Vector (X -> AsyncChannel) (X String -> Any)
(define (guard-for-prop:listener v si)
  (unless (and (vector? v)
               (= 2 (vector-length v))
               (procedure? (vector-ref v 0))
               (procedure-arity-includes? (vector-ref v 0) 1)
               (procedure? (vector-ref v 1))
               (procedure-arity-includes? (vector-ref v 1) 2))
    (raise-argument-error 'guard-for-prop:listener "(vector procedure? procedure?)" v))
  v)
               
(define-values (prop:listener listener? listener-ref) (make-struct-type-property 'listener guard-for-prop:listener))

(define (listener->evt l)
  (unless (listener? l) (raise-argument-error 'listener->evt "listener?" l))
  (define chan ((vector-ref (listener-ref l) 0) l))
  (define on-msg (vector-ref (listener-ref l) 1))
  (wrap-evt chan (λ (msg) (on-msg l msg))))

(define listener<%> (interface* ()
                                ([prop:listener
                                  (vector (λ (o) (send o get-listener-channel))
                                          (λ (o msg) (send o on-message msg)))])
                                get-listener-channel on-message))








#|||||||||||||||||||||||||||||||||||||||||||||||

                DATABASE STUFF

||||||||||||||||||||||||||||||||||||||||||||||||#


(define object-table (make-hasheqv empty))

;; (get-object id) produces the object with the ID id, or #<void> if none is currently loaded
;; get-object: Lazy-Ref -> (U (Instance Mud-Object%) Void)

(define (get-object id)
  (hash-ref object-table id void))

;; (lazy-deref) produces the object pointed to by lazy-ref (from the object table if possible, the database otherwise)
;;  produces #f if the object cannot be found in the otable or the database (i.e. the object has been deleted)
;;  If the object was found, updates its last-access field to the current time
;; lazy-deref: Lazy-Ref -> (U (Instance Mud-Object%) #f)

(define (lazy-deref lr)
  (define result (lazy-deref/no-keepalive lr))
  (when result (set-field! last-access result (current-date)))
  result)

;; (lazy-deref/no-keepalive lr) is the same as lazy-deref except the object's last-access field is not updated
;;    Use this to query an object without making it seem "still in use"
(define (lazy-deref/no-keepalive lr)
  (unless (lazy-ref? lr) (raise-argument-error 'lazy-deref "lazy-ref?" lr))
  (define id (lazy-ref-id lr))
  (define o  (get-object id))
  (if (void? o) (database-load id) o))

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


(define (database-connected?)
  (and (connection? _dbc_) (connected? _dbc_)))

(define (set-database-connection! conn)
  (unless (connection? conn) (raise-argument-error 'set-database-connection! "connection?" conn))
  (local ([define is-postgres? (symbol=? 'postgresql (dbsystem-name (connection-dbsystem conn)))])
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

    (set! new-object-stmt (prepare conn (format "INSERT INTO objects (cid, created, name, deleted) VALUES (~a, ~a, ~a, false) ~a"
                                                (if is-postgres? "$1" "?")
                                                (if is-postgres? "$2" "?")
                                                (if is-postgres? "$3" "?")
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
                                                
                                      

    
    ))
  
(define (database-disconnect!)
  (when (database-connected?)
    (disconnect _dbc_)
    (set! _dbc_ #f)))


;(read (open-input-string (gzip/bytes (bytes->string/utf-8 (_db_load id)))))))

(define (database-load id)
  (define db-val (database-get-object id))
  (if (void? db-val)
      db-val
      (begin
        (hash-set! object-table id db-val)
        db-val)))



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

    

(define (database-get-object id)
  (unless (database-connected?) (error "database not connected"))
  ;"SELECT cid, created, saved, name, deleted, fields FROM objects WHERE oid = ~a"
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  (define tags (query-rows _dbc_ load-tags-stmt id))
  ;(eprintf "~s" obj)
  (define indexed-fields (make-hasheq (map (λ (v)
                                             (cons (string->symbol (vector-ref v 0))
                                                   (parameterize ([current-readtable void-reader]) (read (open-input-string (bytes->string/utf-8 (vector-ref v 1)))))))
                                           (query-rows _dbc_ load-fields-stmt id))))
  
  (if (and obj (not (vector-ref obj 4))) (local [(define classinfo/v (query-row _dbc_ class-load-stmt (vector-ref obj 0)))
                                                 (define fields (parameterize ([current-readtable void-reader]) (read (open-input-string (bytes->string/utf-8 (vector-ref obj 5))))))
                                                 (define classname (string->symbol (vector-ref classinfo/v 0)))
                                                 (define classfile (string->path (vector-ref classinfo/v 1)))
                                                 (define changes (dynamic-rerequire classfile)) ;; TODO: Mark changed mudlib source files so old instances can be refreshed
                                                 (define class (dynamic-require classfile classname))
                                                 (define new-object (new class [id id]))
                                                 (define new-object/tags (get-field tags new-object))
                                                 (define created (sql-datetime->srfi-date (vector-ref obj 1)))
                                                 (define saved (sql-datetime->srfi-date (vector-ref obj 2)))
                                                 ]

                                           ;;; TODO:  This should recompile the changed files, get the timestamps from the changed files, and updated all of their cid's with the timestamp
                                           ;;; TODO (future): Somewhere in the system is a thread that looks for instances where saved < cid.saved, and does a save -> reload to them
                                           ;(when (cons? changes) (eprintf "~v\n" changes))
                                           
                                           ;(define all-fields (hash-union fields indexed-fields))
                                           ;(eprintf "fields=~v\n" indexed-fields)
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
                                           (hash-set! object-table id new-object)
                                           new-object)
      (void)))
  
;(deserialize (read (open-input-string (bytes->string/utf-8 (_db_load id))))))
;(void))

(define (save-object oref)
  (unless (lazy-ref? oref) (raise-argument-error 'save-object "lazy-ref?" oref))
  (define o (get-object (lazy-ref-id oref)))
  (when o (database-save-object o )))

(define (database-save-object o)
  (unless (database-connected?) (error "database not connected"))
  (define oid (get-field id o))
  (define name (get-field name o))
  (define fields (string->bytes/utf-8 (format "~s" (send o save))))
  ;(eprintf "fields=~a\n" fields)
  (define indexed-fields (send o save-index))
  (define saved (srfi-date->sql-timestamp (current-date)))

  (set-field! saved o saved)
  
  (start-transaction _dbc_)
  (query-exec _dbc_ delete-fields-stmt oid)
  (query-exec _dbc_ delete-tags-stmt oid)
  (query-exec _dbc_ save-object-stmt name saved fields oid)
  (for ([(field value) (in-hash indexed-fields)])
    (query-exec _dbc_ save-field-stmt oid (symbol->string field) (string->bytes/utf-8 (format "~s" value))))
  (for* ([(category tags)
          (in-hash (get-field tags o))]
         [tag (in-set tags)])
    (query-exec _dbc_ save-tag-stmt oid category tag))
  (commit-transaction _dbc_))


(define-syntax (get-singleton stx)
  (syntax-case stx()
    [(_ cls (_id arg) ...)
     #'(begin
         (unless (mud-class? cls) (raise-argument-error 'get-singleton "mud-class?" cls))
         (local [(define cid (mud-class-cid cls))
                 (define oid (query-maybe-value _dbc_ get-singleton-stmt cid))]
           (if oid
               (lazy-ref oid) ; already exists
               (local [(define o (new/griftos cls (_id arg) ...))]
                 (query-exec _dbc_ new-singleton-stmt (get-field id o) cid)
                 o))))]))



(define-syntax (new/griftos stx)
  (syntax-case stx ()
    [(_ cls (id arg) ...)
     #'(local [(define c (mud-class-class cls))
               (define o (new c (id arg) ...))]
         (database-new-object o))]))

(define (database-new-object o)
  (define cid (send o get-cid))
  (define name (get-field name o))
  (define created (get-field created o))
  ;"INSERT INTO objects (class, created, name, deleted) VALUES (~v, ~v, ~v, false)
  (define new-obj-result (query _dbc_ new-object-stmt cid (srfi-date->sql-timestamp created) name))

  (cond [(and (simple-result? new-obj-result) (assoc 'insert-id (simple-result-info new-obj-result)))
         (set-field! id o (assoc 'insert-id (simple-result-info new-obj-result)))]
        [(and (rows-result? new-obj-result)
              (= 1 (length (rows-result-headers new-obj-result)))
              (= 1 (length (rows-result-rows new-obj-result))))
         (set-field! id o (vector-ref (first (rows-result-rows new-obj-result)) 0))]
        [else (error 'new "unexpected database result (~v) when instantiating new object" new-obj-result)])
  (send o on-create)
  (send o on-load)
  (database-save-object o)
  (hash-set! object-table (get-field id o) o)
  (oref o))
                

#||||||||||||||||||||||||||||||||||||||||||||

  Threading stuff

||||||||||||||||||||||||||||||||||||||||||||#

(define sync-threads (hasheqv))

(define (make-event-thunk obj)
  (define events (filter identity (list (if (evt? obj) obj #f)
                                        (if (tickable? obj) (tickable->evt obj) #f)
                                        (if (listener? obj) (listener->evt obj) #f))))

  (define evt (match events
                [(list e) e]
                [(list e ...) (apply choice-evt events)]))
  ;; the thunk for an object's thread will continue until either the thread is broken, or the sync event produces EOF
  (define (loop)
    (with-handlers ([exn:break #t])
      (unless (eof-object? (sync evt)) (loop))))
  loop)


(define (add-event o)
  (unless (lazy-ref? o) (raise-argument-error 'add-event "lazy-ref?" o))
  (define id (lazy-ref-id o))
  (define obj (lazy-deref o))
  (unless (or (evt? obj)
              (listener? obj)
              (tickable? obj))
    (raise-argument-error 'add-event "evt?" obj))
  (hash-set! sync-threads id (thread (make-event-thunk obj))))

(define (remove-event o)
  (unless (lazy-ref? o) (raise-argument-error 'remove-event "lazy-ref?" o))
  (define id (lazy-ref-id o))
  (when (hash-has-key? sync-threads id)
    (define thread (hash-ref sync-threads id))
    (break-thread thread)
    (hash-remove! sync-threads id)))


  