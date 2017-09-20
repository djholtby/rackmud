#lang racket

(provide mud-object% define-mud-class* define-mud-class define-mud-struct )
(provide object-table lazy-ref lazy-ref? lazy-deref get-object database-save-object )
;lazy-ref-id set-lazy-ref-id!
(require "mudlib-util.rkt" (for-syntax "mudlib-util.rkt"))

(require racket/class)
(require db srfi/19 db/util/datetime)
(require racket/rerequire)
(require racket/fasl)
(require web-server/private/gzip)
(require racket/hash)



(define-struct lazy-ref (id) #:transparent #:mutable)

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


(define mud-object% (class object% 
                      (super-new)
                      (init-field [id (void)]
                                  [name "object"])
                      (field      [live? #t]
                                  [cid   #f]
                                  ;; tags : (HashTable Symbol (Setof Symbol))
                                  [tags  (make-hasheq)]
                                  )
                      
                      ; (save) for a mud object produces a hash that maps field identifier symbols to values
                      (define/public (save)
                        (make-hasheq `((name . ,name))))

                      (define/public (save-index)
                        (make-hasheq))
  
                      ; (load flds) initializes each field with the values in the given hash map
                      (define/public (load flds)
                        (set! name (hash-ref flds 'name void)))))

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
                     [name/cid (datum->syntax #'name (string->symbol (string-append (symbol->string (syntax->datum #'name)) "/cid")))]
                     )
         #'(begin
             (define name/cid (database-get-cid! 'name (relative-module-path (filepath))))
               
             (define name (class* super-expression (interface-expr ...)
                            (set-field! cid this name/cid)
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
                            def-or-exp ...))
             

             (provide name name/cid)
             )))]))


#|||||||||||||||||||||||||||||||||||||||||||||||

                DATABASE STUFF

||||||||||||||||||||||||||||||||||||||||||||||||#


(define object-table (make-hasheq empty))

;; (get-object id) produces the object with the ID id, or #<void> if none is currently loaded
;; get-object: Lazy-Ref -> (U (Instance Mud-Object%) Void)

(define (get-object id)
  (hash-ref object-table (lazy-ref-id id) void))

;; (lazy-deref) produces the object pointed to by lazy-ref (from the object table if possible, the database otherwise)
;;  produces #fif the object cannot be found in the otable or the database (i.e. the object has been deleted)

;; lazy-deref: Lazy-Ref -> (U (Instance Mud-Object%) #f)

(define (lazy-deref lr)
  (unless (lazy-ref? lr) (raise-argument-error 'lazy-deref "lazy-ref?" lr))
  (define id (lazy-ref-id lr))
  (define o  (get-object id))
  (if (void? o) (database-load id) o))

(define _db_save       void)
(define _db_load       void)
(define _db_conn       void)
(define _db_close      void)
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
    (set! object-load-stmt (prepare conn (format "SELECT class, created, saved, name, deleted, fields FROM objects WHERE oid = ~v"
                                                 (if is-postgres? "$1" "?"))))
    (set! class-load-stmt (prepare conn (format "SELECT classname, module FROM classes WHERE cid = ~v"
                                                (if is-postgres? "$1" "?"))))
    (set! get-classid-stmt (prepare conn (format "SELECT cid FROM classes WHERE classname = ~v AND module = ~v"
                                                 (if is-postgres? "$1" "?")
                                                 (if is-postgres? "$2" "?"))))

    (set! create-classid-stmt (prepare conn (format "INSERT INTO classes (classname,module) VALUES (~v , ~v)"
                                                    (if is-postgres? "$1" "?")
                                                    (if is-postgres? "$2" "?"))))
    
    (set! delete-fields-stmt (prepare conn (format "DELETE FROM indexed_fields WHERE object = ~v"
                                                   (if is-postgres? "$1" "?"))))
    (set! delete-tags-stmt (prepare conn (format "DELETE FROM tags WHERE object = ~v"
                                                 (if is-postgres? "$1" "?"))))

    (set! new-object-stmt (prepare conn (format "INSERT INTO objects (class, created, name, deleted) VALUES (~v, ~v, ~v, false) ~v"
                                                (if is-postgres? "$1" "?")
                                                (if is-postgres? "$2" "?")
                                                (if is-postgres? "$3" "?")
                                                (if is-postgres? "RETURNING oid" ""))))

    (set! save-object-stmt (prepare conn (format "UPDATE objects SET name=~v, saved=~v, fields=~v WHERE oid = ~v"
                                                 (if is-postgres? "$1" "?")
                                                 (if is-postgres? "$2" "?")
                                                 (if is-postgres? "$3" "?")
                                                 (if is-postgres? "$4" "?"))))

    (set! save-tag-stmt (prepare conn (format "INSERT INTO tags (object, category, tag) values (~v, ~v, ~v)"
                                              (if is-postgres? "$1" "?")
                                              (if is-postgres? "$2" "?")
                                              (if is-postgres? "$3" "?"))))

    (set! save-field-stmt (prepare conn (format "INSERT INTO indexed_fields (object, field, value) values (~v, ~v, ~v)"
                                                (if is-postgres? "$1" "?")
                                                (if is-postgres? "$2" "?")
                                                (if is-postgres? "$3" "?"))))

    (set! load-tags-stmt (prepare conn (format "SELECT category, tag FROM tags WHERE object = ~v"
                                               (if is-postgres? "$1" "?"))))

    (set! load-fields-stmt (prepare conn (format ("SELECT field, value FROM indexed_fields WHERE object = ~v"
                                                  (if is-postgres? "$1" "?")))))

    (set! search-tags-stmt (prepare conn (format ("SELECT object FROM tags WHERE category = ~v AND tag = ~v"
                                                  (if is-postgres? "$1" "?")
                                                  (if is-postgres? "$2" "?")))))

    (set! search-index-stmt (prepare conn (format ("SELECT object FROM indexed_fields WHERE field = ~v AND value = ~v"
                                                   (if is-postgres? "$1" "?")
                                                   (if is-postgres? "$2" "?")))))

                                              
    (set! get-singleton-stmt (prepare conn (format ("SELECT oid FROM singletons WHERE cid = ~v"
                                                    (if is-postgres? "$1" "?")))))

    (set! new-singleton-stmt (prepare conn (format ("INSERT INTO singletons (oid, cid) values (~v, ~v)"
                                                    (if is-postgres? "$1" "?")
                                                    (if is-postgres? "$2" "?")))))
                                                
                                      

    
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
    (define cid (query-maybe-value _dbc_ get-classid-stmt class-string module-string))
    (hash-set! cid-map (cons class-name module-name) cid)
    (unless cid
      (query-exec _dbc_ create-classid-stmt class-string module-string)
      (set! cid (query-maybe-value _dbc_ get-classid-stmt class-string module-string))
      (hash-set! cid-map (cons class-name module-name) cid)))
  (unless cid (error 'dataabse-get-cid! "could not find or create new classid"))
  cid)

    

(define (database-get-object id)
  (unless (database-connected?) (error "database not connected"))
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  (define tags (query-rows _dbc_ load-tags-stmt id))
  (define indexed-fields (map (λ (v)
                                (cons (string->symbol (vector-ref v 0))
                                      (read (open-input-string (bytes->string/utf-8 (vector-ref v 1))))))
                              (query-rows _dbc_ load-fields-stmt id)))
    
  (if obj (local [(define classinfo/v (query-row _dbc_ class-load-stmt (vector-ref obj 0)))
                  
                  (define fields (read (open-input-string (bytes->string/utf-8 (gunzip/bytes (vector-ref obj 2))))))
                  (define classname (string->symbol (vector-ref classinfo/v 0)))
                  (define classfile (string->path (vector-ref classinfo/v 1)))
                  (define changes (dynamic-rerequire classfile)) ;; TODO: Mark changed mudlib source files so old instances can be refreshed
                  (define class (dynamic-require classfile classname))
                  (define new-object (new class [id id]))
                  (define new-object/tags (get-field tags new-object))
                  ]

            (when (cons? changes) (void))
            (hash-union! fields indexed-fields)
            (send new-object load fields)
            (for ([row (in-list tags)])
              (local [(define category (vector-ref row 0))
                      (define tag      (string->symbol (vector-ref row 1)))]
                (if (hash-has-key? new-object/tags category)
                    (set-add! (hash-ref new-object/tags category) tag)
                    (hash-set! new-object/tags category (mutable-seteq tag)))))
                
            (send new-object on-load)
            new-object)
      (void)))
  
;(deserialize (read (open-input-string (bytes->string/utf-8 (_db_load id))))))
;(void))

(define (database-save-object o)
  (unless (database-connected?) (error "database not connected"))
  (define oid (get-field id o))
  (define name (get-field name o))
  (define fields (send o save))
  (define indexed-fields (send o save-index))
  (define saved (srfi-date->sql-timestamp (current-date)))

  (set-field! last-save o saved)
  
  (start-transaction _dbc_)
  (query-exec _dbc_ delete-fields-stmt oid)
  (query-exec _dbc_ delete-tags-stmt oid)
  (query-exec _dbc_ save-object-stmt name saved fields oid)
  (for ([(field value) (in-hash indexed-fields)])
    (query-exec _dbc_ save-field-stmt oid field value))
  (for* ([(category tags)
         (in-hash (get-field tags o))]
        [tag (in-set tags)])
    (query-exec _dbc_ save-tag-stmt oid category tag))
  (commit-transaction _dbc_))

#|
(define (get-singleton cid)
  (define oid (query-maybe-value _dbc_ get-singleton-stmt cid))
  (if oid
      (lazy-ref oid) ; already exists
      (local [(define o (new .......!
        
        
        )))
  |#    


(define-syntax (new/griftos stx)
  (syntax-case stx ()
    [(_ cls (id arg) ...)
     #'(local [(define o (new class (id arg) ...))]
         (database-new-object o))]))

(define (database-new-object o)
  (define cid (get-field cid o))
  (define name (get-field name o))
  (define created (get-field created o))
  ;"INSERT INTO objects (class, created, name, deleted) VALUES (~v, ~v, ~v, false)
  (define new-obj-result (query _dbc_ new-object-stmt cid created name))

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
  (lazy-ref o))
                
         
(define-mud-class foo mud-object% (super-new)
  (init-field/index [player-name "Unnamed"])
  (field [my-field (void)])
  )