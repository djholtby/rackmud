#lang racket/base

(require db racket/format gregor racket/match racket/list racket/set "lib-path.rkt" racket/rerequire uuid racket/string sha)

(provide make-rackmud-db-connection make-rackmud-db-pool rackmud-db-version database-log
         database-save-object database-new-object database-get-object database-get-singleton database-make-singleton
         database-connected? database-disconnect database-get-cid! load-class file->cids set-database-connection!
         database-make-token database-token-refresh database-get-all-tokens database-expire-token database-expire-all-tokens
         database-create-field-index database-get-field-index-ids

         field-search-array field-search-array/and field-search-array/or
         field-search-op
         field-search-table/key+value
         field-search-table/key+value/list
         database-start-transaction!
         database-commit-transaction!
         )

(define (make-rackmud-db-pool db-port db-sock db-srv db-db db-user db-pass)
  (virtual-connection (connection-pool
                       (λ () (make-rackmud-db-connection db-port db-sock db-srv db-db db-user db-pass)))))

(define (make-rackmud-db-connection db-port db-sock db-srv db-db db-user db-pass)
  (cond [(and db-sock (not db-srv) (not db-port))
         (postgresql-connect
          #:user db-user
          #:database db-db
          #:socket db-sock
          #:password db-pass)]
        [(not db-sock)
         (postgresql-connect
          #:user db-user
          #:database db-db
          #:server (if db-srv db-srv "localhost")
          #:port (if db-port db-port 5432)
          #:password db-pass)]
        [else (raise-arguments-error 'database-setup
                                     "cannot use both TCP and local socket!" "db-sock" db-sock "db-srv" db-srv "db-port" db-port)]))

(define (rackmud-db-version conn)
  (with-handlers ([exn:fail:sql? (lambda (e) #f)])
    (query-value conn "select val from metadata where id='database-version';")))


(define _dbc_ #f)


(define (sql->moment s)
  (match-define
    (sql-timestamp year month day hour minute second nanosecond tz) s)
  (moment year month day hour minute second nanosecond #:tz (or tz (current-timezone))))

(define (moment->sql g)
  (sql-timestamp (->year g)
                 (->month g)
                 (->day g)
                 (->hours g)
                 (->minutes g)
                 (->seconds g)
                 (->nanoseconds g)
                 (->utc-offset g)))


  
(define object-load-stmt
  (virtual-statement
   "SELECT cid, o.created, o.saved, od.fields FROM objects o INNER JOIN object_fields od \
   ON (o.oid = $1) AND (od.oid = $1) AND (o.saved = od.saved)"))


(define class-load-stmt
  (virtual-statement
   "SELECT classname, module FROM classes WHERE cid = $1"))

                              
(define get-classid-stmt
  (virtual-statement
   "SELECT get_cid($1, $2) as cid"))

(define select-field-ids-statement
  (virtual-statement
   "SELECT get_field_index($1, field_name) as idx FROM (SELECT unnest($2::text[]) as field_name) AS t"))


(define new-object-stmt
  (virtual-statement
   "INSERT INTO objects (cid) VALUES ($1) RETURNING oid, created"))

(define save-object-stmt
  (virtual-statement
   (string-append
    "INSERT INTO object_fields (oid, fields) values ($1, $2)"
    "ON CONFLICT (oid, saved) DO UPDATE SET fields = EXCLUDED.fields RETURNING saved")))
;"UPDATE objects SET name=$1, saved=$2, fields=$3 WHERE oid = $4"))

(define search-tags-stmt
  (virtual-statement
   (string-append "SELECT oid from object_fields WHERE fields->'tags'->$1->'value' @> $2 AND\n"
                  "     EXISTS (SElECT 1 from objects where objects.saved = object_fields.saved)")))

(define search-field-stmt
  (virtual-statement
   "SELECT oid FROM object_fields WHERE fields->$1 = $2 AND EXISTS (SELECT 1 FROM objects WHERE objects.saved = object_fields.saved)"))

(define search-field-stmt/array
  (virtual-statement
   "SELECT oid FROM object_fields WHERE fields->$1 @> $2::jsonb AND EXISTS (SELECT 1 FROM objects WHERE objects.saved = object_fields.saved)"))

(define get-singleton-stmt
  (virtual-statement
   "SELECT oid FROM singletons WHERE cid = $1"))

(define new-singleton-stmt
  (virtual-statement
   "INSERT INTO singletons (oid, cid) values ($1, $2)"))
                                                
(define log-stmt
  (virtual-statement
   "INSERT INTO logfile (level, module, description, backtrace) values ($1, $2, $3, $4)"))

(define new-token-stmt
  (virtual-statement
   "INSERT INTO auth_testing.auth (oid, expires, sig) values ($1, (now() + $2 * interval '1 second'), $3) RETURNING seq"))

(define get-id-for-token-stmt
  (virtual-statement
   (string-append "SELECT oid,((expires IS NOT NULL) and (expires <= now())) AS expired , EXTRACT(epoch FROM (expires - issued))::int AS duration "
                  "FROM auth_testing.auth WHERE seq = $1 AND sig = $2" )))

(define tokens-for-oid-stmt
  (virtual-statement
   "SELECT seq FROM auth where oid = $1"))

(define expire-token-stmt
  (virtual-statement "DELETE FROM auth WHERE seq = $1"))

(define expire-all-tokens-stmt
  (virtual-statement
   "DELETE FROM auth WHERE oid = $1"))

(define module-to-cid-query
  (virtual-statement
   "SELECT cid from classes where module = $1"))

(define (database-connected?)
  (connection? _dbc_))

(define (database-disconnect)
  (when (connection? _dbc_)
    (disconnect _dbc_)
    (set! _dbc_ #f)))


(define (set-database-connection! pool)
  (unless (connection? pool) (raise-argument-error 'set-database-connection! "connection?" pool))
  (when _dbc_ (disconnect _dbc_))
  (set! _dbc_ pool))


(define (log-level->int ll)
  (or (index-of '(none fatal error warning info debug) ll eq?) 0))

#|
  (when (thread? logger-thread)
    (kill-thread logger-thread))

  (set! logger-thread
        (thread
         (λ ()
           (let loop ()
             (match (sync rackmud-log-rec)
               [(vector level msg data topic)
                (when (string? msg) (database-log level (or topic "racket") msg (backtrace data)))])
             (loop)))))|#
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

(define (database-start-transaction!)
  (start-transaction _dbc_))

(define (database-commit-transaction!)
  (commit-transaction _dbc_))


(define cid-map (make-hash))

#|! 
(database-get-cid! class-name module-name) retrieves the classid for the named class variable defined in the named module
  if the database does not contain this key, a new row is inserted

database-get-cid! : Symbol Path -> Nat
|#
(define (database-get-cid! class-name module-name)
  (when (not (database-connected?)) (error 'database-get-cid "database not connected"))
  (define cid (hash-ref cid-map (cons class-name module-name) #f))
  (unless cid
    (define class-string (symbol->string class-name))
    (define module-string (path->string module-name))
    (set! cid (query-value _dbc_ get-classid-stmt module-string class-string))
    (hash-set! cid-map (cons class-name module-name) cid))
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

(define (simplify-name s)
  (list->string (reverse
                 (foldl (λ (c acc)
                          (cond [(char-alphabetic? c) (cons c acc)]
                                [(char-numeric? c) (cons c acc)]
                                [(eqv? c #\space) (cons #\_ acc)]
                                [else acc]))
                        empty
                        (string->list (if (string? s) s (symbol->string s)))))))



(define field-search-statement/?
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 ? $2::jsonb AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))


(define field-search-statement/?&
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 ?& $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/?\|
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 ?| $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/@>
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 @> $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/=
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 = $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/!=
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 != $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/<
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 < $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/<=
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 <= $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/>
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 > $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/>=
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>$1 >= $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/~
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 ~ $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/~*
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 ~* $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))


(define field-search-statement/!~
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 !~ $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))


(define field-search-statement/!~*
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 !~* $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))

(define field-search-statement/like
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 LIKE $2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))


(define field-search-statement/not-like
  (virtual-statement
   (string-append "SELECT oid FROM object_fields WHERE fields#>>$1 NOT LIKE$2 AND EXISTS (SELECT 1 FROM objects WHERE\n"
                  "     objects.oid = object_fields.oid AND objects.saved = object_fields.saved)")))



(define (valid-db-operator? v)
  (memq v '(= != < > <= >= ~ !~ ~* !~*)))


;; (listof Str) Sym JSExpr #t) -> (listof OID)
;; (listof Str) JSExpr JSExpr #f) -> (listof OID)
(define (field-search-table/key+value full-json-path key value #:symbol-table? [json-table? #t])
  (if json-table?
      (query-list _dbc_ field-search-statement/@> full-json-path (hasheq key value))
      (query-list _dbc_ field-search-statement/@> full-json-path (hasheq 'key key 'value value))))

(define (field-search-table/key+value/list full-json-path keys values #:symbol-table? [json-table? #t])
  (if json-table?
      (query-list _dbc_ field-search-statement/@> full-json-path (make-hasheq (map cons keys values)))
      (query-list _dbc_ field-search-statement/@> full-json-path (map (λ (k v)
                                                                        (hasheq 'key k 'value v))))))

;; value: string

(define (field-search-array full-json-path value)
  (eprintf "~a\n" full-json-path)
  (if (string? value)
      (query-list _dbc_ field-search-statement/? full-json-path value)
      (query-list _dbc_ field-search-statement/@> full-json-path value)))

;; values (listof string)

(define (field-search-array/and full-json-path values)
  (if (andmap string? values)
      (query-list _dbc_ field-search-statement/?& full-json-path values)
      (query-list _dbc_ field-search-statement/@> full-json-path values)))

;; values (listof string)

(define (field-search-array/or full-json-path values)
  (if (andmap string? values)
      (query-list _dbc_ field-search-statement/?\| full-json-path values)
      (error "field search OR mode requires string keys")))

(define (field-search-op full-json-path op value)
  (query-list _dbc_
              (case op
                [(=) field-search-statement/=]
                [(!=) field-search-statement/!=]
                [(<) field-search-statement/<]
                [(<=) field-search-statement/<=]
                [(>) field-search-statement/>]
                [(>=) field-search-statement/>=]
                [(like) field-search-statement/like]
                [(not-like) field-search-statement/not-like]
                [(~) field-search-statement/~]
                [(!~) field-search-statement/!~]
                [(~*) field-search-statement/~*]
                [(!~*) field-search-statement/!~*]
                [else (raise-argument-error 'field-search-table/op "valid-db-operator?" op)])
              full-json-path
              value))

(define (database-create-field-index full-field-name [type 'simple] [depth 0])
  (let ([index-name (simplify-name full-field-name)]
        [search-path (string-join (map (λ (s) (format "'~a'" s)) (cons full-field-name (make-list depth "value"))) ", ")])
    (case type
      [(simple number string boolean char bytes moment object)
       (query-exec
        _dbc_
        (format "CREATE INDEX IF NOT EXISTS object_field_~a ON object_fields USING BTREE ((fields#>array[~a])) WHERE\
                 ((fields#>array[~a])) IS NOT NULL"
                index-name search-path search-path))]
      [(list vector symbol-table set hash json)
       (query-exec
        _dbc_
        (format "CREATE INDEX IF NOT EXISTS object_field_~a ON object_fields USING GIN ((fields#>array[~a])) WHERE\
                 ((fields#>array[~a])) IS NOT NULL"
                index-name search-path search-path))]
      [else (raise-argument-error 'database-create-field-index "(or/c 'simple 'json)" type)])))


(define (guard-field-name field-name)
  (list->string (filter (λ (c)
                          (or (char-alphabetic? c)
                              (char-numeric? c)
                              (eqv? c #\-)))
                        (string->list field-name))))

(define (database-get-field-index-ids cid field-symbols)
  (map (λ (name id)
         (string->symbol (format "~a{~a}" (guard-field-name (symbol->string name)) id)))
       field-symbols
       (query-list _dbc_ select-field-ids-statement cid (map symbol->string field-symbols))))

;; database-get-object: nat -> (values nat? moment? moment? jsexpr?)

(define (database-get-object id)
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  
  ;    cid, created, saved, fields
  (if obj
      (values 
       (vector-ref obj 0)
       (sql->moment (vector-ref obj 1))
       (sql->moment (vector-ref obj 2))
       (vector-ref obj 3))
              
      (values #f #f #f #f)))

(define (database-save-object oid fields)
  (unless (database-connected?) (error "database not connected"))
  (sql->moment (query-value _dbc_ save-object-stmt oid fields)))

(define (database-new-object cid)
  (let ([response (query-row _dbc_ new-object-stmt cid)])
    (values (vector-ref response 0)
            (sql->moment (vector-ref response 1)))))

;; (database-make-token oid [duration]) generates a new token for object id OID
;;  that expires after duration seconds (or never expires if duration is #f)
;;  and returns that token
;; database-make-token: OID (or #f Nat) -> Token

(define (database-make-token oid [duration #f])
  (let* ([sig (uuid-string)]
         [sig/hash (sha384 (string->bytes/utf-8 sig))]
         [seq (query-value _dbc_ new-token-stmt oid (false->sql-null duration) sig/hash)])
    (string-append seq "&" sig)))
  
;; parse-sig-token: Str -> (or #f UUID) (or #f UUID)

(define (parse-sig-token maybe-token)
  (if maybe-token
      (let [(strings (string-split maybe-token "&"))]
        (if (and (cons? strings) (cons? (cdr strings)) (null? (cddr strings)) ; 2 element list
                 (uuid-string? (car strings)) (uuid-string? (cadr strings)))  ; both elements are UUID strings
            (values (car strings) (cadr strings))
            (and (log-warning "received invalid refresh token value: ~a" maybe-token) (values #f #f))))
      (values #f #f)))       ; cookie was not found


;; database-token-refresh: Str -> (or #f OID) (or #f duration) (or #f Token)

(define (database-token-refresh old-token)
  (call-with-transaction
   _dbc_
   (λ ()
     (let-values ([(seq sig) (parse-sig-token old-token)])
       (let* ([token-info (and seq (query-maybe-row _dbc_ get-id-for-token-stmt seq (sha384 (string->bytes/utf-8 sig))))]
           [id (and token-info (vector-ref token-info 0))]
           [expired? (and token-info (vector-ref token-info 1))])
      (when token-info (query-exec _dbc_ expire-token-stmt seq))
      (if (and id (not expired?))
          (values id
                  (sql-null->false (vector-ref token-info 2))
                  (database-make-token id (sql-null->false (vector-ref token-info 2))))
          (values id #f #f)))))))




(define (database-get-all-tokens oid)
  (query-list _dbc_ tokens-for-oid-stmt oid))

(define (database-expire-token seq)
  (query-exec _dbc_ expire-token-stmt seq))

(define (database-expire-all-tokens oid)
  (query-exec _dbc_ expire-all-tokens-stmt oid))

(define (database-get-singleton cid)
  (query-maybe-value _dbc_ get-singleton-stmt cid))

(define (database-make-singleton oid cid)
  (query-exec _dbc_ new-singleton-stmt oid cid))