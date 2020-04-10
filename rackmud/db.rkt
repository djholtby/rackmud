#lang racket/base

(require db racket/format gregor racket/match racket/list racket/set "lib-path.rkt" racket/rerequire uuid racket/string)

(provide make-rackmud-db-connection make-rackmud-db-pool rackmud-db-version database-log
         database-save-object database-new-object database-get-object database-find-indexed database-get-singleton database-make-singleton
         database-connected? database-disconnect database-get-cid! load-class file->cids set-database-connection!
         database-make-token database-verify-token database-get-all-tokens database-expire-token database-expire-all-tokens)

(define (make-rackmud-db-pool db-type db-port db-sock db-srv db-db db-user db-pass)
  (virtual-connection (connection-pool
                       (λ () (make-rackmud-db-connection db-type db-port db-sock db-srv db-db db-user db-pass)))))

(define (make-rackmud-db-connection db-type db-port db-sock db-srv db-db db-user db-pass)
  (case db-type
    [(mysql) (cond [(and db-sock (not db-srv) (not db-port))
                    (mysql-connect
                     #:user db-user
                     #:database db-db
                     #:socket db-sock
                     #:password db-pass)]
                   [(not db-sock)
                    (mysql-connect
                     #:user db-user
                     #:database db-db
                     #:server (if db-srv db-srv "localhost")
                     #:port (if db-port db-port 3306)
                     #:password db-pass)]
                   [else (raise-arguments-error 'database-setup "cannot use both TCP and local socket!" "db-sock" db-sock "db-srv" db-srv "db-port" db-port)])]
    [(postgres) (cond [(and db-sock (not db-srv) (not db-port))
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
                      [else (raise-arguments-error 'database-setup "cannot use both TCP and local socket!" "db-sock" db-sock "db-srv" db-srv "db-port" db-port)])]
    [(sqlite3) (sqlite3-connect
                #:database db-db)]
    [(odbc) (odbc-connect
             #:user db-user
             #:dsn db-db
             #:password db-pass)]
    [else (raise-argument-error 'make-rackmud-db-connection "dbtype?" db-type)]))

(define (rackmud-db-version conn)
  (with-handlers ([exn:fail:sql? (lambda (e) #f)])
    (query-value conn "select val from metadata where id='database-version';")))


(define _dbc_ #f)

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
  
(define (database-connected?)
  (connection? _dbc_))

(define (database-disconnect)
  (when (connection? _dbc_)
    (disconnect _dbc_)
    (set! _dbc_ #f)))


(define (set-database-connection! system pool)
  (unless (connection? pool) (raise-argument-error 'set-database-connection! "connection?" pool))
  (if (eq? system 'sqlite3) ;; TODO, does anything else not support timestamp???
      (set! database-date-supported #f)
      (set! database-date-supported #t))
  
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

(define (database-get-object id)
  (start-transaction _dbc_ #:isolation 'read-committed)
  (define obj (query-maybe-row _dbc_ object-load-stmt id))
  (define tags (and obj (query-rows _dbc_ load-tags-stmt id)))
  (define indexed-fields (and obj (map (λ (v)
                                         (cons
                                          (string->symbol (vector-ref v 0))
                                          (vector-ref v 1)))
                                       (query-rows _dbc_ load-fields-stmt id))))
  (commit-transaction _dbc_)
  ;loaded? cid created saved name tags indexed-fields/bytes
   ;    cid, created, saved, name, deleted, fields
  (if (and obj (or (eq? #f (vector-ref obj 4))
                   (zero? (vector-ref obj 4))))
      (values #t
              (vector-ref obj 0)
              (dbtime->moment (vector-ref obj 1))
              (dbtime->moment (vector-ref obj 2))
              (vector-ref obj 3)
              tags
              indexed-fields
              (vector-ref obj 5))
      (values #f #f #f #f #f #f #f #f)))

(define (database-find-indexed index value)
  (unless (database-connected?) (error "database not connected"))
  (query-rows _dbc_ search-index-stmt index value))


(define (database-save-object oid name indexed-fields saved fields tags)
  (unless (database-connected?) (error "database not connected"))
  (let loop ()
    (start-transaction _dbc_)
    (with-handlers ([(λ (e) #t) (λ (e)
                                  (rollback-transaction _dbc_)
                                  #f)])
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
              (in-hash tags)]
             [tag (in-set tags)])
        (query-exec _dbc_ save-tag-stmt oid category tag))
      ; That's all, folks
      (with-handlers ([exn:fail:sql?
                       (λ (e)
                         (match (exn:fail:sql-sqlstate e)
                           [#rx"^40...$" (loop)]
                           ['done (loop)]
                           [else (raise e)]))])
        (commit-transaction _dbc_)
        #t))))



(define (database-new-object cid created name)
  (define new-obj-result (query _dbc_ new-object-stmt cid (moment->dbtime created) name))
  (cond [(and (simple-result? new-obj-result) (assoc 'insert-id (simple-result-info new-obj-result)))
         (cdr (assoc 'insert-id (simple-result-info new-obj-result)))]
        [(and (rows-result? new-obj-result)
              (= 1 (length (rows-result-headers new-obj-result)))
              (= 1 (length (rows-result-rows new-obj-result))))
         (vector-ref (first (rows-result-rows new-obj-result)) 0)]
        [else (error 'new "unexpected database result (~v) when instantiating new object" new-obj-result)]))





(define (database-make-token oid #:expires [expires "9999-01-01T00:00:00Z"])
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
       oid))

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