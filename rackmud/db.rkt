#lang racket/base

(require db)

(provide make-rackmud-db-connection rackmud-db-version)

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
