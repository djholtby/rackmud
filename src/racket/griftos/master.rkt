#lang racket
(require racket/class)
(require json)
(require "objects.rkt")
(require "telnet.rkt")
(require racket/rerequire)
(provide master-object master<%> server-settings start-up)
(provide make-jit set-make-jit!)


;(provide ssl-settings ssl-settings? ssl-settings-private-key ssl-settings-certificate ssl-settings-port)
(provide db-settings  db-settings? db-settings-type db-settings-user db-settings-password db-settings-db
         db-settings-server db-settings-port db-settings-socket)
(provide griftos-config griftos-config? griftos-config-mudlib griftos-config-master-file griftos-config-master-class
         griftos-config-encodings griftos-config-database)

(define master-object #f)
(define server-settings #f)


#|||||||||||||||||||||||||||||||
Native Functions

These are hooks that the C backend will override with C functions

|||||||||||||||||||||||||||||||#

;; (make-jit proc) requests that proc be compiled  to native code
;; make-jit : Procedure -> Void
(define make-jit void)

(define (set-make-jit! proc)
  (unless (procedure? proc)
    (raise-argument-error 'set-make-jit! "procedure?" proc))
  (set! make-jit proc))


#|||||||||||||||||||||
Master Interface

The C backend will instantiate a singleton of this class.
The connection manager will send a telnet-object to it whenever a user connects.
|||||||||||||||||||||#


(define master<%>
  (interface ()
    [on-connect (->m exact-positive-integer? telnet?/c)]))

; DB-Settings is a (db-settings (anyof 'postgres 'sqlite 'odbc 'mysql) - type
;                               String                                 - user
;                               String                                 - password
;                               String                                 - db (for ODBC this is the DSN)
;                               (anyof String False)                   - server (false = localhost or use socket or N/A)
;                               (anyof Port False)                     - port (false = default or use socket or N/A)
;                               (anyof Path False))                    - socket (false = none or N/A)
(struct db-settings (type user password db server port socket))

(struct griftos-config (mudlib master-file master-class encodings database))


; (startup cfg) starts the mud racket side running using the provided config struct cfg
; Effects: * Opens database connection
;          * Loads or instantiates master object
;              - this starts the master's event thread, too

; start-up: Griftos-CFG


(define (start-up cfg)
  (let ([mudlib (griftos-config-mudlib cfg)]
        [master-file (griftos-config-master-file cfg)]
        [master-class (griftos-config-master-class cfg)]
        [db-type (db-settings-type (griftos-config-database cfg))]
        [db-port (db-settings-port (griftos-config-database cfg))]
        [db-sock (db-settings-socket (griftos-config-database cfg))]
        [db-srv  (db-settings-server (griftos-config-database cfg))]
        [db-db   (db-settings-db (griftos-config-database cfg))]
        [db-user (db-settings-user (griftos-config-database cfg))]
        [db-pass (db-settings-password (griftos-config-database cfg))])
    (set! server-settings cfg)
    (database-setup db-type db-port db-sock db-srv db-db db-user db-pass)
    (set-lib-path! mudlib)
    (dynamic-rerequire master-file)
    (define the-master% (dynamic-require master-file master-class))
    (unless (implementation?/mud the-master% master<%>)
      (raise-argument-error 'start-up "master-object%" master-class))
    (set! master-object  (get-singleton the-master%))
    master-object))

#|
The Master Object is responsible for saving any server-wide values that need saving.

It also has the methods for establishing, maintaining, and disconnecting telnet connections (Telnet-User structs)

Suggestion:  You'll almost definitely want a HashMap to map Telnet-Users to some kind of mud object
|#


