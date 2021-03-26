#lang racket/base

(require racket/string racket/match racket/format racket/list racket/class racket/tcp racket/port
         charset racket/undefined db/base racket/pretty racket/runtime-path racket/file racket/system
         (for-syntax racket/base)
         "../menu.rkt" "../db.rkt" "config.rkt")

(provide rackmud-configure)

;(require racket/port racket/system)
(define terminal-width (min 120
                            (or (string->number (string-trim (with-output-to-string
                                                               (λ () (system "tput cols")))))
                                    80)))

;(define terminal-width 80)
(define-runtime-path HERE ".")

(define (touch filename)
  (unless (file-exists? filename)
    (with-output-to-file filename void)))

(define (pathstring->filename path)
  (define-values [root filename must-be-dir?]
    (if (path-string? path) (split-path path) (values #f #f #f)))
  (cond [(not root) 'bad-path]
        [(and (path? root) (not (directory-exists? root))) 'bad-dir]
        [must-be-dir? 'invalid-file-name]
        [else (path->string filename)]))

(define (file->statements file)
  (map (lambda (s) (string-append s ";"))
       (filter (lambda (s) (not (string=? s "")))
               (map string-trim
                    (string-split (file->string file) ";")))))

;(define-runtime-paths [postgres-path mysql-path sqlite3-path]
;  (values "data/postgre.sql" "data/mysql.sql" "data/sqlite.sql"))

(define-runtime-path sql-script-path "data/rackmud_create.sql")
(define database-create-statements (file->statements sql-script-path))

(define (cfg->conn cfg)
  (define db-type (hash-ref cfg 'database:type #f))
  (define db-port (hash-ref cfg 'database:port #f))
  (define db-sock (hash-ref cfg 'database:socket #f))
  (define db-srv (hash-ref cfg 'database:server #f))
  (define db-db (hash-ref cfg 'database:database #f))
  (define db-user (hash-ref cfg 'database:username #f))
  (define db-pass (hash-ref cfg 'database:password #f))
  (with-handlers ([exn? (lambda (e) #f)])
    (make-rackmud-db-connection db-type db-port db-sock db-srv db-db db-user db-pass)))

;; (verify-db-settings cfg) produces #f if cfg contains invalid database settings,
;;   'no-schema if the connection can be established but it does not contain the rackmud tables,
;;   or the version of the rackmud tables present.
(define (verify-db-settings cfg)
  (define conn (cfg->conn cfg))
  (and conn (begin0 (or (rackmud-db-version conn) 'no-schema) (disconnect conn))))
        
(define (load-db-schema cfg)
  (define type (hash-ref cfg 'database:type #f))
  (define conn (cfg->conn cfg))
  (for ([statement (in-list database-create-statements)])
    (query-exec conn statement))
  (disconnect conn))
    


(define (save-config cfg am issues)
  (define filename (hash-ref cfg 'filename))

  (unless (string=? issues "")
    (error 'save-config "Cannot save a config with invalid settings"))
  (define out (open-output-file filename #:exists 'truncate/replace))
  (define new-cfg
    (reverse (filter cdr (map (lambda (sym) (cons sym (hash-ref cfg sym #f))) (hash-values am)))))
  (parameterize ([pretty-print-columns 160])
    (pretty-print new-cfg out 1))
  (close-output-port out))

(define (box++ b)
  (let ([old (unbox b)])
    (set-box! b (add1 old))
    old))

(define (insert-at lst indx value)
  ;  (eprintf "~a ~a ~a\n" lst indx value)
  (cond [(zero? indx) (cons value lst)]
        [(empty? lst) (error 'insert-at "invalid index")]
        [else (cons (first lst)
                    (insert-at (rest lst) (sub1 indx) value))]))

(define (string->error-string s)
  (format "\e[31;1m[~a]\e[0m" s))

(define (string-length/escape s)
  (let loop ([c 0]
             [lst (string->list s)]
             [in-esc? #f])
    (cond [(empty? lst) c]
          [(and in-esc? (char-alphabetic? (first lst)))
           (loop c (rest lst) #f)]
          [in-esc? (loop c (rest lst) #t)]
          [(char=? (first lst) #\u001B) (loop c (rest lst) #t)]
          [else (loop (add1 c) (rest lst) #f)])))
           

(define (pad s width)
  (let ([amnt (max 0 (- width (string-length/escape s)))])
    (string-append s (make-string amnt #\space))))

(define (padl s width)
  (let ([amnt (max 0 (- width (string-length/escape s)))])
    (string-append (make-string amnt #\space) s)))




(define (identifier-string? v)
  (and (string? v)
       (zero? (count (lambda (c)
                       (or (char-whitespace? c)
                           (memq c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\))))
                     (string->list v)))))

(define (none-fmt v)
  (if v (~v v) "(none)"))

(define (default-fmt v)
  (if v (~v v) "(default)"))

(define (password-fmt v)
  (if v "*****" "(none)"))

(define (if-false default)
  (lambda (v) (if v (~v v) default)))









  
(define new-install-menu%
  (class terminal-menu%
    
    (init configuration)
    (define cfg configuration)
    (define database-version #f)
    (define action-map (make-hasheqv))
    (define error-table (make-hasheq))
    (define issues "")

    
    (inherit transmit)

    (super-new)
    
    (define (config-issues cfg)
      (define out (open-output-string))
      (hash-clear! error-table)
      (define fn (pathstring->filename (hash-ref cfg 'filename default-config-name)))
      (case fn
        [(bad-path) (displayln "* config path is invalid" out)
                    (hash-set! error-table 'filename "Bad Path")]
        [(bad-dir) (displayln "* config path leads to invalid directory" out)
                   (hash-set! error-table 'filename "Invalid Directory")]
        [(invalid-file-name) (displayln "* config path is does not lead to a file" out)
                             (hash-set! error-table 'filename "Invalid File Name")]
        [else
         (unless (string=? fn default-config-name)
           (hash-set! error-table 'filename '("\e[33;1m[Not Default]\e[0m")))])
  
      (unless (string? (hash-ref cfg 'database:username #f))
        (displayln "* database user not set" out)
        (hash-set! error-table 'database:username "Required"))
      (unless (string? (hash-ref cfg 'database:database #f))
        (displayln "* database not set" out)
        (hash-set! error-table 'database:database "Required"))
      (when (and (or (hash-ref cfg 'database:server #f)
                     (hash-ref cfg 'database:port #f))
                 (hash-ref cfg 'database:socket #f))
        (displayln "* database set to both TCP and local socket" out)
        (hash-set! error-table 'database:socket "TCP / Socket Mutually Exclusive")
        (when (hash-ref cfg 'database:server #f)
          (hash-set! error-table 'database:server "TCP / Socket Mutually Exclusive"))
        (when (hash-ref cfg 'database:port #f)
          (hash-set! error-table 'database:port "TCP / Socket Mutually Exclusive")))
  
      (when (and (hash-ref cfg 'database:port #f)
                 (not (port-number? (hash-ref cfg 'database:port #f))))
        (displayln "* database port is not a valid port number" out)
        (hash-set! error-table 'database:port "Invalid Port"))
  
      (match (hash-ref cfg 'mudlib-collect #f)
        [#f (displayln "* mudlib collection not set" out)
            (hash-set! error-table 'mudlib-collect "Required")]
        [(? path-string?) #t]
        [else (displayln "* mudlib collection not valid" out)
              (hash-set! error-table 'mudlib-collect "Invalid")])
      (match (hash-ref cfg 'master-module #f)
        [#f (displayln "* master module not set" out)
            (hash-set! error-table 'master-module "Required")]
        [(? path-string?) #t]
        [else (displayln "* master module not valid" out)
              (hash-set! error-table 'master-module "Invalid")])

      (match (hash-ref cfg 'master-classname #f)
        [#f (displayln "* master classname not set" out)
            (hash-set! error-table 'master-classname "Required")]
        [(? identifier-string?) #t]
        [else (displayln "* master classname not a valid identifier" out)
              (hash-set! error-table 'master-classname "Invalid")])
  
      (match (hash-ref cfg 'mudlib-path #f)
        [#f #t]
        [(and (? path-string?) (? directory-exists?)) #t]
        [(? path-string?)
         (displayln "* extra collection directory not found" out)
         (hash-set! error-table 'mudlib-path "Not Found")]
        [else (displayln "* extra collection directory not a valid path" out)
              (hash-set! error-table 'mudlib-path "Invalid")])
  
      (define cert (hash-ref cfg 'ssl:certificate #f))
      (define pkey (hash-ref cfg 'ssl:private-key #f))
      (define ssl? (and cert pkey))
  
      (when cert
        (cond [(not (path-string? cert))
               (displayln "* SSL certificate set but is not a valid path" out)
               (hash-set! error-table 'ssl:certificate "Invalid")
               (set! ssl? #f)]
              [(not (file-exists? cert))
               (displayln "* SSL certificate set but file not found" out)
               (hash-set! error-table 'ssl:certificate "Not Found")
               (set! ssl? #f)]))

      (when pkey
        (cond [(not (path-string? pkey))
               (displayln "* SSL private key set but is not a valid path" out)
               (hash-set! error-table 'ssl:private-key "Invalid")
               (set! ssl? #f)]
              [(not (file-exists? pkey))
               (displayln "* SSL certificate set but file not found" out)
               (hash-set! error-table 'ssl:private-key "Not Found")
               (set! ssl? #f)]))

      (when (and cert (not pkey))
        (displayln "* SSL certificate configured, but private key is not" out)
        (hash-set! error-table 'ssl:private-key "Not Set"))
      (when (and pkey (not cert))
        (displayln "* SSL private key configured, but certificate is not" out)
        (hash-set! error-table 'ssl:certificate "Not Set"))

      (define telnet-port (hash-ref cfg 'telnet:port #f))
      (define stelnet-port (hash-ref cfg 'telnet:ssl-port #f))
  
      (when (and telnet-port
                 (not (port-number? telnet-port)))
        (displayln "* telnet port set to an invalid port number" out)
        (hash-set! error-table 'telnet:port "Invalid")
        (set! telnet-port #f))

      (when (and stelnet-port
                 (not (port-number? stelnet-port)))
        (displayln "* secure telnet port set to an invalid port number" out)
        (hash-set! error-table 'telnet:ssl-port "Invalid")
        (set! stelnet-port #f))
      (when (and stelnet-port
                 (not ssl?))
        (displayln "* secure telnet port set, but SSL not properly configured" out)
        (hash-set! error-table 'telnet:ssl-port "SSL Disabled")
        (set! stelnet-port #f))
      (define telnet? (or telnet-port stelnet-port))

      (when telnet?
        (define encs (hash-ref cfg 'telnet:encodings '()))
        (unless (member "ASCII" encs)
          (displayln "* telnet encodings must at least contain ASCII" out)
          (hash-set! error-table 'telnet:encodings "Invalid")))

      (define http-port (hash-ref cfg 'webserver:port #f))
      (define https-port (hash-ref cfg 'webserver:ssl-port #f))
      (when (and https-port (not ssl?))
        (displayln "* HTTPS port set, but SSL not properly configured" out)
        (hash-set! error-table 'webserver:ssl-port "SSL Disabled")
        (set! https-port #f))
      (when (and http-port (not (port-number? http-port)))
        (displayln "* HTTP port set, but not to a valid port number" out)
        (hash-set! error-table 'webserver:port "Invalid")
        (set! http-port #f))
      (when (and https-port (not (port-number? https-port)))
        (displayln "* HTTPS port set, but not to a valid port number" out)
        (hash-set! error-table 'webserver:ssl-port "Invalid")
        (set! https-port #f))
      (define webserver? (or http-port https-port))

      (define websock? #f)
      (when webserver?
        (define www-path (hash-ref cfg 'webserver:www-path #f))
        (define servlet-url (hash-ref cfg 'webserver:servlet-url #f))
        (define websock-url (hash-ref cfg 'webserver:websock-url #f))
        (define websock-client-url (hash-ref cfg 'webserver:websock-client-url #f))
        (unless (and (path-string? www-path)
                     (directory-exists? www-path))
          (hash-set! error-table 'webserver:www-path (if (path-string? www-path) "Not Found" "Invalid"))
          (displayln "* WWW root is not valid" out))
        (unless (path-string? servlet-url)
          (displayln "* Servlet URL is not valid" out)
          (hash-set! error-table 'webserver:servlet-url "Invalid"))
        (unless (path-string? websock-url)
          (displayln "* Websock URL is not valid" out)
          (hash-set! error-table 'webserver:websock-url "Invalid"))
        (when websock-client-url
          (cond [(path-string? websock-client-url) (set! websock? #t)]
                [else (displayln "* Websock Client URL is not valid" out)
                      (hash-set! error-table 'webserver:websock-client-url "Invalid")])))

      (unless (exact-nonnegative-integer? (hash-ref cfg 'thread-count #f))
        (displayln "* thread count is not a positive integer" out)
        (hash-set! error-table 'thread-count "Invalid"))
  
      (when (not (or telnet? websock?))
        (displayln "* Neither telnet nor websock enabled, server is not usable" out)
        (hash-set! error-table 'general "Neither Telnet Nor Websock Enabled"))
      (set! issues (get-output-string out)))
    
    ;  (config-option out 'interactive item-box )
    (define (config-option out name counter #:format [~~ ~v])
      (define split (string-split (symbol->string name) ":"))
      (define int (box++ counter))
      (define v (hash-ref cfg name #f))
      (define hint (hash-ref error-table name #f))
      (define hint-text
        (cond [(not hint) ""]
              [(string? hint) (string->error-string hint)]
              [(cons? hint) (first hint)]
              [else (error 'config-option "Bad hint for ~a (~a)" name hint)]))
      (hash-set! action-map int name)
      (define line (format "~a) ~a = ~a" (padl (number->string int) 2)
                           (pad (if (or (empty? split) (empty? (rest split))) (symbol->string name) (second split)) 20)
                           (~~ v)))
      (display line out)
      (cond [(string=? "" hint-text) (display "\n" out)]
            [(< (+ (string-length line) (string-length hint-text)) terminal-width) ; not an off by 1, min 1 space before hint
             (displayln (padl (format "~a" hint-text) (- terminal-width (string-length line))) out)]
            [else 
             (display "\n                           ^" out)
             (displayln hint-text out)]))

    (define (display-database-settings out counter )
      (config-option out 'database:database counter  #:format none-fmt)
      (config-option out 'database:username counter  #:format none-fmt)
      (config-option out 'database:password counter  #:format password-fmt)
      (config-option out 'database:server counter  #:format default-fmt)
      (config-option out 'database:port counter  #:format default-fmt)
      (config-option out 'database:socket counter  #:format none-fmt))

       
    (define (display-web-settings out counter )
      (define http (hash-ref cfg 'webserver:port #f))
      (define https (hash-ref cfg 'webserver:ssl-port #f))
      (config-option out 'webserver:port counter  #:format (if-false "(disabled)"))
      (config-option out 'webserver:ssl-port counter  #:format (if-false "(disabled)"))
      (when (or http https)
        (config-option out 'webserver:www-path counter )
        (config-option out 'webserver:servlet-url counter )
        (config-option out 'webserver:websock-url counter )
        (config-option out 'webserver:websock-client-url counter )))
    
      
    (define (config->string)
      (define out (open-output-string))
      (define counter (box 1))
      (hash-clear! action-map)
      (define filename (hash-ref cfg 'filename))
      (config-issues cfg)
      (fprintf out "===Configuration File Settings===\n")

      (config-option out 'filename counter )
  
      (fprintf out "\n===Database Settings===\n")
      (display-database-settings out counter )
      
      (fprintf out "\n===Mudlib Settings===\n")
      (config-option out 'mudlib-collect counter )
      (config-option out 'master-module counter )
      (config-option out 'master-classname counter )
      (config-option out 'mudlib-path counter  #:format none-fmt)
      

      (fprintf out "\n===SSL Settings===\n")
      (config-option out 'ssl:certificate counter  #:format none-fmt)
      (config-option out 'ssl:private-key counter  #:format none-fmt)
      
      (fprintf out "\n===Telnet Settings===\n")
      (config-option out 'telnet:port counter  #:format (if-false "(disabled)"))
      (config-option out 'telnet:ssl-port counter  #:format (if-false "(disabled)"))
      (config-option out 'telnet:encodings counter  #:format (if-false "(default)"))
      
      (fprintf out "\n===Web Settings===\n")
      (display-web-settings out counter )
      
      (fprintf out "\n===Misc Settings===\n")
      (config-option out 'interactive counter )
      (config-option out 'thread-count counter )
      (when (hash-ref error-table 'general #f)
        (displayln (padl (string->error-string (hash-ref error-table 'general)) terminal-width) out))
      
      ;  (display-config-issues cfg out)
      
      (get-output-string out))


  
    
    (define-menu-fsm (msg)
      [start
       [(transmit "Configuration file not found.\nWould you like to create one now?\n") #f]
       [(match (string-foldcase msg)
          [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") 'confirm-settings]
          [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$")
           (transmit eof)]
          [else 'start])]]
      [confirm-settings
       [(transmit (config->string)
                  (format "\nEnter a setting to change, or press [Enter] to write your settings to '~a'\n" (hash-ref cfg 'filename))) #f]
       [(let* ([selection (string->number (string-trim msg))]
               [change-settings (hash-ref action-map selection #f)])
          (cond [change-settings change-settings]
                [(and (string=? (string-trim msg) "")
                      (string=? issues ""))
                 'validate]
                [(string=? (string-trim msg) "")
                 (transmit "Cannot save configuration due to the following issue(s)\n\n\e[31;1m" issues "\e[0m")
                 #f]
                [else (transmit "Invalid Selection") #f]))]]
      [validate
       [(define db-version (verify-db-settings cfg))
        (case db-version
          [(#f) (transmit "\e[31;1mdatabase connection failure, please confirm your database configuration\e[0m\n") 'confirm-settings]
          [(no-schema) 'configure-database]
          [else (set! database-version db-version) 'upgrade-db])]
       [(error 'validate "not implemented")]]
      [upgrade-db
       [(transmit (format "Connected to database (version=~a)\n" database-version)) 'confirm-mudlib]
       [(error 'upgrade-db "not implemented")]] ; TODO - upgrade scripts 
      [configure-database
       [(transmit "Connected to database, but no rackmud schema was found.  Create tables now? (Y / N)\n"
                  "Note: rackmud requires uuid-ossp.  A postgres superuser must install this module first.\n") #f]
       [(match (string-foldcase msg)
          [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") (load-db-schema cfg) 'confirm-mudlib]
          [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$") 'save-verification]
          [else #f])]]
      [confirm-mudlib
       ;; TODO
       ['save-verification]
       [#f]]
      [save-verification
       [(transmit "Save settings and continue?\n") #f]
       [(match (string-foldcase msg)
          [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") (save-config) (transmit #t)]
          [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$") 'confirm-settings]
          [else 'save-verification])]]
      [database:database
       [(transmit "Enter database name, or press [Enter] to leave unchanged\n")]
       [
        (define tmsg (string-trim msg))
        (if (string=? tmsg "")
            'confirm-settings
            (begin
              (hash-set! cfg 'database:database tmsg)
              'confirm-settings))]]
      [database:username
       [(transmit "Enter database username, or press [Enter] to leave unchanged\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [else (hash-set! cfg 'database:username tmsg) 'confirm-settings])]]
      [database:password
       [(transmit "Enter database password, or press [Enter] if database login")
        (transmit " does not require a password\n")
        (transmit 'no-echo)
        #f]
       [
        (transmit 'echo)
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") (hash-set! cfg 'database:password #f) 'confirm-settings]
              [else (hash-set! cfg 'database:password tmsg) 'confirm-settings])]]
      [database:server
       [(transmit "Enter the TCP address of the database server.  Use #f if using a system socket (or to use localhost)\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'database:server #f) 'confirm-settings]
              [else (hash-set! cfg 'database:server tmsg) 'confirm-settings])]]
      [database:port
       [(transmit "Enter the TCP port of the database server.  Use #f if using a system socket (or to use the default port)\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'database:port #f) 'confirm-settings]
              [(port-number? (string->number tmsg)) (hash-set! cfg 'database:port (string->number tmsg)) 'confirm-settings]
              [else (transmit "Enter a valid port number, or #f\n") #f])]]
      [database:socket
       [(transmit "Enter system socket for postgres connection, 'guess to attempt to guess the socket, or #f if not using a system socket\n")]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'database:socket #f) 'confirm-settings]
              [(string=? tmsg "'guess")
               (hash-set! cfg 'database:socket 'guess) 'confirm-settings]
              [(path-string? tmsg) (hash-set! cfg 'database:socket tmsg) 'confirm-settings]
              [else (transmit "Enter a valid socket path, 'guess, or #f\n")])]]
      [mudlib-collect
       [(transmit "Enter the collection name for the mudlib\n\e[33m[The default is \"mudlib\" and that's not a bad choice]\e[0m\n") #f]
       [
        (define tmsg (string-trim msg))
        (define old (hash-ref cfg 'mudlib-collect #f))
        (cond [(and (string=? tmsg "") (path-string? old)) 'confirm-settings]
              [(string=? tmsg "") (transmit "mudlib collection is required\n") #f]
              [(path-string? tmsg) (hash-set! cfg 'mudlib-collect tmsg) 'confirm-settings]
              [else (transmit "invalid module path\n") #f])]]
      [master-module
       [(transmit "Enter the module that defines the master object class\n\e[33m[This must be a module path relative to the mudlib collect]\e[0m\n") #f]
       [
        (define tmsg (string-trim msg))
        (define old (hash-ref cfg 'master-module #f))
        (cond [(and (string=? tmsg "") (path-string? old)) 'confirm-settings]
              [(string=? tmsg "") (transmit "master module is required\n") #f]
              [(path-string? tmsg) (hash-set! cfg 'master-module tmsg) 'confirm-settings]
              [else (transmit "invalid module path\n") #f])]]
      [master-classname
       [(transmit "Enter the master object's class name\n\e[33m[This must be provided by the module specified by master-module]\e[0m\n") #f]
       [
        (define tmsg (string-trim msg))
        (define old (hash-ref cfg 'master-classname #f))
        (cond [(and (string=? tmsg "") (identifier-string? old)) 'confirm-settings]
              [(string=? tmsg "") (transmit "master class name is required\n") #f]
              [(identifier-string? tmsg) (hash-set! cfg 'master-classname tmsg) 'confirm-settings]
              [else (transmit "invalid class name\n") #f])]]
      [mudlib-path
       [(transmit "Enter the additional collects directory that contains the mudlib\n"
                  "\e[33m[enter #f if the mudlib is already in the library search path]\e[0m\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'mudlib-path #f) 'confirm-settings]
              [(path-string? tmsg) (hash-set! cfg 'mudlib-path tmsg) 'confirm-settings]
              [else (transmit "mudlib-path must be a path or #f\n") #f])]]
      [ssl:certificate
       [(transmit "Enter the path to the SSL certificate, or #f to disable SSL\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'ssl:certificate #f) 'confirm-settings]
              [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
              [(not (file-exists? tmsg)) (transmit "file not found\n") #f]
              [else (hash-set! cfg 'ssl:certificate tmsg) 'confirm-settings])]]
      [ssl:private-key
       [(transmit "Enter the path to the SSL private key, or #f to disable SSL\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'ssl:private-key #f) 'confirm-settings]
              [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
              [(not (file-exists? tmsg)) (transmit "file not found\n") #f]
              [else (hash-set! cfg 'ssl:private-key tmsg) 'confirm-settings])]]
      [telnet:port
       [(transmit "Enter the telnet server port (raw), or #f to disable raw telnet\n") #f]
       [
        (define tmsg (string-trim msg))
        (define port (string->number tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'telnet:port #f) 'confirm-settings]
              [(port-number? port) (hash-set! cfg 'telnet:port port) 'confirm-settings]
              [else (transmit "invalid port number\n") #f])]]
      [telnet:ssl-port
       [(transmit "Enter the SSL encrypted telnet server port, or #f to disable SSL telnet\n") #f]
       [
        (define tmsg (string-trim msg))
        (define port (string->number tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'telnet:ssl-port #f) 'confirm-settings]
              [(port-number? port) (hash-set! cfg 'telnet:ssl-port port) 'confirm-settings]
              [else (transmit "invalid port number\n") #f])]]
      [telnet:encodings
       [
        (define enc (hash-ref cfg 'telnet:encodings '()))
        (define counter (box 1))
        (transmit "Current encodings:\n")
        (for-each (lambda (e)
                    (transmit "\t" (box++ counter) "\t" e)
                    (when (eq? undefined (string->mib e))
                      (transmit "\t\e[33;1m[INVALID]\e[0m"))
                    (transmit "\n")) enc)
        (transmit "Commands\n\t[Enter] - return to main settings menu\n\tr <num> remove entry number <num>\n\ta <num> <encoding> add <encoding> before item <num>\n")
        #f]
       [
        (define enc (hash-ref cfg 'telnet:encodings '()))
        (define cmd (string-split (string-foldcase msg)))
        (match cmd
          ['() 'confirm-settings]
          [(list "r" ns) (let [(n (string->number ns))]
                           (cond [(not (exact-positive-integer? n)) (transmit "invalid position\n") #f]
                                 [(> n (length enc)) (transmit "invalid position\n") #f]
                                 [else (hash-set! cfg 'telnet:encodings (remv (list-ref enc (sub1 n)) enc)) 'telnet:encodings]))]
          [(list "a" ns estr) (let [(n (string->number ns))]
                                (cond [(not (exact-positive-integer? n)) (transmit "invalid position\n") #f]
                                      [(> n (add1 (length enc))) (transmit "invalid position\n") #f]
                                      [(not (number? (string->mib estr))) (transmit "unknown encoding\n") #f]
                                      [else (hash-set! cfg 'telnet:encodings (insert-at enc (sub1 n) (string-upcase estr))) 'telnet:encodings]))]
          [else (transmit "unknown command\n") #f])]]
      [interactive
       [(transmit "Enter #t to start the server in interactive mode, #f to disable interactive mode\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'interactive #f) 'confirm-settings]
              [else (hash-set! cfg 'interactive #t)])]]
      [thread-count
       [(transmit "Enter the number of event handler threads (these handle internal events, network connections each get their own threads)\n") #f]
       [
        (define tmsg (string-trim msg))
        (define n (string->number tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(exact-positive-integer? n) (hash-set! cfg 'thread-count n) 'confirm-settings]
              [else (transmit "Thread count must be a positive integer\n") #f])]]
      [webserver:port
       [(transmit "Enter the HTTP port\n") #f]
       [
        (define tmsg (string-trim msg))
        (define port (string->number tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'webserver:port #f) 'confirm-settings]
              [(port-number? port) (hash-set! cfg 'webserver:port port) 'confirm-settings]
              [else (transmit "invalid port number\n") #f])]]
      [webserver:ssl-port
       [(transmit "Enter the HTTP port\n") #f]
       [
        (define tmsg (string-trim msg))
        (define port (string->number tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(string=? tmsg "#f") (hash-set! cfg 'webserver:ssl-port #f) 'confirm-settings]
              [(port-number? port) (hash-set! cfg 'webserver:ssl-port port) 'confirm-settings]
              [else (transmit "invalid port number\n") #f])]]
      [webserver:www-path
       [(transmit "Enter the directory to be used as the rackmud www root\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
              [(not (directory-exists? tmsg)) (transmit "directory not found\n") #f]
              [else (hash-set! cfg 'webserver:www-path
                               (path->string (simplify-path (path->complete-path tmsg)))) 'confirm-settings])]]
      [webserver:servlet-url
       [(transmit "Enter the URL prefix for servlet pages\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(and (path-string? tmsg) (relative-path? tmsg)) (hash-set! cfg 'webserver:servlet-url tmsg) 'confirm-settings]
              [else (transmit "invalid URL prefix\n") #f])]]
      [webserver:websock-url
       [(transmit "Enter the URL prefix for websocket connections\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(and (path-string? tmsg) (relative-path? tmsg)) (hash-set! cfg 'webserver:websock-url tmsg) 'confirm-settings]
              [else (transmit "invalid URL prefix\n") #f])]]   
      [webserver:websock-client-url
       [(transmit "Enter the URL suffix for the built-in websocket client\n"
                  "\e[33m[the complete URL will be the websocket URL followed by this suffix]\e[0m\n") #f]
       [
        (define tmsg (string-trim msg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(and (path-string? tmsg) (relative-path? tmsg)) (hash-set! cfg 'webserver:websock-client-url tmsg) 'confirm-settings]
              [else (transmit "invalid URL prefix\n") #f])]]
      [filename
       [(transmit "Enter the name of the configuration file\n"
                  "\e[33m[recommended value is"
                  default-config-name
                  "]\e[0m\n") #f]
       [(define tmsg (string-trim msg))
        
        (define filename (pathstring->filename tmsg))
        (cond [(string=? tmsg "") 'confirm-settings]
              [(eq? filename 'bad-path)
               (transmit "bad path for configuration file\n") #f]
              [(eq? filename 'bad-dir)
               (transmit "bad directory for configuration file\n") #f]
              [(eq? filename 'invalid-file-name)
               (transmit "bad file name\n") #f]
              [else
               (unless (string=? default-config-name filename)
                 (transmit "This is not the default file name.  You'll need to use a flag each time you launch the server!\n"))
               (hash-set! cfg 'filename tmsg)
               'confirm-settings])]])))

(define (rackmud-configure current-config #:start [start 'start] #:in [in (current-input-port)])

  (define new-config (hash-copy current-config))
  (define new-menu
    (new new-install-menu%
         [configuration new-config]
         [initial-state start]
         [enter-on-create? #t]
         [in in]))
  (send new-menu input-loop)
  (and (eq? #t (send new-menu get-return-status)) new-config))
;(if (eq? #t return-status) new-config #f))


(module+ main
  ;  (define-values (s h) (config->string (load-rackmud-settings)))
  (rackmud-configure (load-rackmud-settings)))
; (display s)
  
