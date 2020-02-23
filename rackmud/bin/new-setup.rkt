#lang racket/base

(require racket/string racket/match racket/format racket/list racket/class racket/tcp "../menu.rkt" "../db.rkt" "config.rkt" telnet/charset racket/undefined db/base racket/pretty
         racket/runtime-path racket/file
         (for-syntax racket/base))
;(require racket/port racket/system)
;(define terminal-width (min 120 (or (string->number (string-trim (with-output-to-string (lambda () (system "tput cols"))))) 80)))

(define terminal-width 80)
(define-runtime-path HERE ".")
;(define (make-rackmud-db-connection db-type db-port db-sock db-srv db-db db-user db-pass)



(define (file->statements file)
  (map (lambda (s) (string-append s ";"))
       (filter (lambda (s) (not (string=? s "")))
               (map string-trim
                    (string-split (file->string file) ";")))))

(define-runtime-paths [postgres-path mysql-path sqlite3-path]
  (values "data/postgre.sql" "data/mysql.sql" "data/sqlite.sql"))

(define (dbtype->path type)
  (case type
    [(postgres) postgres-path]
    [(mysql) mysql-path]
    [(sqlite3) sqlite3-path]
    [else #f]))

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

;; (verify-db-settings cfg) produces #f if cfg contains invalid database settings, 'no-schema if the connection
;;   can be established but it does not contain the rackmud tables, or the version of the rackmud tables present.
(define (verify-db-settings cfg)
  (define conn (cfg->conn cfg))
  (and conn (begin0 (or (rackmud-db-version conn) 'no-schema) (disconnect conn))))
        
(define (load-db-schema cfg)
  (define type (hash-ref cfg 'database:type #f))
  (define conn (cfg->conn cfg))
  (define statements (file->statements (dbtype->path type)))
  (for ([statement (in-list statements)])
    (query-exec conn statement))
  (disconnect conn))
    


(define (save-config vars)
  (define filename (hash-ref vars 'config-name))
  (define am (hash-ref vars 'action-map))
  (define cfg (hash-ref vars 'config))
  (define issues (hash-ref vars 'issues))
  (unless (string=? issues "")
    (error 'save-config "Cannot save a config with invalid settings"))
  (define out (open-output-file filename))
  (define new-cfg  (reverse (filter cdr (map (lambda (sym) (cons sym (hash-ref cfg sym #f))) (hash-values am)))))
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


;  (config-option out 'interactive item-box int->setting)
(define (config-option out name counter int->setting config-table error-table #:format [~~ ~v])
  (define split (string-split (symbol->string name) ":"))
  (define int (box++ counter))
  (define v (hash-ref config-table name #f))
  (define hint (hash-ref error-table name #f))
  (define hint-text
    (cond [(not hint) ""]
          [(string? hint) (string->error-string hint)]
          [else (error 'config-option "Bad hint for ~a (~a)" name hint)]))
  (hash-set! int->setting int name)
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


(define (config-issues cfg)
  (define out (open-output-string))
  (define error-flags (make-hasheq))
  
  (match (hash-ref cfg 'database:type #f)
    [#f (displayln "* database type not set" out)
        (hash-set! error-flags 'database:type "Required")]
    [(or 'postgres 'mysql)
     (unless (string? (hash-ref cfg 'database:username #f))
       (displayln "* database user not set" out)
       (hash-set! error-flags 'database:username "Required"))
     (unless (string? (hash-ref cfg 'database:database #f))
       (displayln "* database not set" out)
       (hash-set! error-flags 'database:database "Required"))
     (when (and (or (hash-ref cfg 'database:server #f)
                    (hash-ref cfg 'database:port #f))
                (hash-ref cfg 'database:socket #f))
       (displayln "* database set to both TCP and local socket" out)
       (hash-set! error-flags 'database:socket "TCP / Socket Mutually Exclusive")
       (when (hash-ref cfg 'database:server #f)
         (hash-set! error-flags 'database:server "TCP / Socket Mutually Exclusive"))
       (when (hash-ref cfg 'database:port #f)
         (hash-set! error-flags 'database:port "TCP / Socket Mutually Exclusive")))
     
     (when (and (hash-ref cfg 'database:port #f)
                (not (port-number? (hash-ref cfg 'database:port #f))))
       (displayln "* database port is not a valid port number" out)
       (hash-set! error-flags 'database:port "Invalid Port"))]
     ['sqlite3
      (cond [(not (hash-ref cfg 'database:database #f))
             (displayln "* database not set" out)
             (hash-set! error-flags 'database:database "Required")]
            [(not (and (path-string? (hash-ref cfg 'database:database))
                       (file-exists? (hash-ref cfg 'database:database))))
             (displayln "* database file not found" out)
             (hash-set! error-flags 'database:database "File Not Found")])])
  
  (match (hash-ref cfg 'mudlib-collect #f)
    [#f (displayln "* mudlib collection not set" out)
        (hash-set! error-flags 'mudlib-collect "Required")]
    [(? path-string?) #t]
    [else (displayln "* mudlib collection not valid" out)
          (hash-set! error-flags 'mudlib-collect "Invalid")])
  (match (hash-ref cfg 'master-module #f)
    [#f (displayln "* master module not set" out)
        (hash-set! error-flags 'master-module "Required")]
    [(? path-string?) #t]
    [else (displayln "* master module not valid" out)
          (hash-set! error-flags 'master-module "Invalid")])

  (match (hash-ref cfg 'master-classname #f)
    [#f (displayln "* master classname not set" out)
        (hash-set! error-flags 'master-classname "Required")]
    [(? identifier-string?) #t]
    [else (displayln "* master classname not a valid identifier" out)
          (hash-set! error-flags 'master-classname "Invalid")])
  
  (match (hash-ref cfg 'mudlib-path #f)
    [#f #t]
    [(and (? path-string?) (? directory-exists?)) #t]
    [(? path-string?)
     (displayln "* extra collection directory not found" out)
     (hash-set! error-flags 'mudlib-path "Not Found")]
    [else (displayln "* extra collection directory not a valid path" out)
          (hash-set! error-flags 'mudlib-path "Invalid")])
  
  (define cert (hash-ref cfg 'ssl:certificate #f))
  (define pkey (hash-ref cfg 'ssl:private-key #f))
  (define ssl? (and cert pkey))
  
  (when cert
    (cond [(not (path-string? cert))
           (displayln "* SSL certificate set but is not a valid path" out)
           (hash-set! error-flags 'ssl:certificate "Invalid")
           (set! ssl? #f)]
          [(not (file-exists? cert))
           (displayln "* SSL certificate set but file not found" out)
           (hash-set! error-flags 'ssl:certificate "Not Found")
           (set! ssl? #f)]))

  (when pkey
    (cond [(not (path-string? pkey))
           (displayln "* SSL private key set but is not a valid path" out)
           (hash-set! error-flags 'ssl:private-key "Invalid")
           (set! ssl? #f)]
          [(not (file-exists? pkey))
           (displayln "* SSL certificate set but file not found" out)
           (hash-set! error-flags 'ssl:private-key "Not Found")
           (set! ssl? #f)]))

  (when (and cert (not pkey))
    (displayln "* SSL certificate configured, but private key is not" out)
    (hash-set! error-flags 'ssl:private-key "Not Set"))
  (when (and pkey (not cert))
    (displayln "* SSL private key configured, but certificate is not" out)
    (hash-set! error-flags 'ssl:certificate "Not Set"))

  (define telnet-port (hash-ref cfg 'telnet:port #f))
  (define stelnet-port (hash-ref cfg 'telnet:ssl-port #f))
  
  (when (and telnet-port
             (not (port-number? telnet-port)))
    (displayln "* telnet port set to an invalid port number" out)
    (hash-set! error-flags 'telnet:port "Invalid")
    (set! telnet-port #f))

  (when (and stelnet-port
             (not (port-number? stelnet-port)))
    (displayln "* secure telnet port set to an invalid port number" out)
    (hash-set! error-flags 'telnet:ssl-port "Invalid")
    (set! stelnet-port #f))
  (when (and stelnet-port
             (not ssl?))
    (displayln "* secure telnet port set, but SSL not properly configured" out)
    (hash-set! error-flags 'telnet:ssl-port "SSL Disabled")
    (set! stelnet-port #f))
  (define telnet? (or telnet-port stelnet-port))

  (when telnet?
    (define encs (hash-ref cfg 'telnet:encodings '()))
    (unless (member "ASCII" encs)
      (displayln "* telnet encodings must at least contain ASCII" out)
      (hash-set! error-flags 'telnet:encodings "Invalid")))

  (define http-port (hash-ref cfg 'webserver:port #f))
  (define https-port (hash-ref cfg 'webserver:ssl-port #f))
  (when (and https-port (not ssl?))
    (displayln "* HTTPS port set, but SSL not properly configured" out)
    (hash-set! error-flags 'webserver:ssl-port "SSL Disabled")
    (set! https-port #f))
  (when (and http-port (not (port-number? http-port)))
    (displayln "* HTTP port set, but not to a valid port number" out)
    (hash-set! error-flags 'webserver:port "Invalid")
    (set! http-port #f))
  (when (and https-port (not (port-number? https-port)))
    (displayln "* HTTPS port set, but not to a valid port number" out)
    (hash-set! error-flags 'webserver:ssl-port "Invalid")
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
      (hash-set! error-flags 'webserver:www-path (if (path-string? www-path) "Not Found" "Invalid"))
      (displayln "* WWW root is not valid" out))
    (unless (path-string? servlet-url)
      (displayln "* Servlet URL is not valid" out)
      (hash-set! error-flags 'webserver:servlet-url "Invalid"))
    (unless (path-string? websock-url)
      (displayln "* Websock URL is not valid" out)
      (hash-set! error-flags 'webserver:websock-url "Invalid"))
    (when websock-client-url
      (cond [(path-string? websock-client-url) (set! websock? #t)]
            [else (displayln "* Websock Client URL is not valid" out)
                  (hash-set! error-flags 'webserver:websock-client-url "Invalid")])))

  (unless (exact-nonnegative-integer? (hash-ref cfg 'thread-count #f))
    (displayln "* thread count is not a positive integer" out)
    (hash-set! error-flags 'thread-count "Invalid"))
  
  (when (not (or telnet? websock?))
    (displayln "* Neither telnet nor websock enabled, server is not usable" out)
    (hash-set! error-flags 'general "Neither Telnet Nor Websock Enabled"))
  
  (values (get-output-string out) error-flags))



(define (display-database-settings config-table error-table out counter int->setting)
  (define type (hash-ref config-table 'database:type #f))
  (config-option out 'database:type counter int->setting config-table error-table #:format none-fmt)
  (cond [(memq type '(postgres mysql))
         (config-option out 'database:database counter int->setting config-table error-table #:format none-fmt)
         (config-option out 'database:username counter int->setting config-table error-table #:format none-fmt)
         (config-option out 'database:password counter int->setting config-table error-table #:format password-fmt)
         (config-option out 'database:server counter int->setting config-table error-table #:format default-fmt)
         (config-option out 'database:port counter int->setting config-table error-table #:format default-fmt)
         (config-option out 'database:socket counter int->setting config-table error-table #:format none-fmt)]
        [(eq? type 'sqlite3)
         (config-option out 'database:database counter int->setting config-table error-table #:format none-fmt)]))
       
(define (display-web-settings config-table error-table out counter int->setting)
  (define http (hash-ref config-table 'webserver:port #f))
  (define https (hash-ref config-table 'webserver:ssl-port #f))
  (config-option out 'webserver:port counter int->setting config-table error-table #:format (if-false "(disabled)"))
  (config-option out 'webserver:ssl-port counter int->setting config-table error-table #:format (if-false "(disabled)"))
  (when (or http https)
    (config-option out 'webserver:www-path counter int->setting config-table error-table)
    (config-option out 'webserver:servlet-url counter int->setting config-table error-table)
    (config-option out 'webserver:websock-url counter int->setting config-table error-table)
    (config-option out 'webserver:websock-client-url counter int->setting config-table error-table)))

(define (config->string config-table filename)
  (define out (open-output-string))
  (define counter (box 1))
  (define int->setting (make-hasheqv))
  (define-values (issues error-table)  (config-issues config-table))
  (fprintf out "===Configuration File Settings===\n")
  (let* ([int (box++ counter)]
         [line (format "~a) filename             = ~v" (padl (number->string int) 2) filename)])
    (display line out)
    (hash-set! int->setting int 'filename)
    (displayln (if (string=? filename default-config-name) "" (padl "\e[33;1m[Will require command line argument]\e[0m" (- terminal-width (string-length line)))) out))

  (fprintf out "\n===Database Settings===\n")
  (display-database-settings config-table error-table out counter int->setting)
  
  (fprintf out "\n===Mudlib Settings===\n")
  (config-option out 'mudlib-collect counter int->setting config-table error-table)
  (config-option out 'master-module counter int->setting config-table error-table)
  (config-option out 'master-classname counter int->setting config-table error-table)
  (config-option out 'mudlib-path counter int->setting config-table error-table #:format none-fmt)


  (fprintf out "\n===SSL Settings===\n")
  (config-option out 'ssl:certificate counter int->setting config-table error-table #:format none-fmt)
  (config-option out 'ssl:private-key counter int->setting config-table error-table #:format none-fmt)

  (fprintf out "\n===Telnet Settings===\n")
  (config-option out 'telnet:port counter int->setting config-table error-table #:format (if-false "(disabled)"))
  (config-option out 'telnet:ssl-port counter int->setting config-table error-table #:format (if-false "(disabled)"))
  (config-option out 'telnet:encodings counter int->setting config-table error-table #:format (if-false "(default)"))
  
  (fprintf out "\n===Web Settings===\n")
  (display-web-settings config-table error-table out counter int->setting)

  (fprintf out "\n===Misc Settings===\n")
  (config-option out 'interactive counter int->setting config-table error-table)
  (config-option out 'thread-count counter int->setting config-table error-table)
  (when (hash-ref error-table 'general #f)
    (displayln (padl (string->error-string (hash-ref error-table 'general)) terminal-width) out))

  ;  (display-config-issues config-table out)

  (values (get-output-string out) int->setting issues))
  
(define new-install-menu%
  (make-menu
   [start
    [(transmit "Would you like to create one now?\n") #f]
    [(match (string-foldcase msg)
       [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") 'confirm-settings]
       [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$")
        (transmit eof)]
       [else 'start])]]
   [confirm-name
    [(if (string=? default-config-name (hash-ref vars 'config-name))
         'validate
         (begin
           (transmit (format "You have launched the server with a non-default config file '~a'.  Would you like to use this name for the new config file? (Answering 'no' will use the default [~a])\n"
                             (hash-ref vars 'config-name) default-config-name))
           #f))]
    [(match
       [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") 'validate]
       [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$") (hash-set! vars 'config-name default-config-name) 'validate]
       [else (transmit "Please answer Y or N only\n") #f])]]
   [confirm-settings
    [(define-values (msg action-map issues) (config->string (hash-ref vars 'config) (hash-ref vars 'config-name)))
     (hash-set! vars 'action-map action-map)
     (hash-set! vars 'issues issues)
     (transmit msg
               (format "\nEnter a setting to change, or press [Enter] to write your settings to '~a'\n" (hash-ref vars 'config-name)))]
    [(let* ([selection (string->number (string-trim msg))]
            [action-map (hash-ref vars 'action-map)]
            [issues (hash-ref vars 'issues)]
            [change-settings (hash-ref action-map selection #f)])
       (cond [change-settings change-settings]
             [(and (string=? (string-trim msg) "")
                   (string=? issues ""))
              'confirm-name]
             [(string=? (string-trim msg) "")
              (transmit "Cannot save configuration due to the following issue(s)\n\n\e[31;1m" issues "\e[0m")
              #f]
             [else (transmit "Invalid Selection") #f]))]]
   [validate
    [(define db-version (verify-db-settings (hash-ref vars 'config)))
     (case db-version
       [(#f) (transmit "\e[31;1mdatabase connection failure, please confirm your database configuration\e[0m\n") 'confirm-settings]
       [(no-schema) 'configure-database]
       [else (hash-set! vars 'database-version db-version) 'upgrade-db])]
    [(error 'validate "not implemented")]]
   [upgrade-db
    [(transmit (format "Connected to database (version=~a)\n" (hash-ref vars 'database-version))) 'confirm-mudlib]
    [(error 'upgrade-db "not implemented")]] ; TODO - upgrade scripts 
   [configure-database
    [(transmit "Connected to database, but no rackmud scheme was found.  Create tables now? (Y / N)\n") #f]
    [(match (string-foldcase msg)
       [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") (load-db-schema (hash-ref vars 'config)) 'confirm-mudlib]
       [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$") 'save-verification]
       [else #f])]]
   [confirm-mudlib
    ;; TODO
    ['save-verification]
    [#f]]
   [save-verification
    [(transmit "Save settings and continue?\n") #f]
    [(match (string-foldcase msg)
       [(pregexp #px"^\\s*y(a|eah|es)?\\s*$") (save-config vars) (transmit #t)]
       [(pregexp #px"^\\s*(n(o|ah|ope)?)?\\s*$") 'confirm-settings]
       [else 'save-verification])]]
   [database:type
    [(transmit "Select database type (postgres, mysql, sqlite3) or press [enter] to leave unchanged\n") #f]
    [(define cfg (hash-ref vars 'config))
     (match (string-foldcase (string-trim msg))
       ["postgres" (hash-set! cfg 'database:type 'postgres) 'confirm-settings]
       ["mysql" (hash-set! cfg 'database:type 'mysql) 'confirm-settings]
       ["sqlite3" (hash-set! cfg 'database:type 'sqlite3) 'confirm-settings]
       ["" 'confirm-settings]
       [else (transmit "invalid selection\n") 'database:type])]]
   [database:database
    [(if (eq? (hash-ref (hash-ref vars 'config) 'database:type) 'sqlite3)
         (transmit "Enter sqlite3 file name, or press [Enter] to leave unchanged\n")
         (transmit "Enter database name, or press [Enter] to leave unchanged\n")) #f]
    [(define cfg (hash-ref vars 'config))
     (define type (hash-ref cfg 'database:type))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(not (eq? type 'sqlite3))
            (hash-set! cfg 'database:database tmsg)
            'confirm-settings]
           [(and (path-string? tmsg) (file-exists? tmsg))
            (hash-set! cfg 'database:database tmsg) 'confirm-settings]
           [(transmit "File not found!\n") 'database:database])]]
   [database:username
    [(transmit "Enter database username, or press [Enter] to leave unchanged\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [else (hash-set! cfg 'database:username tmsg) 'confirm-settings])]]
   [database:password
    [(transmit "Enter database password, or press [Enter] if database login does not require a password\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") (hash-set! cfg 'database:password #f) 'confirm-settings]
           [else (hash-set! cfg 'database:password tmsg) 'confirm-settings])]]
   [database:server
    [(transmit "Enter the TCP address of the database server.  Use #f if using a system socket (or to use localhost)\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'database:server #f) 'confirm-settings]
           [else (hash-set! cfg 'database:server tmsg) 'confirm-settings])]]
   [database:port
    [(transmit "Enter the TCP port of the database server.  Use #f if using a system socket (or to use the default port)\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'database:port #f) 'confirm-settings]
           [(port-number? (string->number tmsg)) (hash-set! cfg 'database:port (string->number tmsg)) 'confirm-settings]
           [else (transmit "Enter a valid port number, or #f\n") #f])]]
   [database:socket
    [(if (eq? (hash-ref (hash-ref vars 'config) 'database:type) 'postgres)
         (transmit "Enter system socket for postgres connection, or 'guess, or #f if not using a system socket\n")
         (transmit "Enter system socket for mysql connection, or #f if not using a system socket\n"))]
    [(define cfg (hash-ref vars 'config))
     (define type (hash-ref cfg 'database:type))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'database:socket #f) 'confirm-settings]
           [(and (eq? type 'postgres) (string=? tmsg "'guess"))
            (hash-set! cfg 'database:socket 'guess) 'confirm-settings]
           [(path-string? tmsg) (hash-set! cfg 'database:socket tmsg) 'confirm-settings]
           [else (transmit "Enter a valid socket path, or #f\n") #f])]]
   [mudlib-collect
    [(transmit "Enter the collection name for the mudlib\n\e[33m[The default is \"mudlib\" and that's not a bad choice]\e[0m\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define old (hash-ref cfg 'mudlib-collect #f))
     (cond [(and (string=? tmsg "") (path-string? old)) 'confirm-settings]
           [(string=? tmsg "") (transmit "mudlib collection is required\n") #f]
           [(path-string? tmsg) (hash-set! cfg 'mudlib-collect tmsg) 'confirm-settings]
           [else (transmit "invalid module path\n") #f])]]
   [master-module
    [(transmit "Enter the module that defines the master object class\n\e[33m[This must be a module path relative to the mudlib collect]\e[0m\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define old (hash-ref cfg 'master-module #f))
     (cond [(and (string=? tmsg "") (path-string? old)) 'confirm-settings]
           [(string=? tmsg "") (transmit "master module is required\n") #f]
           [(path-string? tmsg) (hash-set! cfg 'master-module tmsg) 'confirm-settings]
           [else (transmit "invalid module path\n") #f])]]
   [master-classname
    [(transmit "Enter the master object's class name\n\e[33m[This must be provided by the module specified by master-module]\e[0m\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define old (hash-ref cfg 'master-classname #f))
     (cond [(and (string=? tmsg "") (identifier-string? old)) 'confirm-settings]
           [(string=? tmsg "") (transmit "master class name is required\n") #f]
           [(identifier-string? tmsg) (hash-set! cfg 'master-classname tmsg) 'confirm-settings]
           [else (transmit "invalid class name\n") #f])]]
   [mudlib-path
    [(transmit "Enter the additional collects directory that contains the mudlib\n"
               "\e[33m[enter #f if the mudlib is already in the library search path]\e[0m\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'mudlib-path #f) 'confirm-settings]
           [(path-string? tmsg) (hash-set! cfg 'mudlib-path tmsg) 'confirm-settings]
           [else (transmit "mudlib-path must be a path or #f\n") #f])]]
   [ssl:certificate
    [(transmit "Enter the path to the SSL certificate, or #f to disable SSL\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'ssl:certificate #f) 'confirm-settings]
           [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
           [(not (file-exists? tmsg)) (transmit "file not found\n") #f]
           [else (hash-set! cfg 'ssl:certificate tmsg) 'confirm-settings])]]
   [ssl:private-key
    [(transmit "Enter the path to the SSL private key, or #f to disable SSL\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'ssl:private-key #f) 'confirm-settings]
           [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
           [(not (file-exists? tmsg)) (transmit "file not found\n") #f]
           [else (hash-set! cfg 'ssl:private-key tmsg) 'confirm-settings])]]
   [telnet:port
    [(transmit "Enter the telnet server port (raw), or #f to disable raw telnet\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define port (string->number tmsg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'telnet:port #f) 'confirm-settings]
           [(port-number? port) (hash-set! cfg 'telnet:port port) 'confirm-settings]
           [else (transmit "invalid port number\n") #f])]]
   [telnet:ssl-port
    [(transmit "Enter the SSL encrypted telnet server port, or #f to disable SSL telnet\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define port (string->number tmsg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'telnet:ssl-port #f) 'confirm-settings]
           [(port-number? port) (hash-set! cfg 'telnet:ssl-port port) 'confirm-settings]
           [else (transmit "invalid port number\n") #f])]]
   [telnet:encodings
    [(define cfg (hash-ref vars 'config))
     (define enc (hash-ref cfg 'telnet:encodings '()))
     (define counter (box 1))
     (transmit "Current encodings:\n")
     (for-each (lambda (e)
                 (transmit "\t" (box++ counter) "\t" e)
                 (when (eq? undefined (encoding->mib e))
                   (transmit "\t\e[33;1m[INVALID]\e[0m"))
                 (transmit "\n")) enc)
     (transmit "Commands\n\t[Enter] - return to main settings menu\n\tr <num> remove entry number <num>\n\ta <num> <encoding> add <encoding> before item <num>\n")
     #f]
    [(define cfg (hash-ref vars 'config))
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
                                   [(not (number? (encoding->mib estr))) (transmit "unknown encoding\n") #f]
                                   [else (hash-set! cfg 'telnet:encodings (insert-at enc (sub1 n) (string-upcase estr))) 'telnet:encodings]))]
       [else (transmit "unknown command\n") #f])]]
   [interactive
    [(transmit "Enter #t to start the server in interactive mode, #f to disable interactive mode\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'interactive #f) 'confirm-settings]
           [else (hash-set! cfg 'interactive #t)])]]
   [thread-count
    [(transmit "Enter the number of event handler threads (these handle internal events, network connections each get their own threads)\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define n (string->number tmsg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(exact-positive-integer? n) (hash-set! cfg 'thread-count n) 'confirm-settings]
           [else (transmit "Thread count must be a positive integer\n") #f])]]
   [webserver:port
    [(transmit "Enter the HTTP port\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define port (string->number tmsg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'webserver:port #f) 'confirm-settings]
           [(port-number? port) (hash-set! cfg 'webserver:port port) 'confirm-settings]
           [else (transmit "invalid port number\n") #f])]]
   [webserver:ssl-port
    [(transmit "Enter the HTTP port\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (define port (string->number tmsg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(string=? tmsg "#f") (hash-set! cfg 'webserver:ssl-port #f) 'confirm-settings]
           [(port-number? port) (hash-set! cfg 'webserver:ssl-port port) 'confirm-settings]
           [else (transmit "invalid port number\n") #f])]]
   [webserver:www-path
    [(transmit "Enter the directory to be used as the rackmud www root\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(not (path-string? tmsg)) (transmit "invalid path\n") #f]
           [(not (directory-exists? tmsg)) (transmit "directory not found\n") #f]
           [else (hash-set! cfg 'webserver:www-path
                            (path->string (simplify-path (path->complete-path tmsg)))) 'confirm-settings])]]
   [webserver:servlet-url
    [(transmit "Enter the URL prefix for servlet pages\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(and (path-string? tmsg) (relative-path? tmsg)) (hash-set! cfg 'webserver:servlet-url tmsg) 'confirm-settings]
           [else (transmit "invalid URL prefix\n") #f])]]
   [webserver:websock-url
    [(transmit "Enter the URL prefix for websocket connections\n") #f]
    [(define cfg (hash-ref vars 'config))
     (define tmsg (string-trim msg))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(and (path-string? tmsg) (relative-path? tmsg)) (hash-set! cfg 'webserver:websock-url tmsg) 'confirm-settings]
           [else (transmit "invalid URL prefix\n") #f])]]   
   [webserver:websock-client-url
    [(transmit "Enter the URL suffix for the built-in websocket client\n"
               "\e[33m[the complete URL will be the websocket URL followed by this suffix]\e[0m\n") #f]
    [(define cfg (hash-ref vars 'config))
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
     (define-values [root filename must-be-dir?]
       (if (path-string? tmsg) (split-path tmsg) (values #f #f #f)))
     (cond [(string=? tmsg "") 'confirm-settings]
           [(or must-be-dir? (and filename (directory-exists? tmsg)))
            (transmit "configuration file must be a file, not a directory\n") #f]
           [(eq? root 'relative)
            (hash-set! vars 'config-name tmsg) 'confirm-settings]
           [(and (path? root) (directory-exists? root))
            (hash-set! vars 'config-name tmsg) 'confirm-settings]
           [(path? root) (transmit (format "Directory ~v does not exist\n" root)) #f]
           [else (transmit "Invalid filename") #f])]]
 ))   
(define (new-setup current-config config-name)
  (define return-status (void))
  (define (cli-transmit/single msg)
    (match msg
      [(? string?) (display msg)]
      [(? number?) (display msg)]
      [(? eof-object?) (set! return-status #f)]
      [(? boolean?) (set! return-status msg)]
      ))

  (define (cli-transmit . messages)
    (for-each cli-transmit/single messages))

  (define new-config (hash-copy current-config))
  (define vars (make-hasheq `((config . ,new-config) (config-name . ,config-name))))
  
  (define new-menu                                                                                                                                                                                                (new new-install-menu%
         [transmit-func cli-transmit]
         [vars vars]))
  (send new-menu set-state! 'start)
  (let loop ()
    (when (void? return-status)
      (let ([line (read-line)])
        (unless (eof-object? line)
          (send new-menu receive line)
          (loop)))))
  return-status)
  ;(if (eq? #t return-status) new-config #f))


(module+ main
  ;  (define-values (s h) (config->string (load-rackmud-settings)))
  (new-setup (load-rackmud-settings) default-config-name))
  ; (display s)
  
