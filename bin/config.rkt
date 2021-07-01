#lang racket/base

(require racket/hash racket/cmdline)
(provide load-rackmud-settings default-config default-config-name)

(define default-config-name "rackmud-config.rktd")

(define default-config
  `#hasheq((mudlib-path . #f)
           (mudlib-collect . "mudlib")
           (master-module . "main.rkt")
           (master-classname . "custom-master%")
           (thread-count . 16)
           (telnet:port . 4000)
           (telnet:encodings . ("ASCII"))
           (webserver:port . 8000)
           (webserver:www-path . "/www")
           (webserver:servlet-url . "/servlet")
           (webserver:websock-url . "/socket")
           (webserver:server-directory . "/")
           (webserver:proxy-mode . #f)
           (filename . ,default-config-name)))

     
(define (config-merge base adjustments)
  (hash-union base adjustments
              #:combine
              (lambda (bv av)
                av)))



(define (load-rackmud-settings)
  (define config-name default-config-name)
  (define overrides (make-hasheq))
  (command-line
   #:once-each

   [("-c" "--config") cfg "loads settings from <cfg> (subsequent flags will override)"
    (set! config-name cfg)
    (hash-set! overrides 'filename cfg)]
   [("-b" "--build") "compiles the mudlib (and then terminates)" (hash-set! overrides 'build #t)]
   [("-p" "--telnet-port") port "runs telnet on <port>"
    (hash-set! overrides 'telnet:port (string->number port))]
   [("-s" "--secure-telnet-port") port "runs secure telnet on <port> (implies -S)"
    (hash-set! overrides 'telnet:ssl-port (string->number port))]
   [("-C" "--ssl-cert") cert "path to SSL sertificate"
    (hash-set! overrides 'ssl:certificate cert)]
   [("-K" "--ssl-key") key "path to SSL private key"
    (hash-set! overrides 'ssl:private-key key)]
    
   #:once-any
   [("-i" "--interactive") "run with a Racket REPL hooked into STDIN and STDOUT" (hash-set! overrides 'interactive #t)]
   [("--not-interactive") "disables REPL (if enable by cfg file)" (hash-set! overrides 'interactive #f)]
   
   #:once-any
   [("-S" "--secure-telnet")  "enables secure telnet" (hash-set! overrides 'telnet:ssl #t)]
   [("-N" "--no-secure-telnet") "disables secure telnet" (hash-set! overrides 'telnet:ssl #f)])
  
  (define loaded-settings (with-handlers ([exn:fail:filesystem? (λ (e) (make-hasheq '((no-file? . #t))))])
                            (make-hasheq (with-input-from-file config-name read))))
  
  (config-merge (config-merge default-config loaded-settings) overrides))
