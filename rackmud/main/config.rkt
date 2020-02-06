#lang racket/base

(require racket/hash racket/cmdline)
(provide load-rackmud-settings)

(define default-config
  '#hasheq((mudlib-path . "./mudlibs/mudlib")
           (mudlib-collect . "mudlib")
           (master-module . "main.rkt")
           (master-classname . "custom-master%")
           (thread-count . 4)
           (telnet:port . 4000)
           (telnet:encodings . ("ASCII"))
           (webserver:port . 8000)
           (webserver:www-path . "/www")
           (webserver:servlet-url . "/servlet")
           (webserver:websock-url . "/socket")))


(define (config-merge base adjustments)
  (hash-union base adjustments
              #:combine
              (lambda (bv av)
                av)))



(define (load-rackmud-settings)
  (define config-name "rackmud-config.rktd")
  (define overrides (make-hasheq))
  (command-line
   #:once-each

   [("-c" "--config") cfg "loads settings from <cfg> (subsequent flags will override)" (set! config-name cfg)]
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
   [("-i" "--interactive") "run with a GriftOS REPL hooked into STDIN and STDOUT" (hash-set! overrides 'interactive #t)]
   [("--not-interactive") "disables REPL (if enable by cfg file" (hash-set! overrides 'interactive #f)]
   
   #:once-any
   [("-S" "--secure-telnet")  "enables secure telnet" (hash-set! overrides 'telnet:ssl #t)]
   [("-N" "--no-secure-telnet") "disables secure telnet" (hash-set! overrides 'telnet:ssl #f)])
  
  (define loaded-settings (make-hasheq (with-input-from-file config-name read)))
  (config-merge (config-merge default-config loaded-settings) overrides))