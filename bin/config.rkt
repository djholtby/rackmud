#lang racket/base

(require racket/hash racket/cmdline)
(provide load-rackmud-settings default-config default-config-name)

(define default-config-name "rackmud-config.rktd")

(define default-config
  (make-immutable-hash
   `((mudlib-path . #f)
     (mudlib-collect . "mudlib")
     (mudlib-runtime-path . "./runtime-collects/")
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
     (filename . ,default-config-name))))

     
(define (config-merge base adjustments)
  (hash-union base adjustments
              #:combine
              (lambda (base-val adjusted-val)
                adjusted-val)))



(define (load-rackmud-settings)
  (define config-name default-config-name)
  (define overrides (box (hasheq)))
  (define (override-set! key val)
    (set-box! (hash-set (unbox overrides) key val)))
  
  (command-line
   #:once-each

   [("-c" "--config") cfg "loads settings from <cfg> (subsequent flags will override)"
                      (set! config-name cfg)
                      ( 'filename cfg)]
   [("-b" "--build") "compiles the mudlib (and then terminates)"
                     (override-set! 'build-only #t)]
   [("-B" "--skip-build") "skips the initial compile of mublib "
                          (override-set! 'compile-mudlib-on-launch? #f)]
   [("-p" "--telnet-port") port "runs telnet on <port>"
                           (override-set! 'telnet:port (string->number port))]
   [("-s" "--secure-telnet-port") port "runs secure telnet on <port> (implies -S)"
                                  (override-set! 'telnet:ssl-port (string->number port))]
   [("-C" "--ssl-cert") cert "path to SSL sertificate"
                        (override-set! 'ssl:certificate cert)]
   [("-K" "--ssl-key") key "path to SSL private key"
                       (override-set! 'ssl:private-key key)]
   [("-r" "--runtime-path") path "path to be used for the runtime mudlib collection"
                            (override-set! 'mudlib-runtime-path path)]
   #:once-any
   [("-i" "--interactive") "run with a Racket REPL hooked into STDIN and STDOUT"
                           (override-set! 'interactive #t)]
   [("--not-interactive") "disables REPL (if enable by cfg file)"
                          (override-set! 'interactive #f)]
   
   #:once-any
   [("-S" "--secure-telnet")  "enables secure telnet" (override-set! 'telnet:ssl #t)]
   [("-N" "--no-secure-telnet") "disables secure telnet" (override-set! 'telnet:ssl #f)])
  
  (define loaded-settings (with-handlers ([exn:fail:filesystem? (λ (e) (make-immutable-hasheq
                                                                        '((no-file? . #t))))])
                            (make-immutable-hasheq (with-input-from-file config-name read))))
  
  (config-merge (config-merge default-config loaded-settings) (unbox overrides)))
