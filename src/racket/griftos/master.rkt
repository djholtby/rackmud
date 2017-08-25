#lang racket
(require racket/unsafe/ops)
(require racket/serialize)
(require racket/undefined)
(require racket/class)
(require json)
(require "racket-mud.rkt")

(provide master-object% telnet-socket%)


(define telnet-socket% (class object%
  (super-new)
  (init-field native-socket)
;  (link-native-socket native-socket this)
  (field [gmcp (make-hash)]
         [on-gmcp void]
         [on-text void])

  (define/public (gmcp-send key value)
    (unless (jsexpr? value) (raise-argument-error 'telnet-socket:gmcp-send "jsexpr?" value))
    (native-gmcp-send native-socket key (jsexpr->bytes value)))

  (define/public (gmcp-receive key value)
    (hash-set! gmcp (bytes->symbol key) (bytes->jsexpr value))
    (on-gmcp gmcp))

  (define/public (send msg)
    (unless (or (string? msg) (bytes? msg))
      (raise-argument-error 'telnet-socket:send "(or string? bytes?)" value))
    (native-send native-socket (if (string? msg) (string->bytes/utf-8 msg) msg)))

  (define/public (rec msg)
    (on-msg (bytes->string/utf-8 msg)))


  ))



(define-mud-class master-object% mud-object% () (users)
  (super-init)
  (define users (make-hasheq))
  (define sockets (make-hasheq))


  (define/public (get-user name)
    (define actual-name (if (string? name) (string->symbol name) name))
    (hash-ref users actual-name))
  


  (define/public (hook-socket rsock)
    )
    

  ;; (new-connect nsock) is called whenever the telnet server establishes a new connection
  ;;  creates a new racket socket, associates the native and racket sockets in the sockets hash,
  ;;  and calls hook-socket to hook the default menu to the new native socket
  ;; new-connection: CPointer -> Void

  (define/public (new-connection nsock)
    (define rsock (new telnet-socket% [native-socket nsock]))
    (hash-set! sockets nsock rsock)
    (send this hook-socket rsock)
    (void))

  ;; (disconnect nsock) forwards the disconnect command to the racket socket and removes it from the hash table

  (define/public (disconnect nsock)
    (when (hash-has-key? sockets nsock)
      (define rsock (hash-ref sockets nsock))
      (send rsock disconnect)
      (hash-remove! sockets nsock)))
  
)
