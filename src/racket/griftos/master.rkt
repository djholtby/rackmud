#lang racket
(require racket/unsafe/ops)
(require racket/serialize)
(require racket/undefined)
(require "racket-mud.rkt")
(require "telnet.rkt")

(provide master-object%)



(define-mud-class master-object% mud-object% (users ...)
  (super-init)
  (define users (make-hasheq))
  (define sockets (make-hasheq))


  (define/public (get-user name)
    (define actual-name (if (string? name) (string->symbol name) name))
    (hash-ref users actual-name))
  


  (define/public (hook-socket rsock)
    

  ;; (new-connect nsock) is called whenever the telnet server establishes a new connection
  ;;  creates a new racket socket, associates the native and racket sockets in the sockets hash,
  ;;  and calls hook-socket to hook the default menu to the new native socket
  ;; new-connection: CPointer -> Void

  (define/public (new-connection nsock)
    (define rsock (new telnet-socket% [native-socket nsock]))
    (hash-set! sockets nsock rsock)
    (send this hook-socket rsock)
    #<void>)

  ;; (disconnect nsock) forwards the disconnect command to the racket socket and removes it from the hash table

  (define/public (disconnect nsock)
    (when (hash-has-key? sockets nsock)
      (define rsock (hash-ref sockets nsock))
      (send rsock disconnect)
      (hash-remove! sockets nsock)))
  
