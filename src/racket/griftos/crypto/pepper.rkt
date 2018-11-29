#lang racket/base
(require racket/random racket/file racket/function)
(provide get-pepper)

;; DO NOT UNDER ANY CIRCUMSTANCES LET ANYBODY SEE THIS
(define pepper #f)

(define (get-pepper [filename "PEPPER"] ; TODO: user code should be sandboxed without access to this file.  Only the crypto module should 
                    [length   128])     ; 🤷
  (cond [pepper pepper]
        [(file-exists? filename)
         (set! pepper (file->bytes filename))
         pepper]
        [else
         (set! pepper (crypto-random-bytes length))
         (call-with-output-file filename (curry write-bytes pepper))
         pepper]))
  