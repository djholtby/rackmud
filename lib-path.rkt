#lang racket/base

(require racket/path racket/list)

(provide lib-path filepath path-begins-with? filepath filepath/index set-lib-path! relative-module-path)

(define lib-path #f)

(define (set-lib-path! p)
  (unless (or (path? p)
              (path-string? p)) (raise-argument-error 'set-lib-path! "(or/c path? path-string?)" p))
  (set! lib-path (path->directory-path (simplify-path (if (path? p) p (string->path p))))))

(define-syntax-rule (filepath)
  (variable-reference->resolved-module-path (#%variable-reference)))

(define-syntax-rule (filepath/index)
  (variable-reference->module-path-index (#%variable-reference)))

(define (relative-module-path mod-path)
  (unless lib-path
    (error 'define-saved-class "lib-path not set when evaluating ~a" mod-path))
  (let ([mpath (resolved-module-path-name mod-path)])
    (cond [(path? mpath) (find-relative-path lib-path mpath)]
          [(cons? mpath)  (cons (find-relative-path lib-path (car mpath)) (cdr mpath))])))

(define (path-begins-with? path dir-path)
  (define-values (base name must-be-dir?) (split-path path))
  (cond [(equal? path dir-path) #t]
        [(not base) #f]
        [else (path-begins-with? base dir-path)]))