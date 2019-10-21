#lang racket/base

(require parser-tools/yacc parser-tools/lex (prefix-in re: parser-tools/lex-sre) racket/hash)
(provide file->cfg)

(define-tokens tokens (NUM BOOL ID STR))
(define-empty-tokens empty-tokens (EOF |{| |}| = |,|)) 

(define cfgl
  (lexer
   [(eof) 'EOF]
   [(re:or whitespace) (cfgl input-port)]
   [(re:or "true" "false")
    (token-BOOL lexeme)]
   [(re:: #\"
          (re:*
           (re:or
            (re:: #\\ any-char)
            (re:~ #\\ #\")))
          #\")
    (token-STR lexeme)]
   [(re:: alphabetic
          (re:*
           (re:or alphabetic numeric #\_ #\-)))
    (token-ID lexeme)]
   
   [(re:: #\#
          (re:* (re:~ #\newline))
          #\newline)
    (cfgl input-port)]
    ;(token-COMMENT lexeme)]
   [(re::
     (re:+ numeric)
     (re:? #\. (re:+ numeric)))
    (token-NUM (string->number lexeme))]
   [(re:or #\{ #\} #\= #\,)
    (string->symbol lexeme)]))
           

(define cfgp
  (parser
   (start start)
   (end EOF)
   (tokens tokens empty-tokens)
   (error void)
   (grammar
    (start
     [(error start) $2]
     [(defns) (make-hasheq $1)])
    
    (defns
      [() '()]
      [(defn defns) (append $1 $2)])

    (defn
      [(ID = value) (list (cons (string->symbol $1) $3))]
      [(ID |{| defns |}|) (map (lambda (pair)
                                 (cons
                                  (string->symbol (string-append $1 ":" (symbol->string (car pair))))
                                  (cdr pair)))
                                  $3)])


    (value
     [(STR) (substring $1 1 (sub1 (string-length $1)))]
     [(NUM) $1]
     [(BOOL) (string=? $1 "true")]
     [(ID) $1]
     [(|{| values |}|) $2])

    (values
     [() '()]
     [(value-list) $1])
    
    (value-list
     [(value) (list $1)]
     [(value |,| value-list) (cons $1 $3)]))))
   
(define (file->cfg filename)
  (define ip (open-input-file filename))
  (with-handlers ([exn:fail:read? (lambda (e)
                                    (close-input-port ip)
                                    #f)])
    (begin0
        (cfgp (lambda () (cfgl ip)))
      (close-input-port ip))))

;(file->cfg "griftos.cfg")
#|(let loop ()
  (let ([v (cfgl test-port)])
    (unless (eq? v 'EOF)
      (displayln (token-name v))
      (loop))))
  |#  
