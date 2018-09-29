#lang racket/base

(provide make-lexer tokenize)

(require racket/match)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define (make-lexer preps arts)
  (lexer
   [(:+ whitespace) (cons 'whitespace lexeme)]
   [(:: #\" (:* (char-complement #\")) #\")
    (cons 'quote lexeme)]
   [(:: (:? (:or #\+ #\-))
        (:+ numeric)
        (:?
         (:or
          (:: #\. (:+ numeric))
          (:: #\/ (:+ numeric)))))
    (cons 'num lexeme)]
   [punctuation (cons 'punc lexeme)]
   
   [(:+ alphabetic)
    (let ([lexeme/symbolic (string->symbol (string-foldcase lexeme))])
      (cond [(memq lexeme/symbolic preps)
             (cons 'prep lexeme)]
            [(memq lexeme/symbolic arts)
             (cons 'article lexeme)]
            [else
             (cons 'word lexeme)]))]
   [(:+ (char-complement (:or whitespace punctuation)))
    (cons 'other lexeme)]
   ))

(define (tokenize lexer input [ignore-whitespace #t])
  (define input-port (open-input-string input))
  (let loop ([acc '()])
    (define next-token (lexer input-port))
    (match next-token
      ['eof (reverse acc)]
      [(cons 'whitespace _) (loop (if ignore-whitespace acc (cons next-token acc)))]
      [else (loop (cons next-token acc))])))


;(define english-lexer
;  (make-lexer '(of with at from into in on under over through across above beneath around along) '(the a an some any)))

