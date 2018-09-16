#lang racket/base

(provide cmd-lexer)

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define cmd-lexer
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
    (cons 'num (string->number lexeme))]
   [punctuation (cons 'punc lexeme)]
   [(:or "of" "with" "at" "from" "into" "in" "on" "under" "over" "through" "across" "above" "beneth" "around" "along")
    (cons 'prep lexeme)]
   [(:or "the" "a" "an" "some" "any")
    (cons 'article lexeme)]
   [(:+ alphabetic)
    (cons 'word lexeme)]
   [(:+ (char-complement (:or whitespace punctuation)))
    (cons 'other lexeme)]
   ))

; command patterns are either a CFG or "chomp" which matches to EOL including all whitespace