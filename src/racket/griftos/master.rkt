;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname master) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#lang racket
(require racket/unsafe/ops)
(require racket/serialize)
(require racket/undefined)
(require "racket-mud.rkt")

(define-mud-class master-object% mud-object% players ...)

