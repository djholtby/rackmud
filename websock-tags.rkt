#lang racket/base

(require telnet/mxp)
(provide (struct-out mxp-tag:websock))

(struct mxp-tag:websock mxp-tag (html-equiv))