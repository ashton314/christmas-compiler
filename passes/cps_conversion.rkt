#lang racket

(module+ test
  (require rackunit))

(require "ast.rkt")

(provide ast->cps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS Conversion                                        ;;
;;                                                       ;;
;; This module takes a chunk of AST and converts it into ;;
;; continuation-passing-style.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ast->cps ))
