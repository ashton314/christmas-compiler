#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Code Generation Routines                            ;;
;;                                                       ;;
;; This module takes my AST and converts it into C code. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *prelude*
  #<<__PRELUDE__
# Christmas Compiler output
#include <stdio.h>

__PRELUDE__
  )

