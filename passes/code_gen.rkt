#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLVM IR Generation routines                                              ;;
;;                                                                          ;;
;; This module takes my AST and converts it into LLVM code.                 ;;
;;                                                                          ;;
;; You can see what IR clang produces by writing some C code, and then      ;;
;; running `clang -S -emit-llvm' on the C source. This will create a .ll    ;;
;; file that you can inspect. I have an Emacs mode I snarfed from an        ;;
;; archived repo that makes it more pleasant to work with the IR.           ;;
;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *prelude*
  "target triple = \"x86_64-apple-macosx10.15.0\"")
