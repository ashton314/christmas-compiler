#lang racket

(module+ test
  (require rackunit))

(require "ast.rkt")

(provide (parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the parser for the language. This is the first pass to run ;;
;; over the code, and from this point on the compiler will be working ;;
;; with the AST output of this program.                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a program represented as an S-Expression, return an AST using
;; the structs defined in ast.rkt
(define (parse program)
  (match program
    ;; Primitive operations
    [`(,(? prim-arity-0? op))
     (primitive-op op 0 'void)]

    [`(,(? prim-arity-1? op) ,arg)
     (primitive-op op 1 (parse arg))]

    [`(,(? prim-arity-2? op) ,arg1 ,arg2)
     (primitive-op op 2 (list (parse arg1) (parse arg2)))]

    ;; Variables
    [(? symbol? name) (var-name name)]

    ;; Quotes
    [`(quote ,(? list? lst)) (quoted-list lst)]
    [`(quote ,(? symbol? atom-name)) (quoted-atom atom-name)]

    ;; Literal values
    [(? exact-integer? num) (litteral num)]
    [(? inexact-real? num) (litteral num)]
    [(? string? num) (litteral num)]

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Tests                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[module+ test
  ;; Primitive operations
  (check-equal? (parse '(@+ 1 (@add1 2)))
                (primitive-op '@+ 2 (list (litteral 1) (primitive-op '@add1 1 (litteral 2)))))

  ;; Literals and quotes
  (check-equal? (parse '(quote foo)) (quoted-atom 'foo))
  (check-equal? (parse '(quote (1 2 3 4 5))) (quoted-list '(1 2 3 4 5)))]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions to define primitive operations. Primitives will have ;;
;; names starting with an `@` to more easily distinguish them from their ;;
;; higher-level variadic counterparts. Later we'll include a pass where  ;;
;; we change something like `(+ 1 2 3)` to `(@+ 1 (@+ 2 3))`.            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prim-arity-0? sym)
  (and
   (symbol? sym)
   (member sym '(@panic))))

(define (prim-arity-1? sym)
  (and
   (symbol? sym)
   (member sym '(@car @cdr @add1 @zero? @empty? @null? @not @debug @print))))

(define (prim-arity-2? sym)
  (and
   (symbol? sym)
   (member sym '(@+ @* @/ @- @cons @=))))
