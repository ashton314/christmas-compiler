#lang racket

(module+ test
  (require rackunit))

(require "ast.rkt")

(provide parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the parser for the language. This is the first pass to run ;;
;; over the code, and from this point on the compiler will be working ;;
;; with the AST output of this program.                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a program represented as an S-Expression, return an AST using
;; the structs defined in ast.rkt
(define (parse program)
  (match program
    ;; let-blocks
    [`(let ([,(? symbol? var) ,val-expr] ...)
        ,body ...)
     (let-dec (map (lambda (var val) (let-definition var (parse val))) var val-expr)
              (map parse body))]

    ;; branches
    [`(if ,condition ,t-arm ,f-arm)
     (if-dec (parse condition) (parse t-arm) (parse f-arm))]

    ;; Lambda: body may be composed of several expessions; results of
    ;; the last will be returned
    [`(,(? lambda? _) (,params ...) ,body ...)
     ;; TODO: add a pass to extract free variables
     (lambda-dec params '() (map parse body))]

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

    ;; function application
    [`(,f-expr . ,args)
     ;; TODO: I might want to optimize inlined lambda applications
     (application (parse f-expr) (map parse args))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Tests                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[module+ test
  ;; Let blocks
  (check-equal?
   (parse '(let ((x (@+ 1 2))
                 (y (@* x 2)))
             (@- x y)))
   (let-dec (list (let-definition 'x (primitive-op '@+ 2 (list (litteral 1) (litteral 2))))
                  (let-definition 'y (primitive-op '@* 2 (list (var-name 'x) (litteral 2))))
                  )
            (list (primitive-op '@- 2 (list (var-name 'x) (var-name 'y))))))

  ;; Conditionals
  (check-equal?
   (parse '(if (@zero? x) (quote foo) (quote bar)))
   (if-dec (primitive-op '@zero? 1 (var-name 'x)) (quoted-atom 'foo) (quoted-atom 'bar)))

  ;; Funciton calls
  (check-equal?
   (parse '(foo 1 2))
   (application (var-name 'foo) (list (litteral 1) (litteral 2))))
  (check-equal?
   (parse '((lambda (x) (@+ x 1)) 42))
   (application (lambda-dec '(x) '() (list (primitive-op '@+ 2 (list (var-name 'x) (litteral 1)))))
                (list (litteral 42))))

  ;; Lambda arguments
  (check-equal? (parse '(lambda (x y z) (@+ y z) (@* x y)))
                (lambda-dec '(x y z) '()
                            (list
                             (primitive-op '@+ 2 (list (var-name 'y) (var-name 'z)))
                             (primitive-op '@* 2 (list (var-name 'x) (var-name 'y))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various other helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda? sym)
  (and (member sym '(lambda λ)) (symbol? sym)))
