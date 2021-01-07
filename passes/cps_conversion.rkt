#lang racket

(module+ test
  (require rackunit))

(require racket/pretty)
(require "ast.rkt")
(require "parser.rkt")

(provide ast->cps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS Conversion                                                       ;;
;;                                                                      ;;
;; This module takes a chunk of AST and converts it into                ;;
;; continuation-passing-style.                                          ;;
;;                                                                      ;;
;; The algorithm used here is a very simple, "dumb" algorithm that      ;;
;; ends up producing a lot of annoying redexes. Eventually, I'd like    ;;
;; to make this use the "no-brainer" CPS conversion proposed by Davis   ;;
;; et al. (See M. Davis, W. Meehan, and O. Shivers, “No-brainer CPS     ;;
;; conversion (functional pearl),” Proc. ACM Program. Lang., vol. 1,    ;;
;; no. ICFP, pp. 1–25, Aug. 2017, doi: 10.1145/3110267.)                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Defines "simple" expressions; ones that need no CPS
(define (simple? expr)
  (match expr
    [(litteral _ _) #t]
    [(quoted-atom _ _) #t]
    [(quoted-list _ _) #t]
    [(primitive-op _ _ _ args) (foldl (λ (a b) (and a b)) #t (map simple? args))]
    [(var-name _ _) #t]                 ; I'm pretty sure variable refs are "simple"
    [_ #f]))

[module+ test
  (require "parser.rkt")
  (check-true (simple? (parse '(@+ 1 2))))
  (check-true (simple? (parse '(@+ 1 x))))
  (check-true (simple? (parse '(@+ 1 (@* 2 x)))))

  (check-false (simple? (parse '(lambda (x) (@+ x 1)))))]

;; primary CPS routine; dispatchs on the AST node type and returns AST
;; in CPS style
(define (ast->cps expr k₀)
  (match expr
    ;; The base case
    [(? simple? e) (kapp k₀ e)]

    ;; Just a lambda
    [(lambda-dec type args free-vars body)
     (let ([k-fresh (gensym 'k)])         ; The new continuation variable for the func
       (kapp k₀
             (lambda-dec
              (append args (list k-fresh))
              free-vars

              ;; FIXME: handle multi-bodied lambdas; I'll probably have to
              ;; chain the continuation or something

              (ast->cps (car body) k-fresh))))]

    ;; Application: function needs to be converted
    [(application _ func-ref args)
     #:when (not (simple? func-ref))
     (ast->cps func-ref
               (let ([k-fresh (gensym 'x₁)])
                 (lambda-dec
                  (list k-fresh)
                  '()
                  (ast->cps (application (var-name k-fresh) args) k₀))))]

    ;; Application: function has been converted; now convert args
    [(application _ func-ref args)
     #:when (simple? func-ref)
     
     (define (conv-args simple rst)
       (if (null? rst)
           (application func-ref (append (reverse simple) (list k₀)))
           (if (simple? (car rst))
               (conv-args (cons (car rst) simple) (cdr rst))
               ;; Ok, need to convert this argument
               (ast->cps
                (car rst)
                (let ([k-fresh (gensym 'x₂)])
                  (lambda-dec
                   (list k-fresh)
                   '()
                   (ast->cps
                    (application func-ref
                                 `(,@(reverse simple) ,(var-name k-fresh) ,@(cdr rst)))
                    k₀)))))))

     (conv-args '() args)]

    ;; Primitive op with non-simple arguments
    [(primitive-op _ op-name arity args)

     (define (conv-args simple rst)
       (if (null? rst)
           (primitive-op op-name arity (append (reverse simple) (list k₀)))
           (if (simple? (car rst))
               (conv-args (cons (car rst) simple) (cdr rst))

               ;; Ok, need to convert this argument
               (ast->cps
                (car rst)
                (let ([k-fresh (gensym 'x₂)])
                  (lambda-dec
                   (list k-fresh)
                   '()
                   (ast->cps
                    (primitive-op op-name arity
                                  `(,@(reverse simple) ,(var-name k-fresh) ,@(cdr rst)))
                    k₀)))))))

     (conv-args '() args)]

    ;; Conditional
    [(if-dec _ c-expr t-case f-case)
     #:when (simple? c-expr)
     (if-dec c-expr (ast->cps t-case k₀) (ast->cps f-case k₀))]

    [(if-dec _ c-expr t-case f-case)
     #:when (not (simple? c-expr))
     (ast->cps
      c-expr
      (let ([k-fresh (gensym 'x₃)])
        (lambda-dec (list k-fresh)
                    '()
                    (if-dec (var-name k-fresh)
                            (ast->cps t-case k₀)
                            (ast->cps f-case k₀)))))]


    [_ (error "Unable to match expression in CPS conversion" expr)]))

(define expr-foo (parse '(lambda (map f l) (if (@null? l) '() (@cons (f (@car l)) (map f (@cdr l)))))))

[module+ test
  ;; TODO
  ]
