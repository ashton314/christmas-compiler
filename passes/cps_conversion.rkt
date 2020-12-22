#lang racket

(module+ test
  (require rackunit))

(require "ast.rkt")

(provide ast->cps)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS Conversion                                                       ;;
;;                                                                      ;;
;; This module takes a chunk of AST and converts it into                ;;
;; continuation-passing-style.                                          ;;
;;                                                                      ;;
;; The algorithm used here is the "no-brainer" CPS conversion proposed  ;;
;; by Davis et al. (See M. Davis, W. Meehan, and O. Shivers,            ;;
;; “No-brainer CPS conversion (functional pearl),” Proc. ACM Program.   ;;
;; Lang., vol. 1, no. ICFP, pp. 1–25, Aug. 2017, doi: 10.1145/3110267.) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defines "simple" expressions; ones that need no CPS
(define (simple? expr)
  (match expr
    [(litteral _) #t]
    [(quoted-atom _) #t]
    [(quoted-list _ ) #t]
    [(primitive-op _ _ args) (foldl and #t (map simple? args))]
    
    [_ #f]))

;; primary CPS routine; dispatchs on the AST node type and returns AST
;; in CPS style
(define (ast->cps expr k₀)
  (match expr
    [(? simple? e) e]
    [_ (error "Unable to match expression in CPS conversion" expr)]
    ))
