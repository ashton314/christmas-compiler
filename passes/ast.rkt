#lang racket

(require syntax/parse/define)
(require racket/pretty)

(provide (all-defined-out))

;; TODO: add line numbers to the ast-node data
(struct ast-node ([type #:auto])
  #:auto-value 'bottom
  #:mutable #:transparent)

;; Literal values like 42, 2.345, etc.
(struct litteral ast-node (value)
  #:mutable #:transparent)

;; Quoted values
(struct quoted-atom ast-node (name)
  #:mutable #:transparent)
(struct quoted-list ast-node (lst)
  #:mutable #:transparent)

;; Primitively-defined operations; math operators fall under this category
(struct primitive-op ast-node (name arity arguments)
  #:mutable #:transparent)

;; Names of variables
(struct var-name ast-node (name)
  #:mutable #:transparent)

;; Explicit annotation of a type
(struct type-annotation ast-node (expr)
  #:mutable #:transparent)

;; Labels definition
(struct labels-dec ast-node (definitions body)
  #:mutable #:transparent)
(struct label-definition ast-node (name arguments free-vars body)
  #:mutable #:transparent)

;; Lambda (used in the initial passes; eventually this should get hoisted)
(struct lambda-dec ast-node (arguments free-vars body)
  #:mutable #:transparent)

;; Function definitions
(struct func-def ast-node (label params body)
  #:mutable #:transparent)

;; Closure
(struct closure ast-node (label bindings)
  #:mutable #:transparent)

;; Let; we might compile these into lambdas later
(struct let-dec ast-node (definitions body)
  #:mutable #:transparent)
(struct let-definition ast-node (var-name expr)
  #:mutable #:transparent)

;; General application node; we'll refine through the passes
(struct application ast-node (func-ref args)
  #:mutable #:transparent)

;; Call to a continuation: we distinguish from "regular" function
;; calls because this is essentially just a JUMP command
(struct kapp ast-node (k-ref arg)
  #:mutable #:transparent)

;; Conditional node
(struct if-dec ast-node (condition true-case false-case)
  #:mutable #:transparent)

