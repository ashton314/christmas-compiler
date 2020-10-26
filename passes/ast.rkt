#lang racket

(require syntax/parse/define)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's rethink how we want to build new nodes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax def-ast-node
  (syntax-rules ()
    [(def-ast-node node-name (fields ...))
     (struct node-name
       (fields ... [type #:auto])
       #:auto-value 'bottom
       #:mutable #:transparent)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With those defined, let's build up our AST node types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Literal values like 42, 2.345, etc.
(def-ast-node litteral (value))

;; Quoted values
(def-ast-node quoted-atom (name))
(def-ast-node quoted-list (lst))

;; Primitively-defined operations; math operators fall under this category
(def-ast-node primitive-op (name arity arguments))

;; Names of variables
(def-ast-node var-name (name))

;; Explicit annotation of a type
(def-ast-node type-annotation (expr))

;; Labels definition
(def-ast-node labels-dec (definitions body))

(def-ast-node label-definition (name arguments free-vars body))

;; Lambda (used in the initial passes; eventually this should get hoisted)
(def-ast-node lambda-dec (arguments free-vars body))

;; Let; we might compile these into lambdas later
(def-ast-node let-dec (definitions body))

(def-ast-node let-definition (var-name expr))

;; General application node; we'll refine through the passes
(def-ast-node application (func-ref args))

;; Conditional node
(def-ast-node if-dec (condition true-case false-case))
