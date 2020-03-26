;   Copyright 2020 Paulo Matos
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.

#lang racket/base

(require xsmith
         racr
         xsmith/racr-convenience
         racket/pretty
         racket/random
         racket/list
         racket/class
         racket/string
         racket/port)

;; XSMITH-based fuzzer for R7RS - following Rattle's
;; support for R7RS
;; XSmith does not support grammar names with digits
;; so we'll call this just rattle

(define-spec-component rattle-core)

;;
;; GRAMMAR FOR RATTLE
;;
(add-to-grammar
 rattle-core
 [Program DefinitionContext ()]
 [Expression #f ()
             #:prop may-be-generated #f]

 [DefinitionContext #f ([definitions : Definition * = 0];(random 3)]
                        [expressions : Expression * = (add1 (random 10))])
                    #:prop strict-child-order? #t]
 [Definition #f ([type]
                 [name]
                 Expression)
             #:prop binder-info (name type definition)]
 [Let Expression ([definitions : Definition * = 0 #;(random 3)]
                  [body : DefinitionContext])
      #:prop strict-child-order? #t]

 [LiteralBool Expression ([v = (even? (random 2))])]
 [LiteralNumber Expression (v) #:prop may-be-generated #f]
 [LiteralInt LiteralNumber ()]

 [If Expression ([test : Expression] [then : Expression] [else : Expression])
     #:prop strict-child-order? #t]
 
 )

;;
;; RENDER NODE INFORMATION
;; Used to render the node to the output, in order words it
;; says how to print each AST node properly
;;

;; helper for render-node-info
(define (->se sym . children-refs)
  (λ (n) `(,sym ,@(map (λ (x) (render-node (ast-child x n)))
                       children-refs))))

(add-prop
 rattle-core
 render-node-info
 [Program (λ (n) `(,@(map (λ (x) (render-node x))
                          (append (ast-children (ast-child 'definitions n))
                                  (ast-children (ast-child 'expressions n))))))]
 [DefinitionContext (λ (n) `(,@(map (λ (x) (render-node x))
                                    (ast-children (ast-child 'definitions n)))
                             ,@(map (λ (x) (render-node x))
                                    (ast-children (ast-child 'expressions n)))))]
 [Definition (λ (n) `(define ,(string->symbol (ast-child 'name n))
                       ,(render-node (ast-child 'Expression n))))]
 [Let (lambda (n) `(let (,@(map (lambda (d) `(,(string->symbol (ast-child 'name d))
                                              ,(render-node (ast-child 'Expression d))))
                                (ast-children (ast-child 'definitions n))))
                     ,@(render-node (ast-child 'body n))))]
 [If (->se 'if 'test 'then 'else)]

 [LiteralBool (lambda (n) (ast-child 'v n))]
 [LiteralNumber (lambda (n) (ast-child 'v n))])

;;
;; HOLE RENDERING
;;
(add-prop
 rattle-core
 render-hole-info
 [#f (lambda (h) (list 'HOLE (ast-node-type h)))])

;;
;; FRESH VALUE GENERATION FOR NODES
;; How to generate new fresh values for each node that requires them
;;
(add-prop
 rattle-core
 fresh
 [LiteralInt (hash 'v (* (random 1000000)
                         (if (equal? 0 (random 2)) -1 1)))]

 [Definition (hash 'name (if (equal? (top-ancestor-node current-hole)
                                     (parent-node current-hole))
                             (fresh-var-name "global-")
                             (fresh-var-name "local-"))
                   'type (fresh-concrete-var-type))])

;;
;; TYPING INFORMATION
;; How each node is typed and how types are propagated through the tree
;;
(type-variable-subtype-default #t)
(define number (base-type 'number))
(define int (base-type 'int number))
(define bool (base-type 'bool))

(define (type-thunks-for-concretization)
  (list (lambda () int)
        (lambda () number)
        (lambda () bool)))

(define no-child-types (λ (n t) (hash)))
(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))

(add-prop
 rattle-core
 type-info
 [DefinitionContext [(fresh-type-variable)
                     (λ (n t)
                       (define last-expression
                         (car (reverse (ast-children
                                        (ast-child 'expressions n)))))
                       (hash last-expression t
                             'definitions (λ (c) (fresh-type-variable))
                             'expressions (λ (c) (fresh-type-variable))))]]
 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]

 [Let [(fresh-type-variable) (lambda (n t)
                               (hash 'body t
                                     'definitions (lambda (c) (fresh-type-variable))))]]
 [If [(fresh-type-variable)
      (lambda (n t)
        (hash 'test bool 'then t 'else t))]]
 
 [LiteralBool [bool (no-child-types)]]
 [LiteralInt [int (no-child-types)]])


;;
;; ASSEMBLING ALL THE INFORMATION
;; Is this necessary?
;;
(assemble-spec-components
 rattle
 rattle-core)

;;
;; PROGRAM GENERATION
;;
(define (rattle-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (rattle-generate-ast 'Program)))

;; WHOLE PROGRAM RENDERING
(define (rattle-format-render forms)
  (with-output-to-string
    (lambda ()
      (define (pp x)
        (pretty-print x (current-output-port) 1))
      (for ([form forms])
        (pp form)))))

;; COMMAND LINE
(module+ main
  (xsmith-command-line
   rattle-generate
   #:format-render rattle-format-render))

 
