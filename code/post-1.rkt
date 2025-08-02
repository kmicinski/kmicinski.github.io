#lang racket

(define relation-name? symbol?)

(define (query? q)
  (define (condition-size? R)
    #t) ;; xxx
  (match q
    ;; scan a relation
    [`(scan ,(? relation-name? R)) #t]
    [`(select from ,(? relation-name? R) where ,(? (condition-size? R))) #t]
    ;; natural join on the first N columns
    [`(,(? query? q0) ⋈ ,(? query? q1) on first ,(? nonnegative-integer? N)) #t]
    [`(,(? query? q0) ∪ ,(? query? q0)) #t]
    [`(,(? query? q0) ∩ ,(? query? q0)) #t]
    ;; reorder tuples
    [`(reorder ,(? query? q) ,(? nonnegative-integer?) ...) #t]
    ;; project the first n elements of tuples
    [`(project ,(? query? q) ,(? nonnegative-integer? n)) #t]
    ;; need closed-world assumption
    [`(- ,(? query? q)) #t]
    [_ #f]))

;; q: query?
;; database: hashmap from relation name ↦ ℘(Tuple)
;; universe: ℘(Tuple)
(define (interpret-query q database universe)
  (define (interpret-conditions conditions R)
    (match conditions
      ['() (hash-ref database R)]
      [`([= ,n ,v] . ,rst-conditions)
       (filter (lambda (tuple) (equal? (list-ref tuple n) v)) 
               (interpret-conditions rst-conditions))]))
  (match q
    [`(scan ,R) (hash-ref database R)]
    [`(select from ,R where ,condition)
     (filter (interpret-condition condition)
             (hash-ref database R))]
    [`(,q0 ∪ ,q1) (set-union (interpret-query q0) (interpret-query q1))]
    [`(,q0 ∩ ,q1) (set-intersection (interpret-query q0) (interpret-query q1))]
    [`(,q0 ⋈ ,N ,q1)
     (foldl
      (foldl ))]))
