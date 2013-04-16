;;; compatibility module for running the compiler under Scheme48

(define $unspecific (list 'unspecific-object))
(define (unspecific-object) $unspecific)
(define (unspecific-object? o) (eq? o $unspecific))

(define $unbound (list 'unbound-object))
(define (unbound-object) $unbound)
(define (unbound-object? o) (eq? o $unbound))
