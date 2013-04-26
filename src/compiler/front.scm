;;; front.scm -- front end of the Scheme compiler

(define (front exp)
  (assignment-convert exp (setted-variables exp)))

(define (set!? exp) (and (pair? exp) (eq? 'set! (car exp))))
(define set!-variable cadr)
(define set!-value caddr)

(define (begin? exp) (and (pair? exp) (eq? 'begin (car exp))))
(define begin-body cdr)

;;;
;; == Assignment convertion 
;;
;; Assignment convertion removes `set!` expression from the language.
;; The trick is to insert box around setted variables.

(define (assignment-convert exp env)
  (cond ((literal? exp) (assignment-convert-literal exp env))
        ((variable? exp) (assignment-convert-variable exp env))
        ((if? exp) (assignment-convert-if exp env))
        ((set!? exp) (assignment-convert-set! exp env))
        ((let? exp) (assignment-convert-let exp env))
        ((clambda? exp) (assignment-convert-lambda exp env))
        ((primitive-call? exp) (assignment-convert-primitive-call exp env))
        ((call? exp) (assignment-convert-call exp env))
        (else (error "unknown expression ~a" exp))))

(define (assignment-convert-literal exp env)
  exp)

(define (assignment-convert-variable exp env)
  (if (set-member? exp env)
      (list '%vector-ref exp 0)
      exp))

(define (assignment-convert-if exp env)
  (list 'if (assignment-convert (if-test exp) env)
        (assignment-convert (if-consequent exp) env)
        (assignment-convert (if-alternative exp) env)))

(define (assignment-convert-primitive-call exp env)
  (let ((args (map (lambda (e) (assignment-convert e env)) (cdr exp))))
    (cons (car exp) args)))

(define (assignment-convert-call exp env)
  (map (lambda (e) (assignment-convert e env)) exp))

(define (assignment-convert-set! exp env)
  (list '%vector-set! (set!-variable exp) 0
        (assignment-convert (set!-value exp) env)))

(define (assignment-convert-let exp env)
  (let ((bindings (let-bindings exp)))
    (list 'let (map (lambda (b)
                      (list (binding-name b)
                            (assignment-convert (binding-expression b) env)))
                    bindings)
          (assignment-convert (let-body exp) env))))

(define (assignment-convert-lambda exp env)
  (list 'lambda (clambda-formals exp)
        (set->list (set-difference (free-variables exp)
                                   (clambda-formals exp)))
        (assignment-convert (clambda-body exp) env)))

;;;
;; == Setted variables
;;
;; Given a Scheme expression, we compute the set of all variables that are
;; assigned to with the `set!` operator.

;;;
;; Compute the set of variable that are assigned to in `exp`.
(define (setted-variables exp)
  (cond ((literal? exp) (setted-variables-literal exp))
        ((variable? exp) (setted-variables-variable exp))
        ((if? exp) (setted-variables-if exp))
        ((set!? exp) (setted-variables-set! exp))
        ((let? exp) (setted-variables-let exp))
        ((clambda? exp) (setted-variables-lambda exp))
        ((primitive-call? exp) (setted-variables-primitive-call exp))
        ((call? exp) (setted-variables-call exp))
        (else (error "free variables unknwon expression ~a" exp))))

(define (setted-variables* exp)
  (if (null? exp)
      (set-empty)
      (set-union (setted-variables (car exp))
                 (setted-variables* (cdr exp)))))

(define (setted-variables-literal exp)
  (set-empty))

(define (setted-variables-variable exp)
  (set-empty))

(define (setted-variables-if exp)
  (set-union (if-test exp)
             (set-union (if-consequent exp)
                        (if-alternative exp))))

(define (setted-variables-primitive-call exp)
  (setted-variables* exp))

(define (setted-variables-call exp)
  (setted-variables* exp))

(define (setted-variables-set! exp)
  (set-singleton (set!-variable exp)))

(define (setted-variables-let exp)
  (let ((bindings (let-bindings exp)))
    (setted-variables* (cons (let-body exp) (map bindings-init bindings)))))

(define (setted-variables-lambda exp)
  (setted-variables (clambda-body exp)))

;;;
;; ## Free variable

(define (free-variables exp)
  (cond ((literal? exp) (free-variables-literal exp))
        ((variable? exp) (free-variables-variable exp))
        ((if? exp) (free-variables-if exp))
        ((let? exp) (free-variables-let exp))
        ((clambda? exp) (free-variables-lambda exp))
        ((primitive-call? exp) (free-variables-primitive-call exp))
        ((call? exp) (free-variables-call exp))
        (else (error "free variables unknwon expression ~a" exp))))

(define (free-variables* exps)
  (if (null? exps)
      (set-empty)
      (set-union (free-variables (car exps))
                 (free-variables* (cdr exps)))))

;;;
;; ### Free variables in literals

(define (literal-immediate? exp)
  (or (number? exp)
      (char? exp)
      (boolean? exp)
      (null? exp)
      (unspecific-object? exp)
      (unbound-object? exp)
      (eof-object? exp)))

(define (literal-complex-value exp)
  (if (string? exp)
      exp
      (cadr exp)))

(define (free-variables-literal exp)
  (if (literal-immediate? exp)
      (set-empty)
      (set-singleton (list 'lit (literal-complex-value exp)))))

;;;
;; ### Free variable in variable

(define (free-variables-variable exp)
  (set-singleton (list 'var exp)))

;;;
;; ### Free variable in conditional

(define (free-variables-if exp)
  (set-union (free-variables (if-test exp))
             (set-union (free-variables (if-consequent exp))
                        (free-variables (if-alternative exp)))))

;;;
;; ### Free variable in primitive call

(define (free-variables-primitive-call exp)
  (free-variables* (cdr exp)))

;;;
;; ### Free variable in calling procedures

(define (free-variables-call exp)
  (free-variables* exp))

;;;
;; ### Free variable in let binding

(define (free-variables-let exp)
  (let* ((bindings (let-bindings exp))
         (vars (map car bindings))
         (inits (map cadr bindings)))
    (set-union (free-variables* inits)
               (set-difference (free-variables (let-body exp))
                               (list->set vars)))))

;;;
;; ### Free variable in lambda expressions

(define (free-variables-lambda exp)
  (set-singleton (list 'lambda exp)))

;;;
;; == Set data structure
;;
;; A Set is datastructure where elements appears only once (this is
;; not the case for a list). Elements are compared with the `eq?`
;; predicate. Internally a set is represented as a list of elements.

;;;
;; Returns an empty set
(define (set-empty) '())

;;;
;; Returns the set union of `s1` and `s2`
(define (set-union s1 s2)
  (if (null? s1)
      s2
      (let ((e1 (car s1)))
        (if (member e1 s2)
            (set-union (cdr s1) s2)
            (cons e1 (set-union (cdr s1) s2))))))

;;;
;; Returns the difference of two sets. It computes the set whose elements
;; are come from `s1` but not in `s2`.
(define (set-difference s1 s2)
  (if (null? s1)
      s1
      (let ((e1 (car s1)))
        (if (member e1 s2)
            (set-difference (cdr s1) s2)
            (cons e1 (set-difference (cdr s1) s2))))))

;;;
;; Returns a set containing only `e`
(define (set-singleton e)
  (list e))

;;;
;; Predicate for testing if `element` is in `set`.
(define (set-member? element set)
  (member element set))

;;;
;; Returns a list of all the element of `set`.
(define (set->list set)
  (append set '()))

;;;
;; Converts a list of elements to a set
(define (list->set list)
  (if (null? list)
      (set-empty)
      (let ((e (car list))
            (set (list->set (cdr list))))
        (if (member e set)
            set
            (cons e set)))))

;;;
;; == literal handling

(define (literal-convert exp)
  (cond ((literal? exp) (literal-convert-literal exp))
	((variable? exp) (literal-convert-variable exp))
	((if? exp) (literal-convert-if exp))
	((let? exp) (literal-convert-let exp))
	((begin? exp) (literal-convert-begin exp))
	((clambda? exp) (literal-convert-lambda exp))
	((primitive-call? exp) (literal-convert-primitive-call exp))
	((call? exp) (literal-convert-call exp))
	(else (error "unknown expression ~a" exp))))

(define (literal-convert-literal exp)
  exp)

(define (literal-convert-variable exp)
  exp)

(define (literal-convert-if exp)
  (list 'if (literal-convert (if-test exp))
	(literal-convert (if-consequent exp))
	(literal-convert (if-alternative exp))))

(define (literal-convert-let exp)
  (list 'let (map (lambda (b) (cons (binding-name b)
				    (literal-convert (binding-expression b))))
		  (let-bindings exp))
	(literal-convert (let-body exp))))

(define (literal-convert-begin exp)
  (cons 'begin
	(map literal-convert (begin-body exp))))

(define (literal-convert-lambda exp) exp)

(define (literal-convert-primitive-call exp) exp)

(define (literal-convert-call exp) exp)
