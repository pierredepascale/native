;;; front.scm -- front end of the Scheme compiler

(define (front exp)
  (let ((exp (desugar exp)))
    (assignment-convert exp (set-empty))))

;;;
;; desugar Scheme

(define (desugar exp)
  (cond ((literal? exp) (desugar-literal exp))
	((variable? exp) (desugar-variable exp))
	((if? exp) (desugar-if exp))
	((set!? exp) (desugar-set! exp))
	((let? exp) (desugar-let exp))
	((clambda? exp) (desugar-lambda exp))
	((primitive-call? exp) (desugar-primitive-call exp))
	((begin? exp) (desugar-begin exp))
	
	((and (pair? exp) (eq? 'let* (car exp))) (desugar-let* exp))
	((and (pair? exp) (eq? 'letrec (car exp))) (desugar-letrec exp))
	((and (pair? exp) (eq? 'when (car exp))) (desugar-when exp))
	((and (pair? exp) (eq? 'unless (car exp))) (desugar-unless exp))

	((call? exp) (desugar-call exp))

	(else (error "don't know how to desugar expression ~a" exp))))

(define (desugar-literal exp) exp)
(define (desugar-variable exp) exp)
(define (desugar-if exp)
  (cond ((= (length exp) 3)
	 (list 'if
	       (desugar (if-test exp))
	       (desugar (if-consequent exp))
	       (unspecific-object)))
	((= (length exp) 4)
	 (list 'if
	       (desugar (if-test exp))
	       (desugar (if-consequent exp))
	       (desugar (if-alternative exp))))
	(else (error "malformed if expression ~a" exp))))

(define (desugar-set! exp)
  (if (= (length exp) 3)
      (list 'set! (desugar (set!-variable exp)) (desugar (set!-value exp)))
      (error "malformed set! expression ~a" exp)))

(define (desugar-bindings bindings)
  (cond ((pair? bindings)
	 (let ((binding (car bindings)))
	   (cons
	    (list (car binding) (desugar (cadr binding)))
	    (desugar-bindings (cdr bindings)))))
	((null? bindings) bindings)
	(else (error "malformed bindings ~a" exp))))

(define (desugar-body exp)
  (cond ((null? exp) (unspecific-object))
	((null? (cdr exp)) (desugar (car exp)))
	((pair? exp) (cons 'begin (map desugar exp)))
	(error "malformed body ~a" exp)))

(define (desugar-let exp)
  (if (> (length exp) 2)
      `(let ,(desugar-bindings (let-bindings exp))
	 ,(desugar-body (cddr exp)))
      (error "Malformed let expression ~a" exp)))

(define (desugar-lambda exp)
  (if (> (length exp) 2)
      `(lambda ,(clambda-formals exp)
	 ,(desugar-body (cddr exp)))
      (error "malformed lambda expression ~a" exp)))

(define (desugar-begin exp)
  (cond ((null? (cdr exp)) (unspecific-object))
	((null? (cddr exp)) (desugar (cadr exp)))
	((pair? (cddr exp)) (cons 'begin (map desugar (cdr exp))))
	(else (error "malformed begin expression ~a" exp))))

(define (desugar-primitive-call exp)
  (cons (car exp) (map desugar (cdr exp))))

(define (desugar-call exp)
  (map desugar exp))

(define (desugar-let*-1 bindings body)
  (if (null? bindings)
      body
      (let ((binding (car bindings)))
	`(let ((,(car binding) ,(cadr binding)))
	   (desugar-let*-1 (cdr bindings) body)))))

(define (desugar-let* exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (desugar (desugar-let*-1 bindings body))))

(define (desugar-letrec exp)
  (let* ((bindings (cadr exp))
	 (vars (map car bindings))
	 (inits (map cadr bindings)))
    (desugar `(let ,(map (lambda (v) (list v (unspecific-object))) vars)
		,@(map (lambda (b) (list 'set! (car b) (cadr b))) bindings)
		,@(cddr exp)))))
		   
(define (desugar-when exp) 
  (desugar `(if ,(cadr exp) (begin ,@(cddr exp)))))
(define (desugar-unless exp)
  (desugar `(if (%not ,(cadr exp)) (begin ,@(cddr exp)))))

;;;

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
      (list '%box-ref exp)
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
  (let ((var (set!-variable exp))
	(exp (assignment-convert (set!-value exp) env)))
    (if (set-member? var env)
	(list '%box-set! var exp)
	(list 'set! var exp))))

(define (assignment-convert-let exp env)
  (let* ((bindings (let-bindings exp))
	 (setted-vars (setted-variables (let-body exp)))
	 (non-setted (set-difference (map car bindings) setted-vars)))
    (list 'let (map (lambda (b)
		      (let ((name (car b))
			    (init (cadr b)))
			(if (set-member? name setted-vars)
			    (list name (list '%box (assignment-convert init env)))
			    (list name (assignment-convert init env)))))
		    bindings)
          (assignment-convert (let-body exp) (set-difference (set-union env setted-vars)
							     non-setted)))))

(define (assignment-convert-lambda exp env)
  (let* ((formals (clambda-formals exp))
	 (body (clambda-body exp))
	 (body-setted (setted-variables body))
	 (non-setted (set-difference formals body-setted))
	 (formals-setted (set-difference formals non-setted))
	 (body* (assignment-convert body (set-union formals-setted
						    (set-difference env non-setted)))))
    (if (set-empty? formals-setted)
	(list 'lambda formals body*)
	(list 'lambda formals
	      (list 'let (map (lambda (v) (list v (list '%box v))) formals-setted)
		    body*)))))

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
  (set-union (setted-variables (if-test exp))
             (set-union (setted-variables (if-consequent exp))
                        (setted-variables (if-alternative exp)))))

(define (setted-variables-primitive-call exp)
  (setted-variables* (cdr exp)))

(define (setted-variables-call exp)
  (setted-variables* exp))

(define (setted-variables-set! exp)
  (set-singleton (set!-variable exp)))

(define (setted-variables-let exp)
  (let* ((bindings (let-bindings exp))
	 (inits (setted-variables* (map cadr bindings)))
	 (body (setted-variables (let-body exp))))
    (set-union inits
	       (set-difference body (map car bindings)))))

(define (setted-variables-lambda exp)
  (set-difference (clambda-formals exp)
		  (setted-variables (clambda-body exp))))

;;;
;; ## Free variable

(define (free-variables exp)
  (cond ((literal? exp) (free-variables-literal exp))
        ((variable? exp) (free-variables-variable exp))
        ((if? exp) (free-variables-if exp))
        ((let? exp) (free-variables-let exp))
        ((clambda? exp) (free-variables-lambda exp))
        ((primitive-call? exp) (free-variables-primitive-call exp))
	((begin? exp) (free-variables-begin exp))
	((set!? exp) (free-variables-set! exp))
        ((call? exp) (free-variables-call exp))
        (else (error "free variables unknwon expression ~a" exp))))

(define (free-variables* exps)
  (if (null? exps)
      (set-empty)
      (set-union (free-variables (car exps))
                 (free-variables* (cdr exps)))))

(define (free-variables-begin exp)
  (free-variables* (cdr exp)))

(define (free-variables-set! exp)
  (set-union (set-singleton (list 'var (set!-variable exp)))
	     (free-variables (set!-value exp))))

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
  (set-union (set-singleton (list 'lambda exp))
	     (set-difference (free-variables (clambda-body exp))
			     (list->set (clambda-formals exp)))))

;;;
;; == Set data structure
;;
;; A Set a is datastructure where elements appears only once (this is
;; not the case for a list). Elements are compared with the `eq?`
;; predicate. Internally a set is represented as a list of elements.

;;;
;; Returns an empty set
(define (set-empty) '())

;;;
;; Return #t if a set is empty (containing no elements)
(define (set-empty? set) (null? set))

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
