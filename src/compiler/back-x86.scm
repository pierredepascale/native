;;; compiler.scm -- native compiler for Scheme like compiler

;;;
;; = Compiler toplevel

;; compile the resulting assembly with
;; gcc -m32 -o scm rt.c scheme.S entry_x86.S

(define (compile-code exp env)
  (let ((si (- 0 (* (+ 1 (length (env-local env))) $wordsize))))
;    (display ";; compiling code with env ") (display si) (newline)   
    (emit (compile exp env si %eax)
	  (x86-ret))))

(define (compile exp env si dst)
  (cond ((literal? exp) (compile-literal exp env si dst))
	((variable? exp) (compile-variable exp env si dst))
	((clambda? exp) (compile-lambda exp env si dst))
	((if? exp) (compile-if exp env si dst))
	((let? exp) (compile-let exp env si dst))
	((set!? exp) (compile-set! exp env si dst))
	((primitive-call? exp) (compile-primitive-call exp env si dst))
	((begin? exp) (compile-begin exp env si dst))
	((call? exp) (compile-call exp env si dst))
	(else (error "no compiler for expression ~a" exp))))

;;;
;; == Begin expression

(define (compile-begin exp env si dst)
  (let ((body (begin-body exp)))
    (emit
     (map (lambda (e) (compile e env si dst)) body))))

;;;
;; == Literal expressions

(define (literal? obj)
  (or (number? obj) (char? obj) (boolean? obj) (null? obj) 
      (unspecific-object? obj) (unbound-object? obj) (eof-object? obj)
      (string? obj)
      (and (pair? obj) (eq? 'quote (car obj)))))

(define (compile-literal exp env si dst)
  (if (literal-immediate? exp)
      (emit
       (x86-movl ($ (encode exp)) dst))
      (let ((depth (* $wordsize (+ 2 (lookup-lit-offset (literal-complex-value exp)
							(env-free env))))))
	(emit (x86-movl (^ depth %esi) dst)))))

;;;
;; == Conditional expressions

(define (if? obj) (and (pair? obj) (eq? 'if (car obj))))
(define if-test cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define (compile-if exp env si dst)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (emit
     (compile (if-test exp) env si %eax)
     (x86-cmp ($ (encode #f)) %al)
     (x86-je alt-label)
     (compile (if-consequent exp) env si dst)
     (x86-jmp/l end-label)
     (x86-label alt-label)
     (compile (if-alternative exp) env si dst)
     (x86-label end-label))))

;;;
;; == Primitives

(define-syntax define-primitive
  (syntax-rules ()
    ((define-primitive (?name ?arg ...) . ?body)
     (add-primitive! '?name (lambda (?arg ...) . ?body)))))

(define *primitives* '())

(define (add-primitive! name emitter)
  (let ((entry (assq name *primitives*)))
    (if entry
	(set-cdr! entry emitter)
	(set! *primitives* (cons (cons name emitter) *primitives*)))))

(define (primitive? name) (assq name *primitives*))

(define (primitive-call? exp) (and (pair? exp) (primitive? (car exp))))

(define (compile-primitive-call exp env si dst)
  (let ((entry (assq (car exp) *primitives*)))
    (if entry
	(apply (cdr entry) (cons env (cons si (cons dst (cdr exp)))))
	(error "primitive definition for ~a for found" exp))))

(define-primitive (%fx+1 env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-addl ($ (encode 1)) dst)))

(define-primitive (%fixnum->char env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-shll ($ (- $char-shift $fx-shift)) dst)
   (x86-orl ($ $char-tag) dst)))

(define-primitive (%fixnum? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-and ($ $fx-mask) %al)
   (x86-cmp ($ $fx-tag) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%fx-1+ env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-addl ($ (encode -1)) dst)))

(define-primitive (%null? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-cmp ($ (encode '())) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%not env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-cmp ($ (encode #f)) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%boolean? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-and ($ $bool-mask) %al)
   (x86-cmp ($ (encode #f)) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%char? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-and ($ $char-mask) %al)
   (x86-cmp ($ $char-tag) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%fx= env si dst arg1 arg2)
  (emit
   (compile arg1 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg2 env si %eax)
   (x86-cmpl %eax (^ si %esp))
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ (encode #f)) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define-primitive (%fxzero? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-cmpl ($ (encode 0)) %eax)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ (encode #f)) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))
  
(define-primitive (%fx< env si dst arg1 arg2)
  (emit
   (compile arg1 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg2 env si %eax)
   (x86-cmpl %eax (^ si %esp))
   (x86-setl %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ (encode #f)) %al)
   (if (not (eq? dst %eax))
       (emit "    movl %eax, " dst)
       (list))))

(define-primitive (%fx+ env si dst arg1 arg2)
  (emit
   (compile arg1 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg2 env (- si $wordsize) dst)
   (x86-addl (^ si %esp) dst)))

(define-primitive (%fx- env si dst arg1 arg2)
  (emit
   (compile arg2 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg1 env (- si $wordsize) dst)
   (x86-subl (^ si %esp) dst)))

(define-primitive (%box env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-movl %eax (^ $wordsize %ebp))
   (x86-movl ($ (+ 8 $vector-tag)) (^ 0 %ebp))
   (x86-movl %ebp dst)
   (x86-addl ($ $ptr-tag) dst)
   (x86-addl ($ (* 2 $wordsize)) %ebp)))
  
(define-primitive (%box-ref env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-movl (^ (- $wordsize $ptr-tag) dst) dst)))

(define-primitive (%box-set! env si dst arg1 arg2)
  (emit
   (compile arg2 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg1 env (- si $wordsize) %eax)
   (x86-movl (^ si %esp) %ebx)
   (x86-movl %ebx (^ (- $wordsize $ptr-tag) %eax))))
  
(define-primitive (%vector env si dst arg)
  (error "unimplemented %vector"))

(define-primitive (%vector-ref env si dst arg1 arg2)
  (error "unimplemented %vector-ref"))

(define-primitive (%vector-set! env si dst arg1 arg2 arg3)
  (error "unimplemented %vector-set!"))

(define-primitive (%vector? env si dst arg)
  (tagged-pointer-predicate env si dst arg $vector-tag))

(define-primitive (%pair? env si dst arg)
  (tagged-pointer-predicate env si dst arg $pair-tag))

(define-primitive (%symbol? env si dst arg)
  (tagged-pointer-predicate env si dst arg $symbol-tag))

(define-primitive (%cons env si dst arg1 arg2)
  (emit
   (compile arg1 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg2 env (- si $wordsize) %eax)
   (x86-movl %eax (^ (* 2 $wordsize) %ebp))
   (x86-movl (^ si %esp) %eax)
   (x86-movl %eax (^ $wordsize %ebp))
   (x86-movl ($ $pair-tag) (^ 0 %ebp))
   (x86-movl %ebp dst)
   (x86-addl ($ $ptr-tag) dst)
   (x86-addl ($ (* 3 $wordsize)) %ebp)))

(define-primitive (%car env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-movl (^ (- $wordsize $ptr-tag) dst) dst)))


(define-primitive (%cdr env si dst arg)
  (emit
   (compile arg env si dst)
   (x86-movl (^ (- (* 2 $wordsize) $ptr-tag) dst) dst)))

(define-primitive (%set-car! env si dst arg1 arg2)
  (compile arg2 env si %eax)
  (x86-movl %eax (^ si %esp))
  (compile arg1 env (- si $wordsize) %eax)
  (x86-movl (^ si %esp) %ebx)
  (x86-movl %ebx (^ (- $wordsize $ptr-tag) %eax)))

(define-primitive (%set-cdr! env si dst arg1 arg2)
  (compile arg2 env si %eax)
  (x86-movl %eax (^ si %esp))
  (compile arg1 env (- si $wordsize) %eax)
  (x86-movl (^ si %esp) %ebx)
  (x86-movl %ebx (^ (- (* 2 $wordsize) $ptr-tag) %eax)))

(define-primitive (%closure? env si dst arg)
  (emit
   (compile arg env si %eax)
   (x86-and ($ $primary-mask) %al)
   (x86-cmp ($ $closure-tag) %al)
   (x86-sete %al)
   (x86-movzbl %al %eax)
   (x86-sal ($ $bool-bit) %al)
   (x86-or ($ $immediate-false) %al)
   (if (not (eq? dst %eax))
       (x86-movl %eax dst)
       (list))))

(define (tagged-pointer-predicate env si dst arg tag)
  (let ((end-label (unique-label))
	(ptr-label (unique-label)))
    (emit
     (compile arg env si %eax)
     (x86-movl %eax %ebx)
     (x86-and ($ $primary-mask) %al)
     (x86-cmp ($ $ptr-tag) %al)
     (x86-je ptr-label)
     (x86-movl ($ $immediate-false) dst)
     (x86-jmp end-label)
     ptr-label
     (x86-movl %ebx %eax)
     (x86-movl (^ (- 0 $ptr-tag) %eax) %eax)
     (x86-and ($ $secondary-mask) %al)
     (x86-cmp ($ tag) %al)
     (x86-sete %al)
     (x86-movzbl %al %eax)
     (x86-sal ($ $bool-bit) %al)
     (x86-or ($ $immediate-false) %al)
     (if (not (eq? dst %eax))
	 (x86-movl %eax dst)
	 (list))
     end-label)))

;;;
;; == Variables

(define variable? symbol?)

(define (make-free depth) (list 'free depth))
(define (free-binding? binding)
  (and (pair? binding) (eq? 'free (car binding))))
(define free-binding-depth cadr)

(define (make-local depth) (list 'local depth))
(define (local-binding? binding)
  (and (pair? binding) (eq? 'local (car binding))))
(define local-binding-depth cadr)

(define (make-global depth) (list 'global depth))
(define (global-binding? binding)
  (and (pair? binding) (eq? 'global (car binding))))
(define global-binding-depth cadr)

(define (lookup-free-variable var env)
  (let find ((env env)
	     (depth 0))
    (if (null? env)
	#f
	(let ((entry (car env)))
	  (cond ((and (eq? (car entry) 'var)
		      (eq? (cadr entry) var))
		 (make-free depth))
		((and (eq? (car entry) 'global)
		      (eq? (cadr entry) var))
		 (make-global depth))
		(else (find (cdr env) (+ depth 1))))))))

(define (lookup-local-variable var env)
  (let ((depth (lookup-local-variable-depth var env)))
    (and depth (make-local depth))))

(define (lookup-local-variable-depth var env)
  (if (null? env)
      #f
      (let ((d (lookup-local-variable-depth var (cdr env))))
	  (cond (d (+ d 1))
		((eq? var (car env)) 0)
		(else d)))))

(define (lookup-variable var env)
  (or (lookup-local-variable var (env-local env))
      (lookup-free-variable var (env-free env))))

(define (compile-variable exp env si dst)
  (let ((binding (lookup-variable exp env)))
    (if binding
	(cond ((local-binding? binding)
	       (compile-local-variable binding si dst))
	      ((free-binding? binding)
	       (compile-free-variable binding si dst))
	      ((global-binding? binding)
	       (compile-global-variable binding si dst))
	      (else (error "unknown binding kind ~a" binding)))
	(error "couldn't find a binding for var ~a " exp))))

(define (compile-local-variable binding si dst)
  (emit
   (x86-movl (^ (- 0 (* $wordsize (+ 1 (local-binding-depth binding)))) %esp)
	     dst)))

(define (compile-free-variable binding si dst)
  (emit
   (x86-movl (^ (* $wordsize (+ 2 (free-binding-depth binding))) %esi) dst)))

(define (compile-global-variable binding si dst)
  (let ((end-label (unique-label)))
    (emit
     (x86-movl (^ (* $wordsize (+ 2 (global-binding-depth binding))) %esi) dst)
     (x86-movl (^ (- (* 2 $wordsize) $ptr-tag) dst) dst)
     (x86-cmpl ($ $immediate-unbound) dst)
     (x86-je end-label)
     end-label)))
;;;
;; == Set Expression
(define (compile-set! exp env si dst)
  (let ((binding (lookup-variable (set!-variable exp) env)))
    (if (and binding (global-binding? binding))
	(emit
	 (compile (set!-value exp) env si %eax)
	 (x86-movl (^ (* $wordsize (+ 2 (global-binding-depth binding))) %esi)
		   %ebx)
	 (x86-movl %eax (^ (- (* 2 $wordsize) $ptr-tag) %ebx)))
	(error "internal error: set! expression not setting a global variable ~a" exp))))

;;;
;; == Let expression

(define (let? exp) (and (pair? exp) (eq? 'let (car exp))))
(define let-bindings cadr)
(define let-body caddr)
(define binding-expression cadr)
(define binding-name car)

(define (bind-local-variables names env)
  (make-environment (env-free env)
		    (append (env-local env) names)))

(define (compile-let exp env si dst)
  (let* ((bindings (let-bindings exp))
	 (inits (map binding-expression bindings))
	 (names (map binding-name bindings)))
    (emit
     (compile-inits inits env si)
     (compile (let-body exp) (bind-local-variables names env)
	      (- si (* $wordsize (length names)))
	      dst))))

(define (compile-inits inits env si)
  (if (null? inits)
      inits
      (let ((init (car inits)))
	(emit
	 (compile init env si %eax)
	 (emit-stack-save si)
	 (compile-inits (cdr inits) env (- si $wordsize))))))

;;;
;; == Lambda

(define (make-environment free locals) (cons free locals))
(define (env-local env) (cdr env))
(define (env-free env) (car env))

(define (clambda? exp) (and (pair? exp) (eq? 'lambda (car exp))))
(define clambda-formals cadr)
(define clambda-body caddr)

(define (compile-lambda-free frees env si offset)
  (if (null? frees)
      '()
      (let ((free (car frees)))
	(emit
	 (cond ((eq? (car free) 'lit)
		(compile-lambda-free-lit free env si offset))
	       ((eq? (car free) 'var)
		(compile-lambda-free-var free env si offset))
	       ((eq? (car free) 'lambda)
		(compile-lambda-free-lambda free env si offset))
	       ((eq? (car free) 'global)
		(compile-lambda-free-var free env si offset))
	       (else (error "unknown closed over variable ~a" free)))
	 (compile-lambda-free (cdr frees) env si (+ offset $wordsize))))))

(define (compile-lambda-free-lit lit env si offset)
  (let ((depth (lookup-lit-offset lit (env-free env))))
    (emit (x86-movl (^ (* (+ 2 depth) $wordsize) %esi) %eax)
	  (x86-movl %eax (^ offset %ebp)))))

(define (lookup-lit-offset exp env)
  (if (null? env)
      (error "literal not found in env ~a" exp)
      (let ((entry (car env)))
	(if (and (eq? (car entry) 'lit)
		 (eq? (cadr entry) exp))
	    0
	    (+ 1 (lookup-lit-offset exp (cdr env)))))))

(define (compile-lambda-free-var var env si offset)
  (emit (compile-variable (cadr var) env si %eax)
	(x86-movl %eax (^ offset %ebp))))

(define (compile-lambda-free-lambda lam env si offset)
  (let ((depth (lookup-lambda-offset (cadr lam) (env-free env))))
    (emit (x86-movl (^ (* $wordsize (+ 2 depth)) %esi) %eax)
	  (x86-movl %eax (^ offset %ebp)))))

(define (lookup-lambda-offset exp env)
  (if (null? env)
      (error "lambda code not found in env ~a" exp)
      (let ((entry (car env)))
	(if (and (eq? (car entry) 'lambda)
		 (eq? (cadr entry) exp))
	    0
	    (+ 1 (lookup-lambda-offset exp (cdr env)))))))


(define (header len code) (+ (* len 4) code))

(define (compile-lambda exp env si dst)
  (let* ((free (free-variables exp '()))
	 (free-len (length free))
	 (depth (lookup-lambda-offset exp (env-free env))))
    (emit
     (x86-movl ($ (header (+ (length free) 2) 0)) (^ 0 %ebp))
     (x86-movl (^ (* $wordsize (+ depth 2)) %esi) %eax)
     (x86-addl ($ 7) %eax)
     (x86-movl %eax (^ $wordsize %ebp))
     (compile-lambda-free (free-variables exp (env-local env)) env si (* 2 $wordsize))
     (x86-movl %ebp dst)
     (x86-orl ($ $closure-tag) dst)
     (x86-addl ($ (* $wordsize (+ 2 free-len))) %ebp))))

;;;
;; == Letrec

;; (define (letrec? exp) (and (pair? exp) (eq? 'letrec (car exp))))
;; (define letrec-bindings cadr)
;; (define letrec-body caddr)

;; (define (compile-letrec exp)
;;   (let* ((bindings (letrec-bindings exp))
;; 	 (vars (map binding-name bindings))
;; 	 (lambdas (map binding-expression bindings))
;; 	 (labels (unique-labels vars))
;; 	 (env (make-initial-env vars labels)))
;;     (emit "    .text")
;;     (compile-lambdas lambdas labels env)
;;     (compile-function (letrec-body exp) env (- 0 $wordsize) "L_scheme_entry")))

;;;
;; ### Procedure call

(define call? pair?)
(define call-arguments cdr)
(define call-target car)

(define (compile-arguments args env si)
  (if (null? args)
      '()
      (emit
	(compile (car args) env si %eax)
	(x86-movl %eax (^ si %esp))
	(compile-arguments (cdr args) env (- si $wordsize)))))

(define (compile-call exp env si dst)
  (let* ((arguments (call-arguments exp))
	 (argument-count (length arguments))
	 (saved-closure-pointer-si si)
	 (return-address-si (- si $wordsize))
	 (args-si (- return-address-si $wordsize))
	 (call-label (unique-label)))
    (emit
     (x86-movl %esi (^ saved-closure-pointer-si %esp))
     (compile-arguments (call-arguments exp) env args-si)
     (compile (call-target exp) env si %esi)
     (x86-movl %esi %eax)
     (x86-andl ($ $primary-mask) %eax)
     (x86-cmp ($ $closure-tag) %al)
     (x86-je call-label)
     call-label
     (x86-movl ($ (encode argument-count)) %edx)
     (emit-adjust-base si)
     (x86-movl (^ (- $wordsize $closure-tag) %esi) %eax)
     (x86-addl ($ (- 0 $closure-tag)) %esi)
     (x86-call-*eax)
     (emit-adjust-base (- 0 si))
     (x86-movl (^ saved-closure-pointer-si %esp) %esi))))

;;;
;; == Tests procedure

(define (test-call)
  (scmc '((lambda (a) () a) 123)))

(define (test-fib)
  (scmc '(let ((fib (%cons 1 2)))
	   (let ((a (%set-car! fib (lambda (fib* n) 
				     ()
				     (if (%fx< n 2)
					 n
					 (%fx+ ((%car fib*) fib* (%fx- n 1))
					       ((%car fib*) fib* (%fx- n 2))))))))
	     ((%car fib) fib 36)))))

(define (test-fib-opt)
  (scmc '(let ((fib (%cons 1 2)))
	   (let ((a (%set-car! fib (lambda (fib* n) 
				     ()
				     (if (%fx< n 2)
					 n
					 (%fx+ ((%car fib*) fib* (%fx-1+ n))
					       ((%car fib*) fib* (%fx-1+ (%fx-1+ n)))))))))
	     ((%car fib) fib 36)))))

(define (test exp)
  (let ((env (make-environment (free-variables exp '()) '())))
    (compile-code exp env)))

(define (toplevel-environment env)
  (map (lambda (e)
	 (cond ((eq? 'lit (car e)) (toplevel-environment-lit e))
	       ((eq? 'var (car e)) (toplevel-environment-var e))
	       ((eq? 'global (car e)) (toplevel-environment-global e))
	       ((eq? 'lambda (car e)) (toplevel-environment-lambda e))
	       (else (error "unknown free element ~a" e))))
       (env-free env)))

(define (toplevel-environment-lit e) (cadr e))
(define (toplevel-environment-var e) (cons (cadr e) #f))
(define (toplevel-environment-global e) (make-ref (cadr e) #f))
(define (toplevel-environment-lambda e)
  (let* ((exp (cadr e))
	 (args (clambda-formals exp))
	 (body (clambda-body exp)))
    (assemble (compile-code body (make-environment (free-variables exp '()) args)))))

(define (toplevel-compile-env env)
  (map (lambda (e)
	 (if (eq? 'var (car e))
	     (list 'global (cadr e))
	     e))
       env))

(define (compile-environment free env)
  (map (lambda (e) e)
       free))

(define (scmc exp)
  (let ((stdout (current-output-port)))
    (with-output-to-file "code.fasl"
      (lambda ()
        (let* ((exp (front exp))
	       (env (make-environment (free-variables exp '()) '()))
	       (template (assemble (compile-code exp env)))
	       (closure (make-closure template
				      (toplevel-environment env))))
;          (display ";; " stdout) (write closure stdout) (newline stdout)
          (write-fasl closure (current-output-port)))))))

(define (read-test-from-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let ((exp (read))
            (expected (read)))
        (cons exp expected)))))

(define (compile-file file-name)
  (let ((exp (read-test-from-file file-name)))
    (scmc exp)))

(define (main args)
  (for-each compile-file args))

(define (test-exp description exp expected)
  (scmc exp)
  (let ((result (run/string (linux32 "../runtime/rt" "code.fasl"))))
    (if (string=? result expected)
        (begin
          (display "  [OK!] ") (display description) (newline))
        (begin
          (display "  [ERR] ") (display description) 
	  (display ", expected ") (write expected)
	  (display ", got ") (write result) (newline)))))

(define (run-test-suite)
  (let ((files (directory-files "../../test")))
    (for-each (lambda (fn)
		(if (string=? ".sexp" (file-name-extension fn))
		    (run-tests-in-file fn)
		    #f))	      
	      files)))

(define (run-tests-in-file fn)
  (display "Running test from ") (display fn) (newline)
  (call-with-input-file (string-append "../../test/" fn)
    (lambda (p)
      (let lp ((test (read-test-from-port p)))
	(if (eof-object? test)
	    (newline)
	    (begin
	      (test-exp (car test) (cadr test) (caddr test))
	      (lp (read-test-from-port p))))))))

(define (read-test-from-port port) (read port))

;;; FASL support

(define $fasl/template 0)
(define $fasl/ref 1)
(define $fasl/number 2)
(define $fasl/closure 3)
(define $fasl/symbol 4)
(define $fasl/string 5)
(define $fasl/null 6)
(define $fasl/unbound 7)
(define $fasl/unspecific 8)
(define $fasl/vector 9)
(define $fasl/true 10)
(define $fasl/false 11)
(define $fasl/pair 12)
(define $fasl/char 13)

(define (write-fasl code port)
  ;(debug 'write-fasl code)
  (cond ((template? code) (write-fasl-template code port))
        ((ref? code) (write-fasl-ref code port))
        ((number? code) (write-fasl-number code port))
        ((closure? code) (write-fasl-closure code port))
        ((symbol? code) (write-fasl-symbol code port))
	((string? code) (write-fasl-string code port))
	((char? code) (write-fasl-char code port))
	((null? code) (write-fasl-null code port))
	((eq? code #t) (write-fasl-boolean code port))
	((eq? code #f) (write-fasl-boolean code port))
	((unbound-object? code) (write-fasl-unbound code port))
	((unspecific-object? code) (write-fasl-unspecific code port))
	((vector? code) (write-fasl-vector code port))
	((pair? code) (write-fasl-pair code port))
        (else (error "don't know how to write fasl ~a" code))))

(define (write-fasl-boolean code port)
  (write-u8 port (if code $fasl/true $fasl/false)))

(define (write-fasl-template code port)
  (write-u8 port $fasl/template)
  (write-u32 port (template-length code))
  (let lp ((i 0))
    (if (< i (template-length code))
        (let ((code (template-ref code i)))
          ;(debug "write-fasl-template " code)
          (write-u8 port code)
          (lp (+ i 1))))))

(define (write-fasl-ref code port)
  (write-u8 port $fasl/ref)
  (write-fasl-symbol (ref-name code) port))

(define (write-fasl-closure code port)
  (write-u8 port $fasl/closure)
  (write-u32 port (closure-length code))
  (let lp ((i 0))
    (if (< i (closure-length code))
        (let ((code (closure-ref code i)))
          ;(debug 'write-fasl-closure code)
          (write-fasl code port)
          (lp (+ i 1))))))

(define (write-fasl-symbol code port)
  (write-u8 port $fasl/symbol)
  (let ((sym (symbol->string code)))
    (write-u32 port (string-length sym))
    (let lp ((i 0))
      (if (< i (string-length sym))
          (let ((ch (string-ref sym i)))
            ;(debug "write-fasl-symbol " ch)
            (write-u8 port (char->ascii ch))
            (lp (+ i 1)))))))

(define (write-fasl-number code port)
  (write-u8 port $fasl/number)
  (if (and (< code 128)
           (> code -128))
      (write-u8 port (+ 128 code))
      (error "write too big number ~a" code)))

(define (write-fasl-string str port)
  (write-u8 port $fasl/string)
  (write-u32 port (string-length str))
  (let lp ((i 0))
    (if (< i (string-length str))
	(let ((ch (string-ref str i)))
	  (write-u8 port (char->ascii ch))
	  (lp (+ i 1))))))

(define (write-fasl-char char port)
  (write-u8 port $fasl/char)
  (write-u8 port (char->ascii char)))

(define (write-fasl-null null port)
  (write-u8 port $fasl/null))

(define (write-fasl-unbound unbound port)
  (write-u8 port $fasl/unbound))

(define (write-fasl-unspecific unspecific port)
  (write-u8 port $fasl/unspecific))

(define (write-fasl-vector vec port)
  (write-u8 port $fasl/vector)
  (write-u32 port (vector-length vec))
  (let lp ((i 0))
    (if (< i (vector-length vec))
	(let ((obj (vector-ref vec i)))
	  (write-fasl obj port)
	  (lp (+ i 1))))))

(define (write-fasl-pair pair port)
  (write-u8 port $fasl/pair)
  (write-fasl (car pair) port)
  (write-fasl (cdr pair) port))

(define (u8->char u8) (integer->char (+ u8 (+ -32 (char->integer #\space)))))

(define (make-template size) (vector 'template (make-vector size 0)))
(define (make-template* code) (vector 'template code))
(define (template? obj) (and (vector? obj) (eq? 'template (vector-ref obj 0))))
(define (template-length t) (vector-length (vector-ref t 1)))
(define (template-set! t i v) (vector-set! (vector-ref t 1) i v))
(define (template-ref t i) (vector-ref (vector-ref t 1) i))

(define (make-closure template env)
  (list->vector (cons 'closure (cons template env))))
(define (closure? obj) (and (vector? obj) (eq? 'closure (vector-ref obj 0))))
(define (closure-length closure) (- (vector-length closure) 1))
(define (closure-ref closure offset) (vector-ref closure (+ offset 1)))

(define (make-ref name value) (vector 'ref name value))
(define (ref? obj) (and (vector? obj) (eq? 'ref (vector-ref obj 0))))
(define (ref-name obj) (and (ref? obj) (vector-ref obj 1)))

(define (write-u8 port byte) (write-char (u8->char byte) port))
(define (write-u32 port word)
  (write-u8 port 0)
  (write-u8 port 0)
  (write-u8 port 0)
  (write-u8 port word))
