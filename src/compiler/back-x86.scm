;; compiler.scm -- native compiler for Scheme like compiler

;;;
;; = Compiler toplevel

;; compile the resulting assembly with
;; gcc -m32 -o scm rt.c scheme.S entry_x86.S

(define (compile-code exp env)
  (let ((si (- 0 (* (+ 1 (length (env-local env))) $wordsize))))
    ;(display ";; compiling code with env ") (display exp) (display env) (display si) (newline)   
    (emit (compile exp env si %eax)
	  (x86-ret))))

(define (compile exp env si dst)
;  (display (list 'compile exp (env-local env) (env-free env))) (newline)
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
      (let ((depth (lookup-lit-offset (literal-complex-value exp)
				      (env-free env))))
	(emit-free-load depth dst))))

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
   (x86-movl (^ si %esp) %edx)
   (x86-movl %ebx (^ (- $wordsize $ptr-tag) %eax))))
  
(define-primitive (%vector env si dst arg)
  (error "unimplemented %vector"))

(define-primitive (%vector-ref env si dst arg1 arg2)
  (emit
   (compile arg2 env si %eax)
   (x86-movl %eax (^ si %esp))
   (compile arg1 env (- si $wordsize) %eax)
   (x86-addl ($ (- $wordsize $ptr-tag)) %eax)
   (x86-movl (^ si %esp) %edx)
   (x86-sar ($ $fx-shift) %edx)
   (x86-movl (^^ %eax %edx 0) dst)))

(define-primitive (%vector-set! env si dst arg1 arg2 arg3)
  (let ((t0 si)
	(t1 (- si $wordsize))
	(t2 (- si (* 2 $wordsize))))
    (emit
     (compile arg3 env si %eax)
     (x86-movl %eax (^ t0 %esp))
     (compile arg2 env t1 %eax)
     (x86-movl %eax (^ t1 %esp))
     (compile arg1 env t2 %eax)
     (x86-addl ($ (- $wordsize $ptr-tag)) %eax)
     (x86-movl (^ t1 %esp) %edx)
     (x86-sar ($ $fx-shift) %edx)
     (x86-movl (^ t0 %esp) %ecx)
     (x86-movl %ecx (^^ %eax %edx 0)))))

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

(define-primitive (%print env si dst arg)
  (let ((saved-context-ptr-si si)
	(saved-closure-ptr-si (- si $wordsize))
	(saved-alloc-ptr-si (- si (* 2 $wordsize))))
    (emit
     (compile arg env si %eax)
     (x86-movl %ecx (^ saved-context-ptr-si %esp))
     (x86-movl %esi (^ saved-closure-ptr-si %esp))
     (x86-movl %ebp (^ saved-alloc-ptr-si %esp))
     (emit-adjust-base (- si 12))
     (x86-pushl %eax)
     (x86-movl (^ 44 %ecx) %eax)
     (x86-call-*eax)
     (emit-adjust-base (- 0 (- si 16)))
     (x86-movl (^ saved-closure-ptr-si %esp) %esi)
     (x86-movl (^ saved-alloc-ptr-si %esp) %ebp)
     (x86-movl (^ saved-context-ptr-si %esp) %ecx))))

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
    (debug "compiling variable binding " binding env)
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
  (emit-stack-load (- 0 (+ 1 (local-binding-depth binding))) dst))

(define (compile-free-variable binding si dst)
  (emit-free-load (free-binding-depth binding) dst))

(define (compile-global-variable binding si dst)
  (let ((end-label (unique-label)))
    (debug "emiting global " binding)
    (emit
     (emit-free-load (global-binding-depth binding) dst)
     (x86-movl (^ (- (* 2 $wordsize) $ptr-tag) dst) dst)
     (x86-cmpl ($ $immediate-unbound) dst)
     (x86-je end-label)
     end-label)))
;;;
;; == Set Expression
(define (compile-set! exp env si dst)
  (let ((binding (lookup-variable (set!-variable exp) env)))
    ;(display ";; emiting set! for ") (display binding) (newline)
    (if (and binding (global-binding? binding))
	(emit
	 (compile (set!-value exp) env si %eax)
	 (emit-free-load (global-binding-depth binding) %ebx)
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
		(compile-lambda-free-global free env si offset))
	       (else (error "unknown closed over variable ~a" free)))
	 (compile-lambda-free (cdr frees) env si (+ offset $wordsize))))))

(define (compile-lambda-free-lit lit env si offset)
  (let ((depth (lookup-lit-offset lit (env-free env))))
    (emit
     (emit-free-load depth %eax)
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

(define (compile-lambda-free-global var env si offset)
  (let ((binding (lookup-variable (cadr var) env)))
    (if (and binding (global-binding? binding))
	(emit 
	 (emit-free-load (global-binding-depth binding) %eax)
	 (x86-movl %eax (^ offset %ebp)))
	(error "internal error: free global variable not a global ?!?" binding))))

(define (compile-lambda-free-lambda lam env si offset)
  (let ((depth (lookup-lambda-offset (cadr lam) (env-free env))))
    (emit (emit-free-load depth %eax)
	  (x86-movl %eax (^ offset %ebp)))))

(define (lookup-lambda-offset exp env)
  (if (null? env)
      (error "lambda code not found in env ~a" exp)
      (let ((entry (car env)))
	(if (and (eq? (car entry) 'lambda)
		 (eq? (cadr entry) exp))
	    0
	    (+ 1 (lookup-lambda-offset exp (cdr env)))))))

(define (lookup-lambda-free exp env)
  (if (null? env)
      (error "lambda code not found in env ~a" exp)
      (let ((entry (car env)))
	(if (and (eq? (car entry) 'lambda)
		 (eq? (cadr entry) exp))
	    (caddr entry)
	    (+ 1 (lookup-lambda-offset exp (cdr env)))))))


(define (compile-lambda exp env si dst)
  (let* ((free (lookup-lambda-free exp (env-free env)))
	 (free-len (length free))
	 (depth (lookup-lambda-offset exp (env-free env))))
    (debug 'lambda exp 'local (env-local env) 'free (env-free env) 'cfree free)
    (emit
     (x86-movl ($ (header (+ (length free) 2) 0)) (^ 0 %ebp))
     (emit-free-load depth %eax)
     (x86-addl ($ 7) %eax)
     (x86-movl %eax (^ $wordsize %ebp))
     (compile-lambda-free free env si (* 2 $wordsize))
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
	 (free (caddr e))
	 (args (cadr exp))
	 (body (caddr exp)))
    (assemble (compile-code body (make-environment free args)))))

(define (scmc exp)
  (let ((stdout (current-output-port)))
    (call-with-output-file "code.fasl"
      (lambda (port)
        (let* ((exp (front exp))
	       #;(_ (begin (display ";; top ") (display exp)
			 (display " in ")
			 (display (free-variables exp '()))
			 (newline)))
	       (env (make-environment (free-variables exp '()) '()))
	       (template (assemble (compile-code exp env)))
	       (closure (make-closure template
				      (toplevel-environment env))))
          ;;(display ";; " stdout) (write closure stdout) (newline stdout)
          (write-fasl closure port))))))

(define (compile-file file-name)
  (let ((exp (read-test-from-file file-name)))
    (scmc exp)))

(define (main args)
  (for-each compile-file args))

(define *debug-port* #f)

(define (debug . args)
  (if *debug-port*
      (begin
	(display ";; ")
	(for-each (lambda (e) (display e *debug-port*)) args)
	(newline))))

(define (debug-on!) (set! *debug-port* (current-output-port)))
(define (debug-off!) (set! *debug-port* #f))

(define (scm exp)
  (scmc exp)
  (run (linux32 "../runtime/rt" "code.fasl")))
