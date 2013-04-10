;;; compiler.scm -- native compiler for Scheme like compiler

;;;
;; = Compiler toplevel

;; compile the resulting assembly with
;; gcc -m32 -o scm rt.c scheme.S entry_x86.S

;; (define (compile-function exp env si label)
;;   #;(emit "; compiling " exp si label)
;;   (emit-function-header label)
;;   (compile exp env si '%eax)
;;   (emit "    ret"))

(define (compile-code exp env)
  (emit (compile exp env 0 %eax)
        (x86-ret)))

(define (compile exp env si dst)
  (cond ((literal? exp) (compile-literal exp env si dst))
	((variable? exp) (compile-variable exp env si dst))
	((clambda? exp) (compile-lambda exp env si dst))
	((if? exp) (compile-if exp env si dst))
	((let? exp) (compile-let exp env si dst))
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
  (or (integer? obj) (boolean? obj) (null? obj) (char? obj)))

(define (compile-literal exp env si dst)
  (emit
   (x86-movl ($ (encode exp)) dst)))

;;;
;; == Conditional expressions

(define (if? obj) (and (pair? obj) (eq? 'if (car obj))))
(define if-test cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define (compile-if exp env si dst)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (compile (if-test exp) env si %eax)
    (emit "    cmp $" (encode #f) ", %al")
    (emit "    je " alt-label)
    (compile (if-consequent exp) env si dst)
    (emit "    jmp " end-label)
    (emit alt-label ":")
    (compile (if-alternative exp) env si dst)
    (emit end-label ":")))

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
  (compile arg env si)
  (emit "    addl $" (encode 1) ", " dst))

(define-primitive (%fixnum->char env si dst arg)
  (compile arg env si dst)
  (emit "    shll $" (- $char-shift $fx-shift) ", " dst)
  (emit "    orl $" $char-tag ", " dst))

(define-primitive (%fixnum? env si dst arg)
  (compile arg env si %eax)
  (emit "    and $" $fx-mask ", %al")
  (emit "    cmp $" $fx-tag ", %al")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst %eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%fx-1+ env si dst arg)
  (compile arg env si dst)
  (emit "    addl $" (encode -1) ", " dst))

(define-primitive (%null? env si dst arg)
  (compile arg env si '%eax)
  (emit "    cmp $" (encode '()) ", %al")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%not env si dst arg)
  (compile arg env si '%eax)
  (emit "    cmp $" (encode #f) ", %eax")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    xorl $1, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%boolean? env dst si arg)
  (compile arg env si '%eax)
  (emit "    and $" $bool-mask ", %al")
  (emit "    cmp $" (encode #f) ", %al")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))
 
(define-primitive (%char? env si dst arg)
  (tagged-pointer-predicate env si dst arg $char-mask $char-tag))

(define-primitive (%fx= env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env si '%eax)
  (emit "    cmp %eax, " si "(%esp)")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%fxzero? env si dst arg)
  (compile arg env si '%eax)
  (emit "    cmpl $0, %eax")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%fx< env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env si '%eax)
  (emit "    cmp %eax, " si "(%esp)")
  (emit "    setl %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

(define-primitive (%fx+ env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env (- si $wordsize) dst)
  (emit "    addl " si "(%esp), " dst))

(define-primitive (%fx- env si dst arg1 arg2)
  (compile arg2 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg1 env (- si $wordsize) dst)
  (emit "    subl " si "(%esp), " dst))

(define-primitive (%vector env si dst arg)
  (error "unimplemented %vector"))

(define-primitive (%vector-ref env si dst arg1 arg2)
  (error "unimplemented %vector-ref"))

(define-primitive (%vector-set! env si dst arg1 arg2 arg3)
  (error "unimplemented %vector-set!"))

(define-primitive (%vector? env si dst arg)
  (tagged-pointer-predicate env si dst arg $vector-mask $vector-tag))

(define-primitive (%pair? env si dst arg)
  (tagged-pointer-predicate env si dst arg $pair-mask $pair-tag))

(define-primitive (%cons env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env (- si $wordsize) '%eax)
  (emit "    movl %eax, " $wordsize "(%ebp)")
  (emit "    movl " si "(%esp), %eax")
  (emit "    movl %eax, (%ebp)")
  (emit "    movl %ebp, " dst)
  (emit "    addl $" $pair-tag ", " dst)
  (emit "    addl $" (* 2 $wordsize) ", %ebp"))

(define-primitive (%car env si dst arg)
  (compile arg env si dst)
  (emit "    movl " (- 0 $pair-tag) "(" dst "), " dst))

(define-primitive (%cdr env si dst arg)
  (compile arg env si dst)
  (emit "    movl " (- $wordsize $pair-tag) "(" dst "), " dst))

(define-primitive (%set-car! env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env (- si $wordsize) '%eax)
  (emit "    movl " si "(%esp), %ebx")
  (emit "    movl %eax, " (- 0 $pair-tag) "(%ebx)"))

(define-primitive (%set-cdr! env si dst arg1 arg2)
  (compile arg1 env si '%eax)
  (emit "    movl %eax, " si "(%esp)")
  (compile arg2 env (- si $wordsize) '%eax)
  (emit "    movl " si "(%esp), %ebx")
  (emit "    movl %eax, " (- $wordsize $pair-tag) "(%ebx)"))

(define-primitive (%closure? env si dst arg)
  (tagged-pointer-predicate env si dst arg $closure-mask $closure-tag))

(define-primitive (%symbol? env si dst arg)
  (tagged-pointer-predicate env si dst arg $symbol-mask $symbol-tag))

(define (tagged-pointer-predicate env si dst arg mask tag)
  (compile arg env si %eax)
  (emit "    and $" mask ", %al")
  (emit "    cmp $" tag ", %al")
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $" $bool-bit ", %al")
  (emit "    or $" $immediate-false ", %al")
  (if (not (eq? dst '%eax))
      (emit "    movl %eax, " dst)))

;;;
;; == Variables

(define variable? symbol?)

(define (closed-binding? binding) (and (pair? binding) (eq? 'closed (car binding))))
(define (local-binding? binding) (and (pair? binding) (eq? 'local (car binding))))
(define local-binding-index cdr)
(define closed-binding-index cdr)

(define (lookup-env var env)
  (let ((entry (assq var env)))
    (if entry (cdr entry) #f)))

(define (compile-variable exp env si dst)
  (let ((binding (lookup-env exp env)))
    (if binding
	(if (local-binding? binding)
	    (emit "    movl " (local-binding-index binding) "(%esp), " dst)
	    (emit "    movl " (closed-binding-index binding) "(%esi), " dst))
	(error "unbound variable ~a" exp))))

;;;
;; == Let expression

(define (let? exp) (and (pair? exp) (eq? 'let (car exp))))
(define let-bindings cadr)
(define let-body caddr)
(define binding-expression cadr)
(define binding-name car)

(define (bind-var name index env) (cons (cons name (cons 'local index)) env))
(define (bind-closed-var name index env) (cons (cons name (cons 'closed index)) env))

(define (compile-let exp env si dst)
  (let compile-bindings ((bindings (let-bindings exp))
			 (si si)
			 (new-env env))
    (if (null? bindings)
	(compile (let-body exp) new-env si dst)
	(let ((binding (car bindings)))
	  (compile (binding-expression binding) env si %eax)
	  (emit-stack-save si)
	  (compile-bindings (cdr bindings) (- si $wordsize)
			    (bind-var (binding-name binding) si env))))))

;;;
;; == Procedure

(define (clambda? exp) (and (pair? exp) (eq? 'lambda (car exp))))
(define clambda-formals cadr)
(define clambda-free caddr)
(define clambda-body cadddr)

(define (compile-lambda-free frees env si offset)
  (if (null? frees)
      'ok
      (let ((free (car frees)))
	(compile free env si %eax)
	(emit "    movl %eax, " offset "(%ebp)")
	(compile-lambda-free (cdr frees) env si (+ offset $wordsize)))))

(define (make-closed-env) 12)

(define (compile-entry exp env si dst)
  (let ((body (clambda-body exp))
	(formals (clambda-formals exp))
	(ok-label (unique-label)))
    (emit "    cmpl $" (length formals) ", %edx")
    (emit "    je " ok-label)
    (emit ok-label ":")
    (compile body env si dst)))

(define (make-closed-env frees env offset)
  (if (null? frees)
      env
      (let ((free (car frees)))
	(make-closed-env (cdr frees)
			 (bind-closed-var free offset env)
			 (+ offset $wordsize)))))

(define (compile-lambda exp env si dst)
  (let ((formals (clambda-formals exp))
	(body (clambda-body exp))
	(label-entry (unique-label))
	(label-end (unique-label)))
    (emit "    jmp " label-end)
    (emit label-entry":")
    (let f ((formals formals)
	    (si (- 0 $wordsize))
	    (env env))
      (if (null? formals)
	  (compile-entry exp (make-closed-env (clambda-free exp) env (- (* 2 $wordsize) $closure-tag)) si %eax)
	  (f (cdr formals)
	     (- si $wordsize)
	     (bind-var (car formals) si env))))
    (emit "    ret")
    (emit label-end":")
    (compile-lambda-free (clambda-free exp) env si (* 2 $wordsize))
    (emit "    movl $" (* (+ (length (clambda-free exp)) 2) $wordsize) ", (%ebp)")
    (emit "    movl $" label-entry ", " $wordsize "(%ebp)")
    (emit "    movl %ebp, " dst)
    (emit "    addl $" (* $wordsize (+ (length (clambda-free exp)) 2)) ", %ebp")
    (emit "    addl $" $closure-tag ", " dst)))

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
;; == Procedure call

(define call? pair?)
(define call-arguments cdr)
(define call-target car)

(define (compile-arguments args env si)
  (if (null? args)
      si
      (begin
	(compile (car args) env si %eax)
	(emit "    movl %eax, " si "(%esp)")
	(compile-arguments (cdr args) env (- si $wordsize)))))

(define (compile-call exp env si dst)
  (let ((si* (compile-arguments (call-arguments exp) env (- si $wordsize))))
    (compile (call-target exp) env si* '%esi)
    (emit "    movl $" (encode (length (call-arguments exp))) ", %edx")
    (emit-adjust-base (+ si $wordsize))
    (emit "    movl " (- $wordsize $closure-tag) "(%esi), %eax")
    (emit "    call *%eax")
    (emit-adjust-base (- 0 (+ si $wordsize)))))

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

(define (scmc exp)
  (let ((stdout (current-output-port)))
    (with-output-to-file "code.fasl"
      (lambda ()
        (let ((template (assemble (compile-code exp '()))))
          (display ";; " stdout) (write template stdout) (newline stdout)
          (write-fasl (make-closure template '()) (current-output-port)))))))

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

(define (test-exp name exp)
  (scmc (car exp))
  (let ((result (run/string ("../runtime/rt" "code.fasl"))))
    (if (string=? result (cdr exp))
        (begin
          (display "[OK!] ") (display name) (newline))
        (begin
          (display "[ERR] ") (display name) (newline)))))

(define (run-test-suite)
  (let ((files (directory-files "../../test")))
    (for-each (lambda (fn)
                (let ((test (read-test-from-file (string-append "../../test/" fn))))
                  (test-exp fn exp)))
              files)))

;;; FASL support

(define $fasl/template 0)
(define $fasl/ref 1)
(define $fasl/number 2)
(define $fasl/closure 3)
(define $fasl/symbol 4)

(define (write-fasl code port)
  ;(debug 'write-fasl code)
  (cond ((template? code) (write-fasl-template code port))
        ((ref? code) (write-fasl-ref code port))
        ((number? code) (write-fasl-number code port))
        ((closure? code) (write-fasl-closure code port))
        ((symbol? code) (write-fasl-symbol code port))
        (else (error "don't know how to write fasl ~a" code))))

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

(define (u8->char u8) (integer->char (+ u8 -32 (char->integer #\space))))

(define (make-template size) (vector 'template (make-vector size)))
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
