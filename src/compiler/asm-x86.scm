;;; asm-x86.scm -- assembler for x86

(define (emit . code) code)

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((label (string-append "L_" (number->string count))))
	(set! count (+ count 1))
	label))))

(define (unique-labels vars) (map (lambda (o) (unique-label)) vars))

(define (emit-stack-save si)
  (x86-movl %eax (^ si %esp)))

(define (emit-stack-load si)
  (x86-movl (^ si %esp) %eax))

(define (emit-adjust-base si)
  (if (= si 0)
      (list)
      (x86-addl ($ si) %esp)))

(define (flatten tree)
  (cond ((null? tree) tree)
        ((and (pair? tree) (eq? (car tree) 'short)) (list tree))
        ((and (pair? tree) (eq? (car tree) 'long)) (list tree))
        ((number? tree) (list tree))
	((string? tree) (list tree))
        (else (append (flatten (car tree))
                      (flatten (cdr tree))))))

(define (compute-labels code k)
  (let lp ((code code)
           (labels '())
           (pc 0))
    (if (null? code)
        (k labels pc)
        (let ((op (car code)))
          (cond ((number? op) (lp (cdr code) labels (+ pc 1)))
                ((and (pair? op) (eq? (car op) 'short))
                 (lp (cdr code) labels (+ pc 1)))
                ((and (pair? op) (eq? (car op) 'long))
                 (lp (cdr code) labels (+ pc 4)))
                ((string? op) (lp (cdr code) (cons (cons op pc) labels) pc))
                (else (error "unknown assembler opcode ~a" op)))))))

(define (assemble code)
  (let ((flatten-code (flatten code)))
    (compute-labels flatten-code
                    (lambda (labels size)
                      (assemble-code flatten-code labels size)))))

(define (assemble-code code labels size)
  (let ((template (make-template size)))
    (let lp ((code code)
             (pc 0))
      (if (null? code)                            
          template
          (let ((op (car code)))
            (cond ((number? op)
                   (template-set! template pc op)
                   (lp (cdr code) (+ pc 1)))
                  ((and (pair? op) (eq? (car op) 'short))
		   (let ((offset (label-short-offset (cadr op) pc labels)))
		     (if (or (> offset 127) (< offset -128))
			 (error "offset too large for a short branch: ~a ~a" offset pc)			
			 (template-set! template pc (if (< offset 0)
							(+ 256 offset)
							offset)))
		     (lp (cdr code) (+ pc 1))))
                  ((and (pair? op) (eq? (car op) 'long))
		   (let ((offset (x86-encode-32 (label-long-offset (cadr op) pc labels))))
		     (template-set! template pc (car offset))
		     (template-set! template (+ pc 1) (cadr offset))
		     (template-set! template (+ pc 2) (caddr offset))
		     (template-set! template (+ pc 3) (cadddr offset))
		     (lp (cdr code) (+ pc 4))))
                  ((string? op)
                   (lp (cdr code) pc))
                  (else (error "unknown assembler operand ~a" op))))))))

(define (label-short-offset label pc labels)
  (let ((entry (assoc label labels)))
    (if entry
	(- (cdr entry) (+ pc 1))
	(error "undefined short label ~a" label))))

(define (label-long-offset label pc labels)
  (let ((entry (assoc label labels)))
    (if entry
	(- (cdr entry) (+ pc 4))
	(error "undefined long label ~a" label))))

(define (x86-movl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (cons #xb8 (x86-encode-32 (x86-immediate-value src))))
	((and (x86-immediate? src)
	      (x86-indirect? dst)
	      (eq? %ebp (x86-indirect-register dst)))
	 (cons #xc7 (cons #x45 (cons (x86-encode-8 (x86-indirect-offset dst))
				     (x86-encode-32 (x86-immediate-value src))))))
	((and (x86-immediate? src)
	      (eq? %edx dst))
	 (cons #xba (x86-encode-32 (x86-immediate-value src))))
	((and (eq? src %eax)
	      (x86-indirect? dst)
	      (eq? %esp (x86-indirect-register dst)))
	 (list #x89 #x44 #x24 (x86-encode-8 (x86-indirect-offset dst))))
	((and (eq? src %eax)
	      (x86-indirect? dst)
	      (eq? %ebp (x86-indirect-register dst)))
	 (list #x89 #x45 (x86-encode-8 (x86-indirect-offset dst))))
	((and (eq? src %ebp) (eq? dst %eax))
	 (list #x89 #xe8))
	((and (eq? dst %eax)
	      (x86-indirect? src)
	      (eq? %esp (x86-indirect-register src)))
	 (list #x8b #x44 #x24 (x86-encode-8 (x86-indirect-offset src))))
	((and (eq? dst %eax)
	      (x86-indirect? src)
	      (eq? %esi (x86-indirect-register src)))
	 (list #x8b #x46 (x86-encode-8 (x86-indirect-offset src))))
	((and (eq? src %esi)
	      (x86-indirect? dst)
	      (eq? %esp (x86-indirect-register dst)))
	 (list #x89 #x74 #x24 (x86-encode-8 (x86-indirect-offset dst))))
	((and (eq? src %ebp) (eq? dst %esi))
	 (list #x89 #xee))
	((and (eq? src %esi) (eq? dst %eax))
	 (list #x89 #xf0))
	((and (x86-indirect? src)
	      (eq? %esp (x86-indirect-register src))
	      (eq? %esi dst))
	 (list #x8b #x74 #x24 (x86-encode-8 (x86-indirect-offset src))))
	((and (x86-indirect? src)
	      (eq? (x86-indirect-register src) %esi)
	      (eq? %esi dst))
	 (list #x8b #x76 (x86-encode-8 (x86-indirect-offset src))))
	((and (x86-immediate? src)
	      (eq? %esi dst))
	 (cons #xbe (x86-encode-32 (x86-immediate-value src))))
        (else (error "movl not supported: movl ~s ~s" src dst))))

(define (x86-cmp val dst)
  (if (and (x86-immediate? val) (eq? dst %al))
      (list #x3c (x86-encode-8 (x86-immediate-value val)))
      (error "cmp unsupported asm: cmp ~a ~a" val dst)))

(define (x86-cmpl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
	 (list #x83 #xf8 (x86-encode-8 (x86-immediate-value src))))
	((and (eq? src %eax) (x86-indirect? dst))
	 (list #x39 #x44 #x24 (x86-encode-8 (x86-immediate-value dst))))
        (else (error "cmpl not supported: cmpl ~s ~s" src dst))))

(define (x86-je label)
  (cons #x74 (list 'short label)))

(define (x86-jmp label)
  (cons #xeb (list 'short label)))

(define (x86-orl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (list #x83 #xc8 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-immediate? src) (eq? dst %esi))
	 (list #x83 #xce (x86-encode-8 (x86-immediate-value src))))
        (else (error "unsupported syntax orl ~s ~s" src dst))))

(define (x86-or src dst)
  (cond ((and (x86-immediate? src) (eq? dst %al))
         (list #x0c (x86-encode-8 (x86-immediate-value src))))
        (else (error "or unsupported syntax: or ~s ~s" src dst))))

(define (x86-xorl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (list #x83 #xf0 (x86-encode-8 (x86-immediate-value src))))
        (else (error "xorl unsupported syntax: xorl ~s ~s" src dst))))

(define (x86-andl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (list #x83 #xe0 (x86-encode-8 (x86-immediate-value src))))
        (else (error "andl unsupported syntax: andl ~s ~s" src dst))))

(define (x86-and src dst)
  (cond ((and (x86-immediate? src) (eq? dst %al))
         (list #x24 (x86-encode-8 (x86-immediate-value src))))
        (else (error "unsupported syntax andl ~s ~s" src dst))))

(define (x86-shl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %al))
	 (list #xc0 #xe0 (x86-encode-8 (x86-immediate-value src))))
        (else (error "shl not supported: shl ~s ~s" src dst))))

(define (x86-shll src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
	 (list #xc1 #xe0 (x86-encode-8 (x86-immediate-value src))))
        (else (error "cmpl not supported: cmpl ~s ~s" src dst))))

(define (x86-addl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (list #x83 #xc0 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-immediate? src) (eq? dst %esp))
	 (list #x83 #xc4 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-immediate? src) (eq? dst %esi))
	 (list #x83 #xc6 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-immediate? src) (eq? dst %ebp))
         (list #x83 #xc5 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-indirect? src) (eq? dst %eax))
	 (list #x03 #x44 #x24 (x86-encode-8 (x86-immediate-value src))))
        (else (error "unsupported syntax addl ~s ~s" src dst))))

(define (x86-subl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (list #x83 #xe8 (x86-encode-8 (x86-immediate-value src))))
	((and (x86-indirect? src) (eq? dst %eax))
	 (list #x2b #x44 #x24 (x86-encode-8 (x86-immediate-value src))))
        (else (error "unsupported syntax addl ~s ~s" src dst))))

(define (x86-sal src dst) (x86-shl src dst))

(define (x86-ret) (list #xC3))

(define (x86-call-*eax) (list #xff #xd0))

(define (x86-sete reg)
  (if (eq? reg %al)
      (list #x0f #x94 #xc0)
      (error "sete unsupported asm ~a" reg)))

(define (x86-setl reg)
  (if (eq? reg %al)
      (list #x0f #x9c #xc0)
      (error "setl unsupported asm ~a" reg)))

(define (x86-movzbl src dst)
  (if (and (eq? src %al) (eq? dst %eax))
      (list #x0f #xb6 #xc0)
      (error "movzbl unsupported asm: movzbl ~a ~a" src dst)))

(define (x86-label label) label)

(define (x86-immediate? val) (and (pair? val) (eq? (car val) '$)))
(define (x86-immediate-value imm) (cadr imm))
(define (x86-indirect? val) (and (pair? val) (eq? (car val) '^)))
(define (x86-indirect-register ind) (caddr ind))
(define (x86-indirect-offset ind) (cadr ind))
(define (x86-register? reg) (number? reg))

(define ($ val) (list '$ val))
(define (^ offset reg) (list '^ offset reg))

(define %eax 0)
(define %ecx 1)
(define %edx 2)
(define %ebx 3)
(define %ebp 4)
(define %esp 5)
(define %esi 6)
(define %edi 7)

(define %al 8)
(define %bl 9)
(define %cl 10)
(define %dl 11)

(define $eq 0)
(define $ne 1)
(define $lt 2)
(define $gt 3)

(define (x86-encode-32 val)
  (if (< val 0)
      (if (< val -128)
	  (error "cannot encode value ~a" val)
	  (list (x86-encode-8 val) 255 255 255))
      (let ((b0 (quotient val (* 256 65536)))
	    (b1 (modulo (quotient val 65536) 256))
	    (b2 (modulo (quotient val 256) 256))
	    (b3 (modulo val 256)))
	(list b3 b2 b1 b0))))

(define (x86-encode-8 val)
  (if (or (> val 127) (< val -128))
      (error "value too small to fir in a byte ~a" val))
  (cond ((< val 0) (if (< val -128) (error "value too large ~a" val) (+ 256 val)))
	((> val 127) (error "value too large ~a" val))
	(else val)))
