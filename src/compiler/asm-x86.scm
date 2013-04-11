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
  (x86-movl %eax (@ si %esp)))

(define (emit-stack-load si)
  (x86-movl (@ si %esp) %eax))

(define (emit-adjust-base si)
  (if (= si 0)
      (x86-addl ($ si) %esp)
      (x86-addl ($ si) %esp)))

(define (flatten tree)
  (cond ((null? tree) tree)
        ((and (pair? tree) (eq? (car tree) 'short)) tree)
        ((and (pair? tree) (eq? (car tree) 'long)) tree)
        ((number? tree) (list tree))
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
                ((string? op) (lp (cdr code) (cons op pc) pc))
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
                   (template-set! template pc 0)
                   (lp (cdr code) (+ pc 1)))
                  ((and (pair? op) (eq? (car op) 'long))
                   (template-set! template pc 0)
                   (lp (cdr code) (+ pc 4)))
                  ((string? op)
                   (lp (cdr code) pc))
                  (else (error "unknown assembler operand ~a" op))))))))

(define (x86-movl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (cons #xb8 (x86-encode-32 (x86-immediate-value src))))
        (else (error "movl not supported: movl ~s ~s" src dst))))

(define (x86-cmp val dst)
  (if (eq? dst %al)
      (cons #x3c (x86-encode-8 val))
      (error "unsupported asm")))

(define (x86-je label)
  (cons #x74 (list 'short label)))

(define (x86-jmp label)
  (cons #xeb (list 'short label)))

(define (x86-shll src val) (list))

(define (x86-orl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (cons #x83 (cons #xc8 (x86-encode-8 (x86-immediate-value src)))))
        (else (error "unsupported syntax orl ~s ~s" src dst))))

(define (x86-addl src dst)
  (cond ((and (x86-immediate? src) (eq? dst %eax))
         (cons #x83 (cons #xc0 (x86-encode-8 (x86-immediate-value src)))))
        (else (error "unsupported syntax addl ~s ~s" src dst))))

(define (x86-and src dst)
  (cond ((and (x86-immediate? src) (eq? dst %al))
         (cons #x24 (x86-encode-8 (x86-immediate-value src))))
        (else (error "unsupported syntax andl ~s ~s" src dst))))

(define (x86-sal src dst) (list))

(define (x86-ret) (list #xC3))

(define (x86-call-*eax) (list #xff #xd0))

(define (x86-sete reg)
  (if (eq? reg %al)
      (list #x0f #x94 #xc0)
      (error "unsupported asm")))

(define (x86-movzbl src dst)
  (if (and (eq? src %al) (eq? dst %eax))
      (list #x0f #xb6 #xc0)
      (error "unsupported asm")))

(define (x86-label label) label)

(define (x86-immediate? val) (and (pair? val) (eq? (car val) '$)))
(define (x86-immediate-value imm) (cadr imm))
(define (x86-indirect? val) (and (pair? val) (eq? (car val) '@)))
(define (x86-indirect-register ind) (caddr ind))
(define (x86-indirect-offset ind) (cadr ind))
(define (x86-register? reg) (number? reg))

(define ($ val) (list '$ val))
(define (@ offset reg) (list '@ offset reg))

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
  (let ((b0 (quotient val (* 256 65536)))
        (b1 (modulo (quotient val 65536) 256))
        (b2 (modulo (quotient val 256) 256))
        (b3 (modulo val 256)))
    (list b3 b2 b1 b0)))

(define (x86-encode-8 val)
  (if (< val 256)
      (list val)
      (error "value too large ~a" val)))
