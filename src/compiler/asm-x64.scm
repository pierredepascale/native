;;; asm-x64.scm -- x86_64 assembler

(define (reg? obj) (number? obj))
(define (regi? obj) (and (pair? obj) (reg? (cadr obj))))
(define (^ offset reg) (list '^ offset reg))
(define (regi-register obj) (cadr obj))
(define (regi-offset obj) (car obj))
(define ($ n) (list '$ n))
(define (imm? obj) (and (pair? obj) (eq? (car obj) '$)))
(define (imm obj) (cadr obj))
(define (imm8? obj) (and (imm? obj) (< (cadr obj) 128)))
(define (imm8 obj) (cadr obj))

(define %rax 0)
(define %rcx 1)
(define %rdx 2)
(define %rbx 3)
(define %rsp 4)
(define %rbp 5)
(define %rsi 6)
(define %rdi 7)
(define %r8 8)
(define %r9 9)
(define %r10 10)
(define %r11 11)
(define %r12 12)
(define %r13 13)
(define %r14 14)
(define %r15 15)

;; W = width 0=default 1=64bit
;; R = Register number of modrm.reg
;; X = SIB extension
;; B = register number of modrm.rm

(define (rex-prefix w r x b)
  (+ #x40 (+ (* w 8) (+ (* r 4) (+ (* 2 x) b)))))

(define (rex-modrm-reg reg) (if (> reg 8) 1 0))
(define (rex-sib sib) 0)
(define (rex-modrm-rm rm) (if (> rm 8) 1 0))

;; mod=0X00: [R/M]
;; mod=0X01: [r/m + dusp8]
;; mod=0x02: [r/m + disp32]
;; mod=0x03: r/m
(define (modrm mod reg rm)
  (let ((reg (modulo reg 8))
        (rm (modulo rm 8)))
    (+ (* 64 mod) (+ (* reg 8) rm))))

(define (sib scale index base) (+ (* 64 scale) (+ (* index 8) base)))

;;; instructions

(define (movq src dst)
  (cond ((and (imm? src) (reg? dst))
         (list (rex-prefix 1 0 0 (rex-modrm-rm dst))
               #xc7
               (modrm #b11 0 dst)
               (encode-32 (imm src))))
        ((and (regi? src) (imm8? (regi-offset src)) (reg? dst))
         (list (rex-prefix 1 (rex-modrm-reg dst) 0 (rex-modrm-rm src))
               #x8b
               (modrm #b01 dst (regi-register src))
               (encode-8 (regi-offset dst))))
        ((and (reg? src) (regi? dst) (imm8? (regi-offset dst)))
         (list (rex-prefix 1 (rex-modrm-reg src) 0 (rex-modrm-rm dst))
               #x89
               (modrm #b01 src (regi-register dst))))
        ((and (regi? src) (imm? (regi-offset src)) (reg? dst))
         (list (rex-prefix 1 (rex-modrm-reg dst) 0 (rex-modrm-rm src))
               #x8b
               (modrm #b02 dst (regi-register src))
               (encode-8 (regi-offset dst))))
        ((and (reg? src) (regi? dst) (imm? (regi-offset dst)))
         (list (rex-prefix 1 (rex-modrm-reg src) 0 (rex-modrm-rm dst))
               #x89
               (modrm #b02 src (regi-register dst))))
        (else (error "unknown MOVQ syntax ~a ~b" src dst))))

(define (movzbl src dst)
  (error "MOVZBL not implemented")
  (cond ((and (imm8? src) (reg? dst))
         (list))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown MOVZBL syntax ~a ~b" src dst))))
  
(define (addq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list (rex-prefix 1 0 0 (rex-modrm-rm dst))
               #x83
               (modrm #b11 0 dst)
               (encode-8 (imm8 src))))
        ((and (regi? src) (imm8? (regi-offset src)) (reg? dst))
         (list (rex-prefix 1 (rex-modrm-reg dst) 0
                           (rex-modrm-rm (regi-register src)))
               #x03
               (modrm #b01 dst (regi-register src))
               (encode-8 (regi-offset src))))
        ((and (reg? src) (regi? dst) (imm8? (regi-offset dst)))
         (list (rex-prefix 0 (rex-modrm-reg src) 0
                           (rex-modrm-rm (regi-register dst)))
               #x01
               (modrm #b01 dst (regi-register dst))
               (encode-8 (regi-offset dst))))
        (else (error "unknown ADDDQ syntax ~a ~b" src dst))))

(define (subq src dst)
  (cond ((and (imm? src) (reg? dst))
         (list (rex-prefix 1 0 0 (rex-modrm-rm dst))
               #x83
               (modrm #b11 #b101 dst)
               (encode-8 (imm src))))
        ((and (regi? src) (reg? dst))
         (list (rex-prefix 1 (rex-modrm-reg dst) 0
                           (rex-modrm-rm (regi-register src)))
               #x2b
               (modrm #b01 dst (regi-register src))
               (encode-8 (regi-offset src))))
        ((and (reg? src) (regi? dst))
         (list (rex-prefix 1 (rex-modrm-reg src) 0
                           (rex-modrm-rm (regi-register dst)))
               #x29
               (modrm #b01 src (regi-register dst))
               (encode-8 (regi-offset dst))))
        (else (error "unknown SUBQ syntax ~a ~b" src dst))))

(define (orq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown ORQ syntax ~a ~b" src dst))))

(define (andq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown ANDQ syntax ~a ~b" src dst))))

(define (xorq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list (rex-prefix)
               #x31
               (modrm)
               (encode-8)))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown XORQ syntax ~a ~b" src dst))))

(define (shlq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown XORQ syntax ~a ~b" src dst))))
  
(define (cmpq src dst)
  (cond ((and (imm8? src) (reg? dst))
         (list))
        ((and (regi? src) (reg? dst))
         (list))
        ((and (reg? dst) (regi? dst))
         (list))
        (else (error "unknown CMPQ syntax ~a ~b" src dst))))

;;; control instructions

(define (je label) (cons #x74 (list 'short label)))
(define (jmp label) (cons #xeb (list 'short label)))
(define (jmp/l label) (cons #xe9 (list 'long label)))
(define (retq) (list #xc3))
(define (callq-%rax) (list #xff #xd0))

(define (sete reg)
  (if (eq? reg %al)
      (list #x0f #x94 #xc0)
      (error "unknown SETE encoding")))

(define (setl reg)
  (if (eq? reg %al)
      (list #x0f #x9c #xc0)
      (error "unknown SETL encoding")))

;;; infrastructure

(define (test-asm)
  (if (equal? '(#x48 #x83 #xc1 #x0c)
              (addq ($ 12) %rcx))
      (display "[OK  ] ADDQ $12, %RCX")
      (display "[FAIL] ADDQ $12, %RCX"))
  (newline)
  (if (equal? '(#x48 #x83 #xe9 #x0c)
              (subq ($ 12) %rcx))
      (display "[OK  ] SUBQ $12, %RCX")
      (display "[FAIL] SUBQ $12, %RCX"))
  (newline)
  (if (equal? '(#x48 #xc7 #xc1 #x0c #x00 #x00 #x00)
              (movq ($ 12) %rcx))
      (display "[OK  ] MOVQ $12, %RCX")
      (display "[FAIL] MOVQ $12, %RCX"))
  (newline)
  )

(define (emit . code) code)

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
		   (let ((offset (encode-32 (label-long-offset (cadr op) pc labels))))
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

(define (encode-32 val)
  (if (< val 0)
      (if (< val -128)
	  (error "cannot encode value ~a" val)
	  (list (encode-8 val) 255 255 255))
      (let ((b0 (quotient val (* 256 65536)))
	    (b1 (modulo (quotient val 65536) 256))
	    (b2 (modulo (quotient val 256) 256))
	    (b3 (modulo val 256)))
	(list b3 b2 b1 b0))))

(define (encode-8 val)
  (if (or (> val 255) (< val -128))
      (error "value too small to fit in a byte ~a" val))
  (cond ((< val 0) (+ 256 val))
	(else val)))
