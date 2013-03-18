;;; asm-x86.scm -- assembler for x86

(define (emit . str)
  (for-each display str) (newline))

(define (emit-function-header label)
  (emit "    .globl " label)
  (emit "    .type " label ", @function")
  (emit label ":"))

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((label (string-append "L_" (number->string count))))
	(set! count (+ count 1))
	label))))

(define (unique-labels vars) (map (lambda (o) (unique-label)) vars))

(define (emit-stack-save si)
  (emit "    movl %eax, " si "(%esp)"))

(define (emit-stack-load si)
  (emit "    movl " si "(%esp), %eax"))

(define (emit-adjust-base si)
  (if (= si 0)
      (emit ";   addl $" si ",%esp")
      (emit "    addl $" si ", %esp")))
