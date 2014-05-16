;;; arch-x86.scm -- architecture definition of the runtime

(define $wordsize 4)
(define (wordsize n) (* $wordsize n))

(define $primary-shift #x02)
(define $primary-mask #x03)

(define $fx-shift 2)
(define $fx-mask  #x03)
(define $fx-tag   #b00)

(define $fx-lower -536870912)
(define $fx-upper 536870911)

(define $fx-bits (- (* $wordsize 4) $fx-shift))

(define $closure-tag #x02)
(define $imm-tag #x03)
(define $ptr-tag #x01)

(define (header len code) (+ (* len 4) code))

(define $char-shift #x08)
(define $char-mask #xff)
(define $char-tag #x0f)

(define $secondary-mask  #x07)
(define $secondary-shift #x03)

(define $vector-tag     #x00)
(define $string-tag     #x01)
(define $symbol-tag     #x02)
(define $bytevector-tag #x03)
(define $template-tag   #x04)
(define $pair-tag       #x05)
(define $instance-tag   #x06)
(define $forward-tag    #x07)

(define $bool-bit 4)
(define $bool-mask #b11101111)

(define $immediate-mask       #xff)
(define $immediate-shift      3)
(define $immediate-tag        #b111)
(define $immediate-false      #b00101111)
(define $immediate-true       #b00111111)
(define $immediate-nil        #b01001111)
(define $immediate-unbound    #b01111111)
(define $immediate-unspecific #b01101111)
(define $immediate-eof        #b01011111)

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= $fx-lower x) (<= x $fx-upper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x)))

(define (char-code ch)
  (- (+ 32 (char->integer ch)) (char->integer #\space)))

(define (encode obj)
  (cond ((integer? obj) (* obj 4))
	((boolean? obj) (if obj $immediate-true $immediate-false))
	((char? obj) (+ $char-tag (* 256 (char-code obj))))
	((null? obj) $immediate-nil)
	((eof-object? obj) $immediate-eof)
	((unspecific-object? obj) $immediate-unspecific)
	((unbound-object? obj) $immediate-unbound)
	(else (error "No encoding for object ~a" obj))))

;;; ABI definition
;;
;; %esi - closure pointer
;; %ebp - allocation pointer
;; %ecx - context pointer
;; %eax - result register
;; %edx - argument count

(define (emit-stack-save si)
  (x86-movl %eax (^ si %esp)))

(define (emit-stack-load si dst)
  (let ((depth (wordsize si)))
    (x86-movl (^ depth %esp) dst)))

(define (emit-adjust-base si)
  (if (= si 0)
      (list)
      (x86-addl ($ si) %esp)))

(define (emit-free-load offset dst)
  (let ((off (wordsize (+ 2 offset))))
    (emit (x86-movl (^ off %esi) dst))))
