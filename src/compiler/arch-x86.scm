;;; arch-x86.scm -- architecture definition of the runtime

(define $wordsize 4)

(define $fx-shift 2)
(define $fx-mask  #x03)
(define $fx-tag   #b00)

(define $fx-bits (- (* $wordsize 8) $fx-shift))

(define $pair-shift 3)
(define $pair-mask  #x07)
(define $pair-tag #b001)

(define $closure-shift 3)
(define $closure-mask #x07)
(define $closure-tag #b010)

(define $symbol-shift 3)
(define $symbol-mask #x07)
(define $symbol-tag #b011)

(define $vector-shift 3)
(define $vector-mask  #x07)
(define $vector-tag #b101)

(define $string-shift 3)
(define $string-mask #x07)
(define $string-tag #b110)

(define $bool-shift 8)
(define $bool-bit 6)
(define $bool-mask #b11101111)

(define $char-shift 8)
(define $char-mask #xff)
(define $char-tag #x0f)

(define $immediate-mask       #xff)
(define $immediate-shift      3)
(define $immediate-tag        #b111)
(define $immediate-false      #b00101111)
(define $immediate-true       #b00111111)
(define $immediate-nil        #b01001111)
(define $immediate-unbound    #b01111111)
(define $immediate-unspecific #b01101111)
(define $immediate-eof        #b01011111)

(define $closure "%esi")
(define $argc "%edx")

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= $fx-lower x) (<= x $fx-upper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x)))

(define (encode obj)
  (cond ((integer? obj) (* obj 4))
	((boolean? obj) (if obj $immediate-true $immediate-false))
	((char? obj) (+ $char-tag (* 256 (char->integer obj))))
	((null? obj) $immediate-nil)
	(else (error "No encoding for object ~a" obj))))