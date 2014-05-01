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
