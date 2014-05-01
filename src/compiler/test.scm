;;; test.scm -- test support

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

(define (read-test-from-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let ((exp (read))
            (expected (read)))
        (cons exp expected)))))

