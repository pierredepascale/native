;;; package-s48.scm -- Scheme48 package definition for Scheme like compiler

(define-structure native-compiler
  (export)
  (open scheme scsh)
  (files compat-s48
	 arch-x86
	 asm-x86
	 front
	 back-x86))

