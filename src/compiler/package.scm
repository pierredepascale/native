;;; package.scm -- package definition for Scheme like compiler

(define-structure native-compiler
  (open scheme-runtime scheme-posix)
  (doc "native Scheme compiler")
  (files arch-x86.scm
	 asm-x86.scm
	 front.scm
	 back-x86.scm
	 fasl.scm
	 test.scm
	 ))

