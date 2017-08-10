;;; package.scm -- package definition for Scheme like compiler

(define-structure native-compiler
  (open scheme-runtime scheme-posix)
  (doc "native Scheme compiler")
  (files arch-x64.scm
	 asm-x64.scm
	 front.scm
	 back-x64.scm
	 fasl.scm
	 test.scm
	 ))

