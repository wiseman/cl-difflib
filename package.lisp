;;; CL-DIFFLIB -- A Lisp library for computing differences between
;;; sequences.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: package.lisp,v 1.5 2005/02/24 20:05:52 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This is the package definition.

(cl:defpackage "DIFFLIB"
  (:use #:common-lisp)
  (:export #:opcode
	   #:make-opcode
	   #:opcode-p
	   #:opcode-tag
	   #:opcode-i1
	   #:opcode-i2
	   #:opcode-j1
	   #:opcode-j2
	   #:opcode-range
	   #:opcode=

	   #:sequence-matcher

	   #:sequence-a
	   #:sequence-b
	   #:junk-function
	   #:test-function
	   #:set-sequences
	   #:set-sequence-a
	   #:set-sequence-b

	   #:get-opcodes
	   #:group-opcodes
	   #:similarity-ratio
	   #:quick-similarity-ratio
	   #:very-quick-similarity-ratio
	   
	   #:get-close-matches
	   #:unified-diff
	   #:context-diff))
	   