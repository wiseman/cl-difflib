;;; --------------------------------------------------------------------
;;; CL-DIFFLIB -- A Lisp library for computing differences between
;;; sequences.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: unit-tests.lisp,v 1.6 2005/02/24 20:09:17 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This file contains unit tests for CL-DIFFLIB.

(cl:defpackage "DIFFLIB-TEST"
  (:use #:common-lisp)
  (:export #:run-tests))

(in-package #:difflib-test)


;; Some simple unit test utilities

(defvar *passed-tests* '())
(defvar *failed-tests* '())

(defmacro test (name expr expected-value &optional (comparator '(function equal))
		failure-code)
  `(unless (test-aux ',name ',expr ,expr ,expected-value ,comparator)
    ,failure-code))

(defmacro condition-test (name expr expected-condition &optional (comparator '(function typep))
			  failure-code)
  (let ((completed-var (gensym "COMPLETED"))
	(condition-var (gensym "CONDITION"))
	(value-var (gensym "VALUE")))
    `(let ((,completed-var NIL))
       (multiple-value-bind (,value-var ,condition-var)
	   (ignore-errors
	     ,expr
	     (setf ,completed-var T))
	 (unless (condition-test-aux ',name ',expr ,value-var (not ,completed-var)
				     ,condition-var ,expected-condition ,comparator)
	   ,failure-code)))))

(defun condition-test-aux (name expr value error-p error expected-error comparator)
  (if error-p
      (let ((got-expected-p (funcall comparator error expected-error)))
	(if got-expected-p
	    (test-success name expr error expected-error)
	    (test-failure name expr error expected-error))
	got-expected-p)
      (test-failure name expr value expected-error)))

(defun test-aux (name expr value expected-value comparator)
  (let ((got-expected-p (funcall comparator value expected-value)))
    (if got-expected-p
	(test-success name expr value expected-value)
	(test-failure name expr value expected-value))
    got-expected-p))

(defun test-failure (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *failed-tests*)
  (warn "FAILURE: Test ~S: ~S evaluated to ~S instead of ~S."
	name expr value expected-value)
  nil)

(defun test-success (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *passed-tests*)
  (format T "~&Test ~S passed.~%" name))

(defun begin-tests ()
  (setf *passed-tests* '())
  (setf *failed-tests* '()))

(defun end-tests ()
  (let ((num-failed (length *failed-tests*))
	(num-passed (length *passed-tests*)))
    (format T "~&-----~&Testing complete, ~S of ~S tests failed (~,2F%)"
	    num-failed
	    (+ num-failed num-passed)
	    (* 100.0 (/ num-failed (+ num-failed num-passed))))
    (when (= num-failed 0)
      (format T "~&ALL TESTS PASSED."))))


;; Top level driver

(defun run-tests ()
  (begin-tests)
  (unwind-protect
      (progn
	(test-opcode-equality)
	(test-get-opcodes)
	(test-get-grouped-opcodes)
	(test-similarity-ratio)
	(test-close-matches)
	(test-unified-diff)
	(test-context-diff))
    (end-tests)))


;; Utility functions

(defun diff-opcodes (a b &key (test-function #'eql) matcher)
  (if matcher
      (difflib:set-sequences matcher a b)
      (setf matcher (make-instance 'difflib:sequence-matcher
				   :a a
				   :b b
				   :test-function test-function)))
  (difflib:get-opcodes matcher))

(defun grouped-opcodes (a b n &key (test-function #'eql) matcher)
  (if matcher
      (difflib:set-sequences matcher a b)
      (setf matcher (make-instance 'difflib:sequence-matcher
				   :a a
				   :b b
				   :test-function test-function)))
  (difflib:group-opcodes (difflib:get-opcodes matcher) n))

(defun diff-similarity-ratio (a b &key (test-function #'eql) junk matcher)
  (if matcher
      (difflib:set-sequences matcher a b)
      (setf matcher (make-instance 'difflib:sequence-matcher
				   :a a
				   :b b
				   :test-function test-function
				   :junk-function junk)))
  (difflib:similarity-ratio matcher))

(defun diff-quick-similarity-ratio (a b &key (test-function #'eql) junk matcher)
  (if matcher
      (difflib:set-sequences matcher a b)
      (setf matcher (make-instance 'difflib:sequence-matcher
				   :a a
				   :b b
				   :test-function test-function
				   :junk-function junk)))
  (difflib:quick-similarity-ratio matcher))

(defun diff-very-quick-similarity-ratio (a b &key (test-function #'eql) junk matcher)
  (if matcher
      (difflib:set-sequences matcher a b)
      (setf matcher (make-instance 'difflib:sequence-matcher
				   :a a
				   :b b
				   :test-function test-function
				   :junk-function junk)))
  (difflib:very-quick-similarity-ratio matcher))

(defun opcode (tag i1 i2 j1 j2)
  (difflib:make-opcode :tag tag :i1 i1 :i2 i2 :j1 j1 :j2 j2))

(defun opcodes-from-list (list)
  (mapcar #'(lambda (spec) (apply #'opcode spec))
	  list))

(defun opcode-groups-from-lists (lists)
  (mapcar #'opcodes-from-list lists))

(defun opcodes-equal (l1 l2)
  (and (= (length l1) (length l2))
       (every #'difflib:opcode= l1 l2)))

(defun opcode-groups-equal (l1 l2)
  (every #'opcodes-equal l1 l2))

(defun approx= (a b)
  (< (abs (- a b)) .001))



;; The tests themselves

(defun test-opcode-equality ()
  (test opcode-equality-1
	(difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
	(difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
	#'difflib:opcode=)
  (test opcode-equality-2
	(difflib:make-opcode :tag :delete :i1 1 :i2 2 :j1 3 :j2 4)
	(difflib:make-opcode :tag :delete :i1 1 :i2 2 :j1 5 :j2 6)
	#'difflib:opcode=)
  (test opcode-equality-3
	(difflib:make-opcode :tag :insert :i1 1 :i2 2 :j1 3 :j2 4)
	(difflib:make-opcode :tag :insert :i1 5 :i2 6 :j1 3 :j2 4)
	#'difflib:opcode=)
  (test opcode-equality-4
	(difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4)
	(difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4)
	#'difflib:opcode=)
  (test opcode-equality-5
	(difflib:opcode= (difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :replace :i1 5 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-6
	(difflib:opcode= (difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :replace :i1 5 :i2 2 :j1 5 :j2 4))
	NIL)
  (test opcode-equality-7
	(difflib:opcode= (difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :replace :i1 5 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-9
	(difflib:opcode= (difflib:make-opcode :tag :delete :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :delete :i1 5 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-10
	(difflib:opcode= (difflib:make-opcode :tag :delete :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :delete :i1 1 :i2 5 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-11
	(difflib:opcode= (difflib:make-opcode :tag :insert :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :insert :i1 1 :i2 2 :j1 5 :j2 4))
	NIL)
  (test opcode-equality-12
	(difflib:opcode= (difflib:make-opcode :tag :insert :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :insert :i1 1 :i2 2 :j1 3 :j2 5))
	NIL)
  (test opcode-equality-13
	(difflib:opcode= (difflib:make-opcode :tag :equal :i1 5 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-14
	(difflib:opcode= (difflib:make-opcode :tag :equal :i1 1 :i2 6 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-15
	(difflib:opcode= (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 5 :j2 4)
			 (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-16
	(difflib:opcode= (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 5)
			 (difflib:make-opcode :tag :equal :i1 1 :i2 2 :j1 3 :j2 4))
	NIL)
  (test opcode-equality-17
	(difflib:opcode= (difflib:make-opcode :tag :replace :i1 1 :i2 2 :j1 3 :j2 4)
			 (difflib:make-opcode :tag :equal   :i1 1 :i2 2 :j1 3 :j2 4))
	NIL))


(defun test-get-opcodes ()
  (test opcodes-1
	(diff-opcodes "123456789"
		      "12346789")
	(opcodes-from-list '((:equal 0 4 0 4)
			     (:delete 4 5 4 4)
			     (:equal 5 9 4 8)))
	#'opcodes-equal)
  (test opcodes-2
	(diff-opcodes "how i enjoy the music"
		      "how i like the music")
	(opcodes-from-list '((:equal 0 6 0 6)
			     (:insert 6 6 6 9)
			     (:equal 6 7 9 10)
			     (:delete 7 11 10 10)
			     (:equal 11 21 10 20)))
	#'opcodes-equal)
  (test opcodes-3
	(diff-opcodes "living insuide a tomato"
		      "living inside a tomato")
	(opcodes-from-list '((:equal 0 10 0 10)
			     (:delete 10 11 10 10)
			     (:equal 11 23 10 22)))
	#'opcodes-equal)
  (test opcodes-4
	(diff-opcodes "123456789"
		      "987654321")
	(opcodes-from-list '((:insert 0 0 0 8)
			     (:equal 0 1 8 9)
			     (:delete 1 9 9 9)))
	#'opcodes-equal)
  (test opcodes-5
	(diff-opcodes "running into a mountain"
		      "ruining my mountain boy")
	(opcodes-from-list '((:equal 0 2 0 2)
			     (:replace 2 3 2 3)
			     (:equal 3 8 3 8)
			     (:replace 8 14 8 10)
			     (:equal 14 23 10 19)
			     (:insert 23 23 19 23)))
	#'opcodes-equal)
  (test opcodes-6
	(diff-opcodes "one two three four five six seven"
		      "big three two four five apricot seven")
	(opcodes-from-list '((:replace 0 7 0 3)
			     (:equal 7 13 3 9)
			     (:insert 13 13 9 13)
			     (:equal 13 24 13 24)
			     (:replace 24 25 24 27)
			     (:equal 25 26 27 28)
			     (:replace 26 27 28 31)
			     (:equal 27 33 31 37)))
	#'opcodes-equal)
  (test opcodes-7
	(diff-opcodes '(1 2 3 4 5 6 7 8 9)
		      '(1 2 3 4 6 7 8 9))
	(diff-opcodes "123456789"
		      "12346789")
	#'opcodes-equal)
  (test opcodes-8
	(diff-opcodes '(1 2 3 4 5 6 7 8 9)
		      '(1 2 3 4 6 7 8 9))
	(diff-opcodes #(1 2 3 4 5 6 7 8 9)
		      #(1 2 3 4 6 7 8 9))
	#'opcodes-equal)
  (let ((s1 (make-array (list 1000))))
    (dotimes (i (length s1))
      (setf (elt s1 i) i))
    (let ((s2 (copy-seq s1)))
      (setf (elt s2 500) 5)
      (test opcodes-9
	    (diff-opcodes s1 s2)
	    (opcodes-from-list '((:EQUAL 0 500 0 500)
				 (:REPLACE 500 501 500 501)
				 (:EQUAL 501 1000 501 1000)))
	    #'opcodes-equal)))
  (let ((s1 (make-array (list 1000) :initial-element 1)))
    (let ((s2 (copy-seq s1)))
      (setf (elt s2 500) 5)
      (test opcodes-10
	    (diff-opcodes s1 s2)
	    (opcodes-from-list '((:EQUAL 0 500 0 500) (:REPLACE 500 1000 500 1000)))
	    #'opcodes-equal)))
  (let ((matcher (make-instance 'difflib:sequence-matcher)))
    (test opcodes-11
	  (diff-opcodes "123456789"
			"12346789"
			:matcher matcher)
	  (opcodes-from-list '((:equal 0 4 0 4)
			       (:delete 4 5 4 4)
			       (:equal 5 9 4 8)))
	  #'opcodes-equal)
    (test opcodes-12
	  (diff-opcodes "how i enjoy the music"
			"how i like the music"
			:matcher matcher)
	  (opcodes-from-list '((:equal 0 6 0 6)
			       (:insert 6 6 6 9)
			       (:equal 6 7 9 10)
			       (:delete 7 11 10 10)
			       (:equal 11 21 10 20)))
	  #'opcodes-equal)))


(defun test-get-grouped-opcodes ()
  (test grouped-opcodes-1
	(grouped-opcodes '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"
			   "13" "14" "15" "16" "17" "18" "19" "20" "21" "22"
			   "23" "24" "25" "26" "27" "28" "29" "30" "31" "32"
			   "33" "34" "35" "36" "37" "38" "39")
			 '("1" "2" "3" "4" "5" "6" "7" "8" "i" "9" "10" "11"
			   "12" "13" "14" "15" "16" "17" "18" "19" "20x" "21"
			   "22" "28" "29" "30" "31" "32" "33" "34" "35y" "36"
			   "37" "38" "39")
			 3
			 :test-function #'equal)
	(opcode-groups-from-lists '(((:equal 5 8 5 8) (:insert 8 8 8 9) (:equal 8 11 9 12))
				    ((:equal 16 19 17 20) (:replace 19 20 20 21)
				     (:equal 20 22 21 23) (:delete 22 27 23 23) (:equal 27 30 23 26))
				    ((:equal 31 34 27 30) (:replace 34 35 30 31) (:equal 35 38 31 34))))
	#'opcode-groups-equal))


(defun test-similarity-ratio ()
  (test ratio-1
	(diff-similarity-ratio "abcd" "bcde")
	0.75
	#'approx=)
  (test ratio-2
	(diff-similarity-ratio "private Thread currentThread;"
			       "private volatile Thread currentThread;"
			       :junk #'(lambda (c) (eql c #\space)))
	0.866
	#'approx=)
  (test ratio-3
	(diff-quick-similarity-ratio "abcd" "bcde")
	0.75
	#'approx=)
  (test ratio-4
	(diff-very-quick-similarity-ratio "abcd" "bcde")
	1
	#'approx=))


(defun test-close-matches ()
  (test close-matches-1
	(difflib:get-close-matches "appel" '("ape" "apple" "peach" "puppy"))
	'("apple" "ape")
	#'equal))


(defun test-unified-diff ()
  (test unified-diff-1
	(with-output-to-string (s)
	  (difflib:unified-diff s
				'("1" "2" "3" "4" "5")
				'("1" "2" "8" "7" "5" "6")
				:test-function #'equal
				:from-file "Original.txt"
				:from-file-date "Wed Feb 02 21:28:00 2005"
				:to-file "Modified.txt"
				:to-file-date "Wed Feb 02 21:28:01 2005"))
	(with-output-to-string (s)
	  (dolist (line '("--- Original.txt Wed Feb 02 21:28:00 2005"   
			  "+++ Modified.txt Wed Feb 02 21:28:01 2005"
			  "@@ -1,5 +1,6 @@"
			  " 1"
			  " 2"
			  "-3"
			  "-4"
			  "+8"
			  "+7"
			  " 5"
			  "+6"))
	    (format s "~&~A" line)))
	#'string=))


(defun test-context-diff ()
  (test context-diff-1
	(with-output-to-string (s)
	  (difflib:context-diff s
				'("one" "two" "three" "four")
				'("zero" "one" "tree" "four")
				:test-function #'equal
				:from-file "Original.txt"
				:from-file-date "Wed Feb 02 21:28:00 2005"
				:to-file "Modified.txt"
				:to-file-date "Wed Feb 02 21:28:01 2005"))
	(with-output-to-string (s)
	  (dolist (line '("*** Original.txt Wed Feb 02 21:28:00 2005"
			  "--- Modified.txt Wed Feb 02 21:28:01 2005"
			  "***************"
			  "*** 1,4 ***"
			  "  one"
			  "! two"
			  "! three"
			  "  four"
			  "--- 1,4 ----"
			  "+ zero"
			  "  one"
			  "! tree"
			  "  four"))
	    (format s "~&~A" line)))
	#'string=))
