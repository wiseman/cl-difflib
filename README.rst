cl-difflib
==========

cl-difflib is a Common Lisp library for computing differences between
pairs of sequences. It is nearly a transcription of Python's difflib_
module, which contains the following description of its algorithm:

  The basic algorithm predates, and is a little fancier than, an
  algorithm published in the late 1980's by Ratcliff and Obershelp
  under the hyperbolic name "gestalt pattern matching". The basic idea
  is to find the longest contiguous matching subsequence that contains
  no "junk" elements (R-O doesn't address junk). The same idea is then
  applied recursively to the pieces of the sequences to the left and
  to the right of the matching subsequence. This does not yield
  minimal edit sequences, but does tend to yield matches that "look
  right" to people.

cl-difflib can do unified diffs:

  CL-USER> (unified-diff *standard-output*
                         '("one" "two" "three" "four" "five" "six")
                         '("one" "three" "four" "seven" "six")
                         :test-function #'equal)
  ---  
  +++  
  @@ -1,6 +1,5 @@
   one
  -two
   three
   four
  -five
  +seven
   six
  ; No value


And it can do context diffs:

  CL-USER> (context-diff *standard-output*
			 '("one" "two" "three" "four" "five" "six")
			 '("one" "three" "four" "seven" "six")
			 :test-function #'equal)
  ***  
  ---  
  ***************
  *** 1,6 ***
    one
  - two
    three
    four
  ! five
    six
  --- 1,5 ----
    one
    three
    four
  ! seven
    six
  ; No value


It should work on any sequence (as long as the elements can be
properly kept track of in a hash table--that's what the
``:test-function`` argument is for):

  CL-USER> (context-diff *standard-output*
		 #(1 2 3 4 5 6)
		 #(1 3 4 7 6)
		 :from-file "original.fake"
		 :from-file-date "Mon Sep 27 14:13:57 2004"
		 :to-file "modified.fake"
		 :to-file-date "Thu Jan 13 15:55:05 2005")
  *** original.fake Mon Sep 27 14:13:57 2004
  --- modified.fake Thu Jan 13 15:55:05 2005
  ***************
  *** 1,6 ***
    1
  - 2
    3
    4
  ! 5
    6
  --- 1,5 ----
    1
    3
    4
  ! 7
    6
  ; No value

It's got some similarity measures:

  CL-USER> (defparameter *lisp-symbols* '())
  *LISP-SYMBOLS*
  CL-USER> (do-external-symbols (sym "COMMON-LISP")
	     (push (symbol-name sym) *lisp-symbols*))
  NIL
  CL-USER> (get-close-matches "PRING" *lisp-symbols*)
  ("PRINC" "PRINT" "PRIN1")
  CL-USER> (get-close-matches "SPROING" *lisp-symbols*)
  ("STRING" "PROG" "STRINGP")
  CL-USER> (get-close-matches "WITH-HASHING-TABLE" *lisp-symbols*)
  ("HASH-TABLE" "WITH-HASH-TABLE-ITERATOR" "HASH-TABLE-P")

And it's got some lower level building blocks:

  CL-USER> (let ((m (make-instance 'sequence-matcher
				   :a '("one" "two" "three" "four" "five" "six")
				   :b '("one" "three" "four" "seven" "six")
				   :test-function #'equal)))
	     (pprint (get-opcodes m)))

  (#<OPCODE :EQUAL 0 1 0 1> #<OPCODE :DELETE 1 2 1 1> #<OPCODE :EQUAL 2 4 1 3>
   #<OPCODE :REPLACE 4 5 3 4> #<OPCODE :EQUAL 5 6 4 5>)
  ; No value

Some things it doesn't have: Python ``difflib``'s ``Differ`` class,
and the ``ndiff`` or ``restore`` functions (maybe someday). It also
hasn't been performance tuned.  It has some ugly bits of code that
look a little Pythonesque. I did change a few things to be more Lispy,
but most of the documentation for the Python module should still be
applicable.

cl-difflib is pretty simple, but the diffs it generates look
decent. It should be completely portable Lisp. And, best of all, you
can download it via ``ASDF-INSTALL``.

(Nathan Froyd has some `diff code`_ that looks more complicated than
this.)



.. _difflib: http://docs.python.org/library/difflib.html
.. _diff code: http://www.cliki.net/diff
