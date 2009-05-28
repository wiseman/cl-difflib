;;; --------------------------------------------------------------------
;;; CL-DIFFLIB -- A Lisp library for computing differences between
;;; sequences.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: difflib.lisp,v 1.11 2005/02/24 20:05:52 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This is nearly a transcription of Python's difflib module, which
;;; contains the following description of its algorithm:
;;;
;;;     The basic algorithm predates, and is a little fancier than, an
;;;     algorithm published in the late 1980's by Ratcliff and
;;;     Obershelp under the hyperbolic name "gestalt pattern
;;;     matching".  The basic idea is to find the longest contiguous
;;;     matching subsequence that contains no "junk" elements (R-O
;;;     doesn't address junk).  The same idea is then applied
;;;     recursively to the pieces of the sequences to the left and to
;;;     the right of the matching subsequence.  This does not yield
;;;     minimal edit sequences, but does tend to yield matches that
;;;     "look right" to people.

(in-package :difflib)

;; A couple utility macros

;; Hey, it's just like Python's enumerate function!
(defmacro enumerate ((index-var elt-var sequence &optional result-form) &body body)
  "Iterates over a sequence while keeping track of an index.

  (enumerate (i e '(a b c))
    (format T \"~&~S ~S\" i e))
  =>
  1 a
  2 b
  3 c"
  (let ((sequence-var (gensym "SEQUENCE")))
    `(let ((,sequence-var ,sequence))
       (dotimes (,index-var (length ,sequence-var) ,result-form)
	 (let ((,elt-var (elt ,sequence-var ,index-var)))
	   ,@body)))))

;; Hey, it's just like Python's range function!
(defmacro do-range ((var start-form end-form &optional result-form) &body body)
  "Iterates VAR through the range of integers in [START-FORM,
  END-FORM).  Returns the value of END-FORM (at the time END-FORM is
  evaluated, VAR is bound to the value of END-FORM.

  (do-range (i 10 (length s))
    (print (elt s i)))"
  (let ((start-var (gensym))
	(end-var (gensym)))
    `(let ((,start-var ,start-form)
	   (,end-var ,end-form))
       (do ((,var ,start-var (1+ ,var)))
	   ((>= ,var ,end-var) ,result-form)
	 ,@body))))



(defstruct opcode
  "A single instruction for modifying sequence A into sequence B,
  where TAG has the following possible values and meanings:

    :REPLACE  a[i1:i2] should be replaced by b[j1:j2]
    :DELETE   a[i1:i2] should be deleted
    :INSERT   b[j2:j2] should be inserted
    :EQUAL    a[i1:i2] = b[j1:j2]"    
  tag
  i1
  i2
  j1
  j2)

(defmethod print-object ((self opcode) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "~S ~S ~S ~S ~S"
	    (opcode-tag self)
	    (opcode-i1 self)
	    (opcode-i2 self)
	    (opcode-j1 self)
	    (opcode-j2 self))))

(defun opcode-range (opcode)
  (with-slots (i1 i2 j1 j2) opcode
    (ecase (opcode-tag opcode)
      ((:replace :equal)
       (values i1 i2 j1 j2))
      ((:delete)
       (values i1 i2))
      ((:insert)
       (values j1 j2)))))

(defun opcode= (op1 op2)
  "Tests two opcodes for equality."
  (and (eq (opcode-tag op1) (opcode-tag op2))
       (multiple-value-bind (op1-lo1 op1-hi1 op1-lo2 op1-hi2)
	   (opcode-range op1)
	 (multiple-value-bind (op2-lo1 op2-hi1 op2-lo2 op2-hi2)
	     (opcode-range op2)
	   (and (eql op1-lo1 op2-lo1)
		(eql op1-hi1 op2-hi1)
		(eql op1-lo2 op2-lo2)
		(eql op1-hi2 op2-hi2))))))
	    

(defclass sequence-matcher ()
  (;; User-supplied slots
   (a                  :initform nil   :initarg :a             :accessor sequence-a)
   (b                  :initform nil   :initarg :b             :accessor sequence-b)
   (junk-function      :initform nil   :initarg :junk-function :accessor junk-function)
   (test-function      :initform #'eql :initarg :test-function :accessor test-function)
   ;; Intermediate data
   (b-junk-function    :initform nil :accessor b-junk-function)
   (b2j                :initform nil :accessor b2j)
   (b-popular-function :initform nil :accessor b-popular-function)
   ;; Cache slots
   (opcodes            :initform nil :accessor opcodes)
   (matching-blocks    :initform nil :accessor matching-blocks)
   (full-b-count       :initform nil :accessor full-b-count))
  (:documentation "The sequence-matcher class compares pairs of
  sequences.  The main restriction is that sequence elements must
  be hashable (use :test-function to specify the type of
  hashtable)."))

(defgeneric set-sequences (matcher seq-a seq-b &optional force-p))
(defgeneric set-sequence-a (matcher seq &optional force-p))
(defgeneric set-sequence-b (matcher seq &optional force-p))
(defgeneric chain-b (matcher))
(defgeneric find-longest-match (matcher alo ahi blo bhi))
(defgeneric get-matching-blocks (matcher))
(defgeneric helper (matcher alo ahi blo bhi answer))
(defgeneric get-opcodes (matcher))
(defgeneric similarity-ratio (matcher))
(defgeneric quick-similarity-ratio (matcher))
(defgeneric very-quick-similarity-ratio (matcher))


(defmethod initialize-instance :after ((self sequence-matcher) &key)
  (set-sequences self (sequence-a self) (sequence-b self) T))


(defmethod set-sequences ((self sequence-matcher) a b &optional force-p)
  (set-sequence-a self a force-p)
  (set-sequence-b self b force-p))


(defmethod set-sequence-a ((self sequence-matcher) a &optional force-p)
  (unless (and (not force-p) (eq a (sequence-a self)))
    (setf (sequence-a self) a)
    (setf (matching-blocks self) '())
    (setf (opcodes self) '())))


(defmethod set-sequence-b ((self sequence-matcher) b &optional force-p)
  (unless (and (not force-p) (eq b (sequence-b self)))
    (setf (sequence-b self) b)
    (setf (matching-blocks self) '())
    (setf (opcodes self) '())
    (setf (full-b-count self) nil)
    (chain-b self)))

(defmethod chain-b ((self sequence-matcher))
  (let* ((b (sequence-b self))
	 (n (length b))
	 (b2j (setf (b2j self) (make-hash-table :test (test-function self))))
	 (popular (make-hash-table :test (test-function self))))
    (enumerate (i elt b)
      (if (has-key elt b2j)
	  (let ((indices (gethash elt b2j)))
	    (if (and (>= n 200) (> (* (length indices) 100) n))
		(progn
		  (setf (gethash elt popular) T)
		  ;; Clear indices
		  (setf (gethash elt b2j) '()))
		  (setf (gethash elt b2j) (append indices (list i)))))
	  (setf (gethash elt b2j) (list i))))
    ;; Purge leftover indices for popular elements.
    (maphash #'(lambda (elt v)
		 (declare (ignore v))
		 (setf (gethash elt b2j) '()))
	     popular)
    ;; Now b2j.keys() contains elements uniquely, and especially when
    ;; the sequence is a string, that's usually a good deal smaller
    ;; than len(string).  The difference is the number of isjunk calls
    ;; saved.
    (let ((junk-function (junk-function self))
	  (junk (make-hash-table :test (test-function self))))
      (when junk-function
	(mapc #'(lambda (hash)
		  (maphash #'(lambda (elt v)
			       (declare (ignore v))
			       (when (funcall junk-function elt)
				 (setf (gethash elt junk) T)
				 (setf (gethash elt hash) '())))
			   hash))
	      (list popular b2j)))
      ;; Now for x in b, isjunk(x) == x in junkdict, but the latter is
      ;; much faster.  Note too that while there may be a lot of junk in
      ;; the sequence, the number of *unique* junk elements is probably
      ;; small.  So the memory burden of keeping this dict alive is
      ;; likely trivial compared to the size of b2j.
      (if junk-function
	  (setf (b-junk-function self)
		#'(lambda (elt) (has-key elt junk)))
	  (setf (b-junk-function self)
		#'(lambda (elt) (declare (ignore elt)) NIL)))
      (setf (b-popular-function self)
	    #'(lambda (elt) (has-key elt popular))))))
  

(defmethod find-longest-match ((self sequence-matcher) alo ahi blo bhi)
  (let ((test-function (test-function self)))
    (let ((a (sequence-a self))
	  (b (sequence-b self))
	  (b2j (b2j self))
	  (not-junk-function-p (not (junk-function self)))
	  (b-junk-function (b-junk-function self))
	  (best-i alo)
	  (best-j blo)
	  (best-size 0)
	  ;; find longest junk-free match
	  ;; during an iteration of the loop, j2len[j] = length of longest
	  ;; junk-free match ending with a[i-1] and b[j]
	  (j2len (make-hash-table :test test-function)))
      (do-range (i alo ahi)
	;; look at all instances of a[i] in b; note that because
	;; b2j has no junk keys, the loop is skipped if a[i] is junk
	(let ((newj2len (make-hash-table :test test-function)))
	  (tagbody
	     (dolist (j (gethash (elt a i) b2j '()))
	       ;; a[i] matches b[j]
	       (unless (< j blo)
		 (when (>= j bhi)
		   (go continue))
		 (let ((k (setf (gethash j newj2len) (+ (gethash (- j 1) j2len 0) 1))))
		   (when (> k best-size)
		     (setf best-i (+ (- i k) 1)
			   best-j (+ (- j k) 1)
			   best-size k)))))
	   continue
	     (setf j2len newj2len))
	  
	  ;; Extend the best by non-junk elements on each end.  In particular,
	  ;; "popular" non-junk elements aren't in b2j, which greatly speeds
	  ;; the inner loop above, but also means "the best" match so far
	  ;; doesn't contain any junk *or* popular non-junk elements.
	  (loop while (and (> best-i alo)
			   (> best-j blo)
			   (or not-junk-function-p (not (funcall b-junk-function (elt b (- best-j 1)))))
			   (funcall test-function (elt a (- best-i 1)) (elt b (- best-j 1))))
	     do (decf best-i)
	       (decf best-j)
	       (incf best-size))
	  (loop while (and (< (+ best-i best-size) ahi)
			   (< (+ best-j best-size) bhi)
			   (or not-junk-function-p (not (funcall b-junk-function (elt b (+ best-j best-size)))))
			   (funcall test-function (elt a (+ best-i best-size)) (elt b (+ best-j best-size))))
	     do (incf best-size))
	  
	  ;; Now that we have a wholly interesting match (albeit
	  ;; possibly empty!), we may as well suck up the matching junk
	  ;; on each side of it too.  Can't think of a good reason not
	  ;; to, and it saves post-processing the (possibly
	  ;; considerable) expense of figuring out what to do with it.
	  ;; In the case of an empty interesting match, this is clearly
	  ;; the right thing to do, because no other kind of match is
	  ;; possible in the regions.
	  (loop while (and (> best-i alo)
			   (> best-j blo)
			   (funcall b-junk-function (elt b (- best-j 1)))
			   (funcall test-function (elt a (- best-i 1)) (elt b (- best-j 1))))
	     do (decf best-i)
	       (decf best-j)
	       (incf best-size))
	  (loop while (and (< (+ best-i best-size) ahi)
			   (< (+ best-j best-size) bhi)
			   (funcall b-junk-function (elt b (+ best-j best-size)))
			   (funcall test-function (elt a (+ best-i best-size)) (elt b (+ best-j best-size))))
	     do (incf best-size))))
      (values best-i best-j best-size))))
  
(defmethod get-matching-blocks ((self sequence-matcher))
  (if (matching-blocks self)
      (matching-blocks self)
      (let ((matching-blocks '())
	    (la (length (sequence-a self)))
	    (lb (length (sequence-b self))))
	(setf matching-blocks (helper self 0 la 0 lb matching-blocks))
	(setf matching-blocks (append matching-blocks (list
						       (list la lb 0))))
	(setf (matching-blocks self) matching-blocks))))


(defmethod helper ((self sequence-matcher) alo ahi blo bhi answer)
  (multiple-value-bind (i j k)
      (find-longest-match self alo ahi blo bhi)
    (let ((x (list i j k)))
      ;; a[alo:i] vs b[blo:j] unknown
      ;; a[i:i+k] same as b[j:j+k]
      ;; a[i+k:ahi] vs b[j+k:bhi] unknown
      (when (not (= k 0))
	(when (and (< alo i) (< blo j))
	  (setf answer (helper self alo i blo j answer)))
	(setf answer (append answer (list x)))
	(when (and (< (+ i k) ahi) (< (+ j k) bhi))
	  (setf answer (helper self (+ i k) ahi (+ j k) bhi answer))))
      answer)))

(defmethod get-opcodes ((self sequence-matcher))
  (if (opcodes self)
      (opcodes self)
      (let ((i 0)
	    (j 0)
	    (opcodes '()))
	(dolist (block (get-matching-blocks self))
	  (destructuring-bind (ai bj size) block
	    ;; invariant: we've pumped out correct diffs to change
            ;; a[:i] into b[:j], and the next matching block is
            ;; a[ai:ai+size] == b[bj:bj+size].  So we need to pump out
            ;; a diff to change a[i:ai] into b[j:bj], pump out the
            ;; matching block, and move (i,j) beyond the match
	    (let ((tag nil))
	      (cond ((and (< i ai) (< j bj))
		     (setf tag :replace))
		    ((< i ai)
		     (setf tag :delete))
		    ((< j bj)
		     (setf tag :insert)))
	      (when tag
		(let ((opcode (make-opcode :tag tag :i1 i :i2 ai :j1 j :j2 bj)))
		  (push opcode opcodes)))
	      (setf i (+ ai size))
	      (setf j (+ bj size))
	      ;; the list of matching blocks is terminated by a
	      ;; sentinel with size 0
	      (when (not (= size 0))
		(push (make-opcode :tag :equal :i1 ai :i2 i :j1 bj :j2 j) opcodes)))))
	(setf (opcodes self) (reverse opcodes)))))



(defun group-opcodes (opcodes n)
  (let ((o (first opcodes)))
    (when (eq (opcode-tag o) :equal)
      (setf opcodes
	    (append (list (make-opcode :tag (opcode-tag o)
				       :i1 (max (opcode-i1 o) (- (opcode-i2 o) n))
				       :i2 (opcode-i2 o)
				       :j1 (max (opcode-j1 o) (- (opcode-j2 o) n))
				       :j2 (opcode-j2 o)))
		    (rest opcodes)))))
  (let ((o (car (last opcodes))))
    (when (eq (opcode-tag o) :equal)
      (setf opcodes
	    (append
	     (butlast opcodes)
	     (list (make-opcode :tag (opcode-tag o)
				:i1 (opcode-i1 o)
				:i2 (min (opcode-i2 o) (+ (opcode-i1 o) n))
				:j1 (opcode-j1 o)
				:j2 (min (opcode-j2 o) (+ (opcode-j1 o) n))))))))
  (let ((nn (* n 2))
	(group '())
	(groups '()))
    (dolist (o opcodes)
      (with-slots (tag i1 i2 j1 j2) o
	(if (and (eq tag :equal) (> (- i2 i1) nn))
	    (progn
	      (push (make-opcode :tag tag :i1 i1 :i2 (min i2 (+ i1 n)) :j1 j1 :j2 (min j2 (+ j1 n)))
		    group)
	      (push (reverse group) groups)
	      (setf group '())
	      (push (make-opcode :tag tag :i1 (max i1 (- i2 n)) :i2 i2
				 :j1 (max j1 (- j2 n)) :j2 j2)
		    group))
	    (push o group))))
    (when (and group
	       (not (and (= (length group) 1)
			 (eq (opcode-tag (first group)) :equal))))
      (push (reverse group) groups))
    (reverse groups)))

(defmethod similarity-ratio ((self sequence-matcher))
  "Returns a measure of the sequences' similarity (a value in [0,
  1])."
  (let ((matches (reduce #'(lambda (sum trip)
			     (+ sum trip))
			 (get-matching-blocks self)
			 :key #'(lambda (triple) (elt triple 2)))))
    (calculate-similarity-ratio matches (+ (length (sequence-a self))
					   (length (sequence-b self))))))


(defun calculate-similarity-ratio (matches length)
  (if length
      (/ (* 2 matches) length)
      1))

(defmethod quick-similarity-ratio ((self sequence-matcher))
  (unless (full-b-count self)
    (setf (full-b-count self)
	  (make-hash-table :test (test-function self))))
  (let ((full-b-count (full-b-count self))
	(a (sequence-a self))
	(b (sequence-b self)))
    (dotimes (i (length b))
      (let ((e (elt b i)))
	(incf (gethash e full-b-count 0))))
    (let ((avail (make-hash-table :test (test-function self))))
      (let ((matches 0))
	(dotimes (i (length a))
	  (let ((e (elt a i)))
	    (let ((numb (multiple-value-bind (count has-key)
			    (gethash e avail)
			  (if has-key
			      count
			      (gethash e full-b-count 0)))))
	      (setf (gethash e avail) (- numb 1))
	      (when (> numb 0)
		(incf matches)))))
	(calculate-similarity-ratio matches (+ (length a) (length b)))))))

(defmethod very-quick-similarity-ratio ((self sequence-matcher))
  (let ((la (length (sequence-a self)))
	(lb (length (sequence-b self))))
    (calculate-similarity-ratio (min la lb) (+ la lb))))

(defun get-close-matches (word possibilities &key (max 3) (cutoff 0.6))
  (let ((matcher (make-instance 'sequence-matcher))
	(matches '()))
    (set-sequence-b matcher word)
    (dolist (p possibilities)
      (set-sequence-a matcher p)
      ;; Just as an example of the benefit of the approximate ratio
      ;; functions, when comparing the string "RUN" to the symbol
      ;; names of all symbols external to the COMMON-LISP package, the
      ;; performance is as follows:
      ;;
      ;;   Optimization                  Time                 Memory
      ;;   ----------------------------------------------------------------
      ;;   similarity-ratio              1269             15,291,880
      ;;   quick-similarity-ratio         165 (0.13)       1,074,712 (0.07)
      ;;   very-quick-similarity-ratio     54 (0.04)         336,456 (0.02)
      (when (and (>= (very-quick-similarity-ratio matcher) cutoff)
		 (>= (quick-similarity-ratio matcher) cutoff)
		 (>= (similarity-ratio matcher) cutoff))
	(push (cons p (similarity-ratio matcher)) matches)))
    (setf matches (sort matches #'> :key #'cdr))
    (mapcar #'car
	    (subseq matches 0 (min (length matches) max)))))


(defun has-key (key hash)
  "Checks whether a key value is present in a hash table."
  (multiple-value-bind (val in-p)
      (gethash key hash) 
    (declare (ignore val))
    in-p))



#||
(defclass differ ()
  ((line-junk-function :initform nil :initarg :line-junk-function :accessor line-junk-function)
   (char-junk-function :initform nil :initarg :char-junk-function :accessor char-junk-function)))

(defmethod compare ((self differ) a b stream)
  (let ((matcher (make-instance 'sequence-matcher
				:a a
				:b b
				:junk-function (line-junk-function self))))
    (let ((opcodes (get-opcodes matcher)))
      (dolist (op opcodes)
	(ecase (opcode-tag op)
	  ((:replace)
	   (fancy-replace stream a b op))
	  ((:delete)
	   (dump stream #\- a op))
	  ((:insert)
	   (dump stream #\+ b op))
	  ((:equal)
	   (dump stream #\space a op)))))))

(defun dump (stream char seq op)
  (multiple-value-bind (lo hi)
      (opcode-range op)
    (do-range (i lo hi)
      (format stream "~&~A ~A" char (elt seq i)))))
||#

#||
(defun file-lines (path)
  (with-input-from-stream (in path :direction :input)
    (let ((lines '()))
      (do ((line (read-line path nil :eof)
		 (read-line path nil :eof)))
	  ((eq line :eof) (reverse lines))
	(push line lines)))))

			    
(defun unified-diff-files (out-stream path-a path-b &key (n 3))
  (let ((a (file-lines path-a))
	(b (file-lines path-b)))
    (unified-diff out-stream a b :n n)))
||#


(defun unified-diff (stream a b &key from-file to-file
		                     from-file-date to-file-date (n 3)
		                     (test-function #'eql) junk-function)
  (let ((started NIL)
	(matcher (make-instance 'sequence-matcher
				:a a
				:b b
				:junk-function junk-function
				:test-function test-function)))
    (dolist (group (group-opcodes (get-opcodes matcher) n))
      (unless started
	(format stream "~&--- ~A ~A" (or from-file "") (or from-file-date ""))
	(format stream "~&+++ ~A ~A" (or to-file "") (or to-file-date ""))
	(setf started T))
      (let ((i1 (opcode-i1 (elt group 0)))
	    (i2 (opcode-i2 (elt group (- (length group) 1))))
	    (j1 (opcode-j1 (elt group 0)))
	    (j2 (opcode-j2 (elt group (- (length group) 1)))))
	(format stream "~&@@ -~D,~D +~D,~D @@" (+ i1 1) (- i2 i1) (+ j1 1) (- j2 j1))
	(dolist (op group)
	  (with-slots (tag i1 i2 j1 j2) op
	    (when (eq tag :equal)
	      (map 'nil
		   #'(lambda (line)
		       (format stream "~& ~A" line))
		   (subseq a i1 i2)))
	    (when (member tag '(:replace :delete))
	      (map 'nil
		   #'(lambda (line)
		       (format stream "~&-~A" line))
		   (subseq a i1 i2)))
	    (when (member tag '(:replace :insert))
	      (map 'nil
		   #'(lambda (line)
		       (format stream "~&+~A" line))
		   (subseq b j1 j2))))))))
  (values))

(defun context-diff (stream a b &key from-file to-file
		                     from-file-date to-file-date (n 3)
		                     (test-function #'eql) junk-function)
  (flet ((some-opcodes-contain (ops tags)
	   (find-if #'(lambda (op)
			(member (opcode-tag op) tags))
		    ops)))
    (let ((started NIL)
	  (prefix-map '((:insert  . "+")
			(:delete  . "-")
			(:replace . "!")
			(:equal   . " ")))
	  (matcher (make-instance 'sequence-matcher
				  :a a
				  :b b
				  :junk-function junk-function
				  :test-function test-function)))
      (dolist (group (group-opcodes (get-opcodes matcher) n))
	(when (not started)
	  (format stream "~&*** ~A ~A" (or from-file "") (or from-file-date ""))
	  (format stream "~&--- ~A ~A" (or to-file "") (or to-file-date ""))
	  (setf started T))
	(format stream "~&***************" )
	(let ((first-op (first group))
	      (last-op (car (last group))))
	  (if (>= (- (opcode-i2 last-op)
		     (opcode-i1 first-op))
		  2)
	      (format stream "~&*** ~D,~D ***"
		      (+ (opcode-i1 first-op) 1)
		      (opcode-i2 last-op))
	      (format stream "~&*** ~D ***" (opcode-i2 last-op)))
	  (when (some-opcodes-contain group '(:replace :delete))
	    (dolist (opcode group)
	      (unless (eq (opcode-tag opcode) :insert)
		(map 'nil
		     #'(lambda (line)
			 (format stream "~&~A ~A" 
				 (cdr (assoc (opcode-tag opcode) prefix-map))
				 line))
		     (subseq a (opcode-i1 opcode) (opcode-i2 opcode))))))
	  (if (>= (- (opcode-j2 last-op)
		     (opcode-j1 first-op))
		  2)
	      (format stream "~&--- ~D,~D ----"
		      (+ (opcode-j1 first-op) 1)
		      (opcode-j2 last-op))
	      (format stream "~&--- ~D ----" (opcode-j2 last-op)))
	  (when (some-opcodes-contain group '(:replace :insert))
	    (dolist (opcode group)
	      (unless (eq (opcode-tag opcode) :delete)
		(map 'nil
		     #'(lambda (line)
			 (format stream "~&~A ~A"
				 (cdr (assoc (opcode-tag opcode) prefix-map))
				 line))
		     (subseq b (opcode-j1 opcode) (opcode-j2 opcode))))))))))
  (values))
