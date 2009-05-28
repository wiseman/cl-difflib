;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CL-DIFFLIB -- A Lisp library for computing differences between
;;; sequences.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: cl-difflib-tests.asd,v 1.4 2005/02/24 20:05:52 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This is the ASDF system definition for the unit tests.

(in-package :asdf)

(asdf:operate 'asdf:load-op :cl-difflib)


(asdf:defsystem :cl-difflib-tests
    :depends-on (:cl-difflib)
    :components ((:file "unit-tests")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :cl-difflib-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:difflib-test)))
      (error "test-op failed")))
