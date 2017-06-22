(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defparameter *trace-facts*
  `((10 "kCachegrind")))


(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))

(defmacro er (&body body)
  `(statements (<< "std::cerr" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
	 `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))))


(let ((code `(with-compilation-unit
	       (include <fstream>)
	       (include <iostream>)
	       
	       ,@(dox :brief ""
		      :usage ""
		      :params '((initialize "")
				)
		      :return "")
	       
	       
	       (function (main ((argc :type int)
				(argv :type char**)) int)
			 
			 (return 0)))))
  (write-source "stage/cl-gen-magnum/source/main" "cpp" code)
  #+nil(sb-ext:run-program "/bin/sh" '("/home/martin/stage/cl-gen-lens-trace-quad/run.sh")))


