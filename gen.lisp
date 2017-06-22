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
	       (include <Magnum/DefaultFramebuffer.h>)
	       (include <Magnum/Platform/Sdl2Application.h>)

	       (raw "using namespace Magnum;")

	       (class MyApplication ("public Platform::Application")
		      (access-specifier public)
		      (function (MyApplication ((arguments :type "const Arguments&")) explicit))
		      (access-specifier private)
		      (function (drawEvent () void :specifier override)))

	       
	       
	       ,@(dox :brief ""
		      :usage ""
		      :params '((arguments "")
				)
		      :return "")

	       ;; FIXME: I should add list initialization
	       ;; MyApplication::MyApplication(const Arguments& arguments): Platform::Application{arguments} {
	       ;; // TODO: Add your initialization code here
	       ;; }
	       (function ("MyApplication::MyApplication" ((arguments :type "const Arguments&")) nil
							 :ctor (("Platform::Application" arguments)))
			 (raw "// add some code")
			 (raw ""))

	       (function ("MyApplication::drawEvent" () void)
			  (funcall defaultFramebuffer.clear
				   "FramebufferClear::Color")
			  (funcall swapBuffers))
	       (funcall MAGNUM_APPLICATION_MAIN MyApplication)
	       #+nil
	       (function (main ((argc :type int)
				(argv :type char**)) int)
			 
			 (return 0)))))
  (write-source "stage/cl-gen-magnum/source/main" "cpp" code)
  #+nil(sb-ext:run-program "/bin/sh" '("/home/martin/stage/cl-gen-lens-trace-quad/run.sh")))


