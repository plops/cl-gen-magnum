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
	       (include <Magnum/Context.h>)
	       (include <Magnum/Version.h>)
	       (include <Magnum/Renderer.h>)
	       (include <Magnum/Buffer.h>)
	       (include <Magnum/Mesh.h>)
	       (include <Magnum/Shaders/Shaders.h>)
	       (include <Magnum/Shaders/VertexColor.h>)
	       (include <Magnum/Math/Color.h>)
	       (include <array>)
	       (raw "using namespace std;")
	       (raw "using namespace Magnum;")
	       (raw "using namespace Magnum::Math::Literals;")

	       (class MyApplication ("public Platform::Application")
		      (access-specifier public)
		      (function (MyApplication ((arguments :type "const Arguments&")) explicit))
		      (access-specifier private)
		      (function (drawEvent () void :specifier override))
		      (decl ((m_buffer :type Buffer)
			     (m_mesh :type Mesh)
			     (m_shader :type "Shaders::VertexColor2D"))))

	       (struct TriangleVertex ()
		       (decl ((position :type Vector2)
			      (color :type Color3))))
	       ,(let ((v '((-.5 -.5 #xff0000)
			   (.5 -.5 #x00ff00)
			   (.0 .5 #x0000ff))))
		     `(decl ((data :type ,(format nil "array<TriangleVertex,~a>" (length v))
				   :ctor (list (list ,@(loop for (x y color) in v collect
							    `(list (list ,x ,y) ,(format nil "0x~6,'0x_srgbf" color)))))))))
	       
	       
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
			 (funcall "Renderer::setClearColor" (funcall "Color3::fromHsv" (raw "216.0_degf") .85s0 1s0))
			 (funcall m_buffer.setData data "BufferUsage::StaticDraw")
			 (raw "m_mesh.setPrimitive(MeshPrimitive::Triangles).setCount(3).addVertexBuffer(m_buffer,0,Shaders::VertexColor2D::Position{},Shaders::VertexColor2D::Color{Shaders::VertexColor2D::Color::Components::Three})")
			 
			 #+nil
			 (funcall addVertexBuffer
				  m_buffer 0 "Shaders::VertexColor2D::Position{}"  "Shaders::VertexColor2D::Color{Shaders::VertexColor2D::Color::Components::Three}")
			 (<< (funcall Debug)
			     (string "running on: ") (funcall "Context::current().version")
			     (string " using ") (funcall "Context::current().rendererString")))

	       (function ("MyApplication::drawEvent" () void)
			  (funcall defaultFramebuffer.clear
				   "FramebufferClear::Color")
			  (funcall m_mesh.draw m_shader)
			  (funcall swapBuffers))
	       (funcall MAGNUM_APPLICATION_MAIN MyApplication)
	       #+nil
	       (function (main ((argc :type int)
				(argv :type char**)) int)
			 
			 (return 0)))))
  (write-source "stage/cl-gen-magnum/source/main" "cpp" code)
  #+nil(sb-ext:run-program "/bin/sh" '("/home/martin/stage/cl-gen-lens-trace-quad/run.sh")))


