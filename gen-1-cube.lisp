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
	       (include <Magnum/Shaders/Phong.h>)
	       (include <Magnum/Math/Color.h>)
	       (include <Magnum/Primitives/Cube.h>)
	       (include <Magnum/MeshTools/CompressIndices.h>)
	       (include <Magnum/MeshTools/Interleave.h>)
	       (include <Magnum/Trade/MeshData3D.h>)
	       (include <array>)
	       (raw "using namespace std;")
	       (raw "using namespace Magnum;")
	       (raw "using namespace Magnum::Math::Literals;")

	       (class MyApplication ("public Platform::Application")
		      (access-specifier public)
		      (function (MyApplication ((arguments :type "const Arguments&")) explicit))
		      (access-specifier private)
		      (function (drawEvent () void :specifier override))
		      (function (mousePressEvent ((event :type MouseEvent&)) void :specifier override))
		      (function (mouseReleaseEvent ((event :type MouseEvent&)) void :specifier override))
		      (function (mouseMoveEvent ((event :type MouseMoveEvent&)) void :specifier override))
		      
		      (decl ((m_index_buffer :type Buffer)
			     (m_vertex_buffer :type Buffer)
			     (m_mesh :type Mesh)
			     (m_shader :type "Shaders::Phong")

			     (m_transformation :type Matrix4)
			     (m_projection :type Matrix4)

			     (m_previous_mouse_position :type Vector2i)
			     (m_color :type Color3))))

	       (struct TriangleVertex ()
		       (decl ((position :type Vector2)
			      (color :type Color3))))
	       ,(let ((v '((-.5 -.5 #xff0000)
			   (.5 -.5 #x00ff00)
			   (.0 .5 #x0000ff))))
		     `(decl ((data :type ,(format nil "array<TriangleVertex,~a>" (length v))
				   :ctor (list (list ,@(loop for (x y color) in v collect
							    `(list (list ,x ,y) ,(format nil "0x~6,'0x_srgbf" color)))))))))
	       
	       
	       #+nil ,@(dox :brief ""
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
			 (funcall "Renderer::enable" "Renderer::Feature::DepthTest")
			 (funcall "Renderer::enable" "Renderer::Feature::FaceCulling")
			 (let ((cube :type "Trade::MeshData3D" :ctor (funcall "Primitives::Cube::solid")))
			   #+nil (setf cube (funcall "Primitives::Cube::solid"))
			   (funcall m_vertex_buffer.setData
				    (funcall "MeshTools::interleave"
					     (funcall cube.positions 0)
					     (funcall cube.normals 0))
				    "BufferUsage::StaticDraw")
			   (let ((index_data :type "Containers::Array<char>")
				 (index_type :type "Mesh::IndexType")
				 (index_start :type UnsignedInt)
				 (index_end :type UnsignedInt))
			     (setf (funcall tie index_data index_type index_start index_end)
				   (funcall "MeshTools::compressIndices" (funcall cube.indices)))
			     (funcall m_index_buffer.setData index_data "BufferUsage::StaticDraw")

			     ,@(loop for (e f) in `((setPrimitive ((funcall cube.primitive)))
						(setCount ((funcall "cube.indices().size")))
						(addVertexBuffer (m_vertex_buffer 0 "Shaders::Phong::Position{}" "Shaders::Phong::Normal{}"))
						(setIndexBuffer (m_index_buffer 0 index_type index_start index_end)))
			      collect
				`(funcall (slot-value m_mesh ,e) ,@f))))

			 (setf m_transformation (* (funcall "Matrix4::rotationX" "30.0_degf")
						   (funcall "Matrix4::rotationY" "40.0_degf"))
			       m_color (funcall "Color3::fromHsv" "35.0_degf" 1s0 1s0)
			       m_projection (* (funcall "Matrix4::perspectiveProjection"
						      "35.0_degf"
						      (funcall "Vector2{defaultFramebuffer.viewport().size()}.aspectRatio")
						      .01s0 100s0)
					       (funcall "Matrix4::translation"
							(funcall "Vector3::zAxis" -10s0))))
			 
			 
			 
			 (<< (funcall Debug)
			     (string "running on: ") (funcall "Context::current().version")
			     (string " using ") (funcall "Context::current().rendererString")))

	       
	       (function ("MyApplication::drawEvent" () void)
			  (funcall defaultFramebuffer.clear
				   (|\|| "FramebufferClear::Color"
					 "FramebufferClear::Depth"))
			  ,@(loop for (e f) in `((setLightPosition ((list 7s0 5s0 2.5s0)))
						 (setLightColor ("Color3{1.0f}"))
						 (setDiffuseColor (m_color))
						 (setAmbientColor ((funcall "Color3::fromHsv"
									    (funcall m_color.hue)
									    1s0
									    .3s0)))
						 (setTransformationMatrix (m_transformation))
						 (setNormalMatrix ((funcall m_transformation.rotationScaling)))
						 (setProjectionMatrix (m_projection)))
			      collect
				 `(funcall (slot-value m_shader ,e) ,@f))
			  
			  (funcall m_mesh.draw m_shader)
			  (funcall swapBuffers))

	       (function ("MyApplication::mousePressEvent" ((event :type MouseEvent&)) void)
			 (if (!= "MouseEvent::Button::Left" (funcall event.button))
			     (return))
			 (setf m_previous_mouse_position (funcall event.position))
			 (funcall event.setAccepted))
	       (function ("MyApplication::mouseReleaseEvent" ((event :type MouseEvent&)) void)
			 (setf m_color (funcall "Color3::fromHsv" (+ "50.0_degf" (funcall m_color.hue))
						1s0 1s0))
			 (setf m_previous_mouse_position (funcall event.position))
			 (funcall event.setAccepted)
			 (funcall redraw))
	       (function ("MyApplication::mouseMoveEvent" ((event :type MouseMoveEvent&)) void)
			 (if (! (& "MouseMoveEvent::Button::Left"
				   (funcall event.buttons)))
			     (return))
			 (let ((delta :type "const Vector2" :ctor (* 3s0
								     (/ (make-instance Vector2
										       (- (funcall event.position)
											  m_previous_mouse_position))
									(make-instance Vector2
										       (funcall "defaultFramebuffer.viewport().size"))))))
			   
			   (setf 
			    m_transformation (* (funcall "Matrix4::rotationX"
							 (make-instance Rad (funcall delta.y)))
						m_transformation
						(funcall "Matrix4::rotationY"
							 (make-instance Rad (funcall delta.x))))
			    m_previous_mouse_position (funcall event.position)))
			 
			 (funcall event.setAccepted)
			 (funcall redraw))
	       	       
	       (funcall MAGNUM_APPLICATION_MAIN MyApplication)
	       #+nil
	       (function (main ((argc :type int)
				(argv :type char**)) int)
			 
			 (return 0)))))
  (write-source "stage/cl-gen-magnum/source/main" "cpp" code)
  #+nil(sb-ext:run-program "/bin/sh" '("/home/martin/stage/cl-gen-lens-trace-quad/run.sh")))


