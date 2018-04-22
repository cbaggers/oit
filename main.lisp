(in-package #:oit)

;;----------------------------------------------------------------------

(defvar *sphere* nil)
(defvar *solid-fbo* nil)
(defvar *projection* nil)

;;----------------------------------------------------------------------

(defpipeline-g pline-0 ()
  :vertex
  (lambda-g ((vert g-pnt)
             &uniform
             (offset :vec3)
             (color :vec4)
             (proj :mat4))
    (let ((pos3 (pos vert)))
      (values
       (* proj (v! (+ pos3 offset) 1))
       color)))
  :fragment
  (lambda-g ((color :vec4))
    color))

;;----------------------------------------------------------------------

(defun init ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport)) res)
    ;;
    (setf *projection* (rtg-math.projection:perspective-v2 res 1f0 100f0 45f0 )))
  ;;
  (unless *sphere*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *sphere* (make-buffer-stream v :index-array i))))
  ;;
  (when *solid-fbo*
    (free (attachment-tex *solid-fbo* 0))
    (free (attachment-tex *solid-fbo* :d))
    (free *solid-fbo*))
  (setf *solid-fbo* (make-fbo 0 :d)))

;;----------------------------------------------------------------------

(defun sphere (offset color)
  (map-g #'pline-0 *sphere*
         :offset offset
         :color color
         :proj *projection*))

(defun step-main ()
  (as-frame
    (sphere (v! 0 -1 -9) (v! 1 0 0 0))
    (sphere (v! 0.3 -1 -5) (v! 0 1 0 0))
    (sphere (v! -0.3 -1 -7) (v! 0 0 1 0))))

(def-simple-main-loop oit (:on-start #'init)
  (step-main))

;;----------------------------------------------------------------------
