(in-package #:oit)

;;----------------------------------------------------------------------

(defparameter *red* (v! 0.8627451 0.08235294 0.08235294 1))
(defparameter *blue* (v! 0.07058824 0.22352941 0.8901961 0.5))
(defparameter *green* (v! 0.011764706 0.7019608 0.25490198 0.75))

(defvar *sphere* nil)
(defvar *solid-fbo* nil)
(defvar *solid-col-sampler* nil)
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

(defpipeline-g pline-blit ()
  :vertex
  (lambda-g ((vert :vec2))
    (values (v! vert 0 1)
            (+ (* vert 0.5) 0.5)))
  :fragment
  (lambda-g ((uv :vec2)
             &uniform (sam :sampler-2d))
    (texture sam uv)))

(defun blit (sampler)
  (map-g #'pline-blit (get-quad-stream-v2)
         :sam sampler))

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
  (setf *solid-fbo* (make-fbo (list 0 :element-type :vec4)
                              :d))
  (setf *solid-col-sampler*
          (sample (attachment-tex *solid-fbo* 0))))

;;----------------------------------------------------------------------
;; Meshkin

(defpipeline-g pline-meshkin ()
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
  (lambda-g ((color :vec4)
             &uniform
             (sam :sampler-2d))
    (let ((c0 (s~ (texel-fetch sam
                               (ivec2 (s~ gl-frag-coord :xy))
                               0)
                  :xyz))
          (ci (s~ color :xyz))
          (ai (w color)))
      (v! (- ci (* ai c0)) 1))))

(defvar *blend-one-one*
  (make-blending-params
   :source-rgb :one
   :destination-rgb :one
   :source-alpha :one
   :destination-alpha :one))

(defun transparent-sphere-meshkin (offset color)
  (map-g #'pline-meshkin *sphere*
         :offset offset
         :color color
         :proj *projection*
         :sam *solid-col-sampler*))

(defun draw-transparent-meshkin ()
  (with-setf (depth-mask) nil
    (with-blending *blend-one-one*
      (with-fbo-bound (*solid-fbo*)
        (transparent-sphere-meshkin (v! 0.4 -1 -5) *green*)
        (transparent-sphere-meshkin (v! -0.3 -1 -7) *blue*)))))

(defun draw-meshkin ()
  (as-frame
    (draw-opaque)
    (draw-transparent-meshkin)
    (blit *solid-col-sampler*)))

;;----------------------------------------------------------------------

(defun sphere (offset color)
  (map-g #'pline-0 *sphere*
         :offset offset
         :color color
         :proj *projection*))

(defun draw-opaque ()
  (with-fbo-bound (*solid-fbo*)
    (clear-fbo *solid-fbo*)
    (sphere (v! 0 -1 -9) (v! 1 0 0 0))))

(defun step-main ()
  (draw-meshkin))

(def-simple-main-loop oit (:on-start #'init)
  (step-main))

;;----------------------------------------------------------------------
