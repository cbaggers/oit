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
    (saturate (texture sam uv))))

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
        (sample (attachment-tex *solid-fbo* 0)))
  ;;
  (bavoil-myers-init)
  ;;
  (boit-init))

;;----------------------------------------------------------------------
;; Ordered

(defvar *one-min-alpha*
  (make-blending-params))

(defun draw-ordered ()
  (as-frame
    (sphere (v! 0 -1 -9) *red*)
    (with-blending *one-min-alpha*
      (sphere (v! -0.3 -1 -7) *blue*)
      (sphere (v! 0.4 -1 -5) *green*))))

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
  (draw-opaque)
  (draw-transparent-meshkin)
  (blit *solid-col-sampler*))

;;----------------------------------------------------------------------
;; Bavoil and Myers

(defvar *bavoil-myers-fbo* nil)
(defvar *bavoil-myers-col0-sampler* nil)
(defvar *bavoil-myers-col1-sampler* nil)

(defun bavoil-myers-init ()
  (when *bavoil-myers-fbo*
    (free (attachment-tex *bavoil-myers-fbo* 0))
    ;; dont free depth as it is from solid
    (free *bavoil-myers-fbo*))
  (setf *bavoil-myers-fbo* (make-fbo (list 0 :element-type :vec4)
                                    (list 1 :element-type :vec4)
                                    (list :d (attachment-tex *solid-fbo* :d))))
  (setf *bavoil-myers-col0-sampler*
        (sample (attachment-tex *bavoil-myers-fbo* 0)))
  (setf *bavoil-myers-col1-sampler*
        (sample (attachment-tex *bavoil-myers-fbo* 0))))

(defpipeline-g pline-bavoil-myers-0 ()
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
    (values color (vec4 1))))

(defun trans-sphere-bavoil-meyers-accum (offset color)
  (map-g #'pline-bavoil-myers-0 *sphere*
         :offset offset
         :color color
         :proj *projection*))

(defun bavoil-myers-accum ()
  (with-setf (depth-mask) nil
    (with-blending *blend-one-one*
      (with-fbo-bound (*bavoil-myers-fbo*)
        (clear-fbo *bavoil-myers-fbo*)
        (trans-sphere-bavoil-meyers-accum (v! 0.4 -1 -5) *green*)
        (trans-sphere-bavoil-meyers-accum (v! -0.3 -1 -7) *blue*)))))

(defpipeline-g bavoil-myers-compose-pline ()
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
             (solid-sam :sampler-2d)
             (accum-sam :sampler-2d)
             (count-sam :sampler-2d))
    (flet ((fetch ((s :sampler-2d))
             (texel-fetch s
                          (ivec2 (s~ gl-frag-coord :xy))
                          0)))
      (let ((accum (fetch accum-sam))
            (n (max 1f0 (x (fetch accum-sam)))))
        (v! (/ (s~ accum :xyz) (max (w accum) 0.0001))
            (expt (max 0f0 (- 1f0 (/ (w accum) n)))
                  n))))))

(defun bavoil-myers-compose-sphere (offset color)
  (map-g #'bavoil-myers-compose-pline
         *sphere*
         :offset offset
         :color color
         :proj *projection*
         :solid-sam *solid-col-sampler*
         :accum-sam *bavoil-myers-col0-sampler*
         :count-sam *bavoil-myers-col1-sampler*))

(defvar *bavoil-myers-blend*
  (make-blending-params
   :source-rgb :one-minus-src-alpha
   :source-alpha :one-minus-src-alpha
   :destination-rgb :src-alpha
   :destination-alpha :src-alpha))

(defun bavoil-myers-compose ()
  (with-fbo-bound (*solid-fbo*)
    (with-blending *bavoil-myers-blend*
      (bavoil-myers-compose-sphere (v! 0.4 -1 -5) *green*)
      (bavoil-myers-compose-sphere (v! -0.3 -1 -7) *blue*))))


(defun draw-bavoil-myers ()
  (draw-opaque)
  (bavoil-myers-accum)
  (bavoil-myers-compose)
  (blit *solid-col-sampler*))

;;----------------------------------------------------------------------
;; boit

(defvar *boit-fbo* nil)
(defvar *boit-col0-sampler* nil)
(defvar *boit-col1-sampler* nil)

(defun boit-init ()
  (when *boit-fbo*
    (free (attachment-tex *boit-fbo* 0))
    ;; dont free depth as it is from solid
    (free *boit-fbo*))
  (setf *boit-fbo* (make-fbo (list 0 :element-type :vec4)
                             (list 1 :element-type :vec4)
                             (list :d (attachment-tex *solid-fbo* :d))))
  (setf (attachment-blending *boit-fbo* 0)
        (make-blending-params
         :source-rgb :one
         :source-alpha :one
         :destination-rgb :one
         :destination-alpha :one))
  (setf (attachment-blending *boit-fbo* 1)
        (make-blending-params
         :source-rgb :zero
         :source-alpha :zero
         :destination-rgb :one-minus-src-alpha
         :destination-alpha :one-minus-src-alpha))
  (setf *boit-col0-sampler*
        (sample (attachment-tex *boit-fbo* 0)))
  (setf *boit-col1-sampler*
        (sample (attachment-tex *boit-fbo* 1))))

(defpipeline-g boit-clear ()
  :vertex
  (lambda-g ((vert :vec2))
    (values (v! vert 0 1)
            (+ (* vert 0.5) 0.5)))
  :fragment
  (lambda-g ((uv :vec2))
    (values (vec4 0) (vec4 1f0))))

(defun clear-accum ()
  (with-fbo-bound (*boit-fbo* :with-blending nil)
    (clear-fbo *boit-fbo*)
    (with-setf (depth-mask) nil
      (map-g #'boit-clear (get-quad-stream-v2)))))

;; float weight =
;;   pow(alpha + 0.01f, 4.0f) +
;;   max(0.01f,
;;       min(3000.0f, 0.3f / (0.00001f + pow(abs(z) / 200.0f, 4.0f))));

(defun-g depth-estimator-0 ((depth :float) (alpha :float))
  (+ (expt (+ alpha 0.01) 4f0)
     (max 0.01f0
          (min 3000f0 (/ 0.3f0 (+ 0.00001f0
                                 (expt (/ (abs depth) 200f0)
                                       4f0)))))))
;; - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defpipeline-g boit-accum-sphere-pline ()
  :vertex
  (lambda-g ((vert g-pnt)
             &uniform
             (offset :vec3)
             (color :vec4)
             (proj :mat4))
    (let* ((pos3 (pos vert))
           (clip-pos (* proj (v! (+ pos3 offset) 1))))
      (values clip-pos color
              (- (z clip-pos)))))
  :fragment
  (lambda-g ((color :vec4) (z :float))
    (let* ((ci (s~ color :xyz))
           (ai (w color))
           ;; fudging
           (z (* z 500f0))
           ;;
           (weight (depth-estimator-0 z ai)))
      (values
       (v! (* ci ai weight) ai)
       (vec4 (* ai weight))))))

(defun boit-accum-sphere (offset color)
  (map-g #'boit-accum-sphere-pline *sphere*
         :offset offset
         :color color
         :proj *projection*))

;; IS THIS SETTING BLENDING CORRECTLY?
;;
(defun boit-accum ()
  (with-fbo-bound (*boit-fbo*)
    (with-setf (depth-mask) nil
      (boit-accum-sphere (v! 0.4 -1 -5) *green*)
      (boit-accum-sphere (v! -0.3 -1 -7) *blue*))))

(defpipeline-g boit-composite-pline ()
  :vertex
  (lambda-g ((vert :vec2))
    (values (v! vert 0 1)
            (+ (* vert 0.5) 0.5)))
  :fragment
  (lambda-g ((uv :vec2)
             &uniform
             (accum-sam :sampler-2d)
             (revealage-sam :sampler-2d))
    (let* ((accum (texture accum-sam uv))
           (accum-rgb (s~ accum :xyz))
           (reveal (x (texture revealage-sam uv)))
           (transparent (v! (/ accum-rgb (clamp reveal 0.0001 50000.0))
                            (w accum))))
      (if (>= (w accum) 1.0)
          (discard) ;;(vec4 1)
          (transparent)))))

(defvar *boit-composite-blend*
  (make-blending-params
   :source-rgb :one-minus-src-alpha
   :source-alpha :one-minus-src-alpha
   :destination-rgb :src-alpha
   :destination-alpha :src-alpha))

(defun boit-composite ()
  (progn ;;with-fbo-bound (*solid-fbo* :with-blending nil)
    (progn;;with-blending *boit-composite-blend*
      (with-setf (depth-mask) nil
        (map-g #'boit-composite-pline (get-quad-stream-v2)
               :accum-sam *boit-col0-sampler*
               :revealage-sam *boit-col1-sampler*)))))

(defun draw-boit ()
  (draw-opaque)
  (clear-accum)
  (boit-accum)
  (boit-composite)
  ;;(blit *solid-col-sampler*)
  ;; (draw-tex-tl *boit-col0-sampler*)
  ;; (draw-tex-tr *boit-col1-sampler*)
  ;; (boit-accum-sphere (v! 0.4 -1 -5) *green*)
  ;; (boit-accum-sphere (v! -0.3 -1 -7) *blue*)
  )

;;----------------------------------------------------------------------

(defun sphere (offset color)
  (map-g #'pline-0 *sphere*
         :offset offset
         :color color
         :proj *projection*))

(defun draw-opaque ()
  (with-fbo-bound (*solid-fbo*)
    (clear-fbo *solid-fbo*)
    (sphere (v! 0 -1 -9) *red*)))

(defun step-main ()
  (as-frame
    ;; (draw-ordered)
    ;; (draw-meshkin)
    ;; (draw-bavoil-myers)
    (draw-boit)
    ))

(def-simple-main-loop oit (:on-start #'init)
  (step-main))

;;----------------------------------------------------------------------
