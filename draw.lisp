#-ccl(error "CCL only")

;;;; generalize vecto to arbitrary surfaces (half working)


(in-package :cl-user)
(require :cocoa)

(ql:quickload :vecto)
(ql:quickload :flexi-streams)
(ql:quickload :alexandria)

(defpackage :draw
  (:use :cl :alexandria)
  (:shadow :fill))

(in-package :draw)

(defun nil? (x)
  (or (null x) (ccl::%null-ptr-p x)))

(defmacro in-cocoa-thread (&body b)
  (let ((ip (gensym))
        (l (gensym)))
    `(let ((,ip ccl::*initial-process*)
           (,l (lambda () ,@b)))
       (if (eq ccl::*current-process* ,ip)
           (funcall ,l)
           (ccl::process-interrupt ,ip ,l)))))

(defun show-view (view)
  (when (not (nil? view))
    (in-cocoa-thread
      (let ((w
             (make-instance
              'ns:ns-window
              :with-content-rect (#/bounds view)
              :style-mask (logior #$NSTitledWindowMask
                                       #$NSClosableWindowMask
                                       #$NSMiniaturizableWindowMask)
              :backing #$NSBackingStoreBuffered
              :defer t)))
        (#/setFrameOrigin: w (ns:make-ns-point 100 100))
        (#/setContentView: w view)
        (#/setLevel: w 100)
        (#/orderFront: w ccl:+null-ptr+)
        w))))

(defclass red-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void)
    ((self red-view) (rect :<NSR>ect))
  (#/set (#/redColor ns:ns-color))
  (#_NSRectFill rect))

(defvar *surface* nil)

(defgeneric call-with-surface (surface fn))

(defmacro with-surface (s &body b)
  (alexandria:once-only (s)
    `(let ((*surface* ,s))
       (call-with-surface ,s (lambda () ,@b)))))

(defgeneric %set-color (surface c))
(defgeneric %set-rgba-fill (surface r g b a))
(defgeneric %set-rgba-stroke (surface r g b a))


(defgeneric %fill-rect (surface x y w h))
(defgeneric %clear-canvas (surface))
(defgeneric %call-with-graphics-state (surface fn))
(defgeneric %translate (surface x y))
(defgeneric %rotate (surface r))
(defgeneric %scale (surface sx sy))


(defgeneric %move-to (surface x y))
(defgeneric %line-to (surface x y))
(defgeneric %stroke  (surface))
(defgeneric %fill (surface))

(defgeneric %set-line-width (surface w))
(defgeneric %set-line-join  (surface style))

(defgeneric %centered-ellipse-path (surface x y rx ry))

(defun set-color (c)
  (%set-color *surface* c))
(defun set-rgba-stroke (r g b a)
  (%set-rgba-stroke *surface* r g b a))
(defun set-rgb-stroke (r g b)
  (set-rgba-stroke r g b 1.0))
(defun set-rgba-fill (r g b a)
  (%set-rgba-fill *surface* r g b a))
(defun set-rgb-fill (r g b)
  (set-rgba-fill r g b 1.0))


(defun fill-rect (x y w h)
  (%fill-rect *surface* x y w h))
(defun clear-canvas ()
  (%clear-canvas *surface*))
(defmacro with-graphics-state (&body b)
  `(%call-with-graphics-state *surface* (lambda () ,@b)))
(defun translate (x y)
  (%translate *surface* x y))
(defun rotate (r)
  (%rotate *surface* r))
(defun scale (sx sy)
  (%scale *surface* sx sy))

(defun move-to (x y)
  (%move-to *surface* x y))
(defun line-to (x y)
  (%line-to *surface* x y))
(defun stroke ()
  (%stroke *surface*))

(shadow 'fill)
(defun fill ()
  (%fill *surface*))

(defun set-line-width (w)
  (%set-line-width *surface* w))
(defun set-line-join (style)
  (%set-line-join *surface* style))

(defun centered-ellipse-path (x y rx ry)
  (%centered-ellipse-path *surface* x y rx ry))

(defun centered-circle-path (x y r)
  (centered-ellipse-path x y r r))


(defmethod %set-color ((surface ns:ns-view) c)
  (let ((nsc
         (case c
           (:red    (#/redColor ns:ns-color))
           (:green  (#/greenColor ns:ns-color))
           (:blue   (#/blueColor ns:ns-color))
           (:yellow (#/yellowColor ns:ns-color))
           (:white  (#/whiteColor ns:ns-color))
           (:black  (#/blackColor ns:ns-color)))))
    (when nsc
      (#/set nsc))))


(defmethod %set-rgba-fill ((surface ns:ns-view) r g b a)
  (#/setFill (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                               r g b a)))

(defmethod %set-rgba-stroke ((surface ns:ns-view) r g b a)
  (#/set (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                               r g b a)))

(defmethod %fill-rect ((surface ns:ns-view) x y w h)
  (#/fillRect: ns:ns-bezier-path (ns:make-ns-rect x y w h))
  ;(#_NSRectFill (ns:make-ns-rect x y w h))
  )

(defmethod %clear-canvas ((surface ns:ns-view))
  (#_NSRectFill (#/bounds surface)))

(defvar %path% nil)

(defmethod call-with-surface ((view ns:ns-view) fn)
  (in-cocoa-thread
    (when (#/lockFocusIfCanDraw view)
      (unwind-protect
           (let ((*surface* view)
                 (%path% %path%))
             (funcall fn))
        (#/unlockFocus view)
        (#/flushGraphics (#/currentContext ns:ns-graphics-context))
        (#/flushWindow (#/window view))))))



(defmethod %translate ((surface ns:ns-view) x y)
  (let ((f (#/transform ns:ns-affine-transform)))
    (#/translateXBy:yBy: f x y)
    (#/concat f)))

(defmethod %rotate    ((surface ns:ns-view) r)
    (let ((f (#/transform ns:ns-affine-transform)))
    (#/rotateByRadians: f r)
    (#/concat f)))

(defmethod %scale     ((surface ns:ns-view) sx sy)
    (let ((f (#/transform ns:ns-affine-transform)))
      (#/scaleXBy:yBy: f sx sy)
      (#/concat f)))

(defun %%new-path ()
   (setf %path% (#/bezierPath ns:ns-bezier-path)))

(defmethod %move-to ((surface ns:ns-view) x y)
  (%%new-path)
  (#/moveToPoint: %path% (ns:make-ns-point x y)))

(defmethod %line-to ((surface ns:ns-view) x y)
  (#/lineToPoint: %path% (ns:make-ns-point x y)))

(defmethod %stroke  ((surface ns:ns-view))
  (#/stroke %path%))

(defmethod %fill ((surface ns:ns-view))
  (#/fill %path%))

(defmethod %set-line-width ((surface ns:ns-view) w)
  (#/setDefaultLineWidth: ns:ns-bezier-path w))

(defmethod %set-line-join ((surface ns:ns-view) style)
  (let ((s (case style
             (:miter #$NSMiterLineJoinStyle)
             (:bevel #$NSBevelLineJoinStyle)
             (:round #$NSRoundLineJoinStyle))))
    (when s
      (#/setDefaultLineJoinStyle: ns:ns-bezier-path s))))

(defmethod %centered-ellipse-path ((surface ns:ns-view) x y rx ry)
  (%%new-path)
  (#/appendBezierPathWithOvalInRect:
   %path%
    (ns:make-ns-rect (- x rx) (- y ry)
                     (+ rx rx) (+ ry ry))))


(defmethod %call-with-graphics-state ((surface ns:ns-view) fn)
  (#/saveGraphicsState (#/currentContext ns:ns-graphics-context))
  (let ((lw (#/defaultLineWidth ns:ns-bezier-path))
        (ls (#/defaultLineJoinStyle ns:ns-bezier-path)))
    (unwind-protect (funcall fn)
      (#/setDefaultLineWidth: ns:ns-bezier-path lw)
      (#/setDefaultLineJoinStyle: ns:ns-bezier-path ls)
      (#/restoreGraphicsState (#/currentContext ns:ns-graphics-context)))))

#|
(defvar *r nil)
(setf *r (make-instance 'red-view
                        :with-frame (ns:make-ns-rect 0 0 500 300)))
(show-view *r)
(#/close (#/window *r))
(#/release *r)

(with-surface *r
  (set-line-width 1.0)
  (set-color :white)
  (clear-canvas)
  (set-color :red)
  (with-graphics-state
    (set-line-width 5.0)
    (set-line-join :round)
    (translate 230 0)
    (loop repeat 80
         do (progn
              (move-to 105 0)
              (line-to -5 5)
              (line-to  55 -105)
              (line-to 30 30)
              (set-rgba-stroke 0.1 0.4 1.0 0.6)
              (stroke)
              (translate 4 4)
              (set-rgba-stroke 0.1 0.6 0.6 0.5)
              (stroke)
              (translate -4 -4)
              (translate 10 10)
              (rotate 0.17)
              (scale 0.95 0.97))))
  (with-graphics-state
    (translate 130 80)
    (rotate 0.6)
    (loop ;for color in '#1=(:red :green :blue :yellow . #1#)
         for c in '#2=((1 0 0 0.25)
                       (1 0 1 0.3)                       
                       (0 0 1 0.4)
                       (0 1 0 0.5)
                       (1 1 0 0.5)
                       . #2#)
         repeat 80
         do (progn
              ;(set-color color)
              (apply #'set-rgba-fill c)
              (translate 30 30)
              (rotate -0.04)
              (scale 0.95 0.95)
              (fill-rect 0 0 100 100)
              (rotate -0.2))))
  (centered-circle-path 30 30 20)
  (set-rgba-fill 0.2 0.2 0.8 0.3)
  (stroke)
  (fill)
  (fill-rect 30 200 40 40))
|#
