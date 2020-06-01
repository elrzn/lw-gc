;;;; lw-gc.lisp

(in-package #:lw-gc)

(defmethod gc-generation-info-title ((self gc-generation-info))
  "Return the title of the GC-GENERATION-INFO object, based on the
given generation id."
  (format nil "Generation ~a" (gc-generation-info-generation-number self)))

(defmethod gc-generation-info-allocated ((self gc-generation-info))
  (system:count-gen-num-allocation (gc-generation-info-generation-number self)))

(capi:define-interface gc-generation-info ()
  ((generation-number
    :initarg :number
    :reader gc-generation-info-generation-number
    :type fixnum
    :initform (error "Generation number is mandatory.")
    :documentation
    "The generation number that identifies the interface. This is
injected via the interface parent."))
  (:panes
   (title capi:title-pane :accessor gc-generation-info-title)
   (allocated capi:title-pane :accessor gc-generation-info-allocated)
   (gc-button capi:push-button
              :text "Collect"
              :callback #'(lambda (data interface)
                            (declare (ignore data interface))
                            (harlequin-common-lisp:gc-generation generation-number))))
  (:layouts
   (main-layout capi:row-layout '(title allocated gc-button))))

(defmethod initialize-instance :after ((self gc-generation-info) &key)
  (let ((number (gc-generation-info-generation-number self)))
    ;; For some reason I cannot make use of the methods defined above
    ;; for returning the title and the number of allocated objects for
    ;; the given GC-GENERATION-INFO instance. I believe I have to do
    ;; some reading on method qualifiers. Perhaps these methods are
    ;; not even attached yet at the moment of creating the new
    ;; instance?
    (setf (capi:title-pane-text (gc-generation-info-title self))
          (format nil "Generation ~a" number))
    (setf (capi:title-pane-text (gc-generation-info-allocated self))
          (format nil "~a allocated"
                  (system:count-gen-num-allocation number)))))

(capi:define-interface gc-info ()
  ()
  (:panes
   (allocated capi:title-pane
              :text (let* ((info (system:room-values)))
                      (format nil "Allocated ~a out of ~a"
                              (getf info :total-allocated)
                              (getf info :total-size))))
   ;; Is it a good idea to do this here? Would it be better to declare
   ;; this as a fixed array as part of the slots?
   (generation-1 gc-generation-info :number 1 :accessor gc-info-generation-1)
   (generation-2 gc-generation-info :number 2 :accessor gc-info-generation-2)
   (generation-3 gc-generation-info :number 3 :accessor gc-info-generation-3)
   (generation-4 gc-generation-info :number 4 :accessor gc-info-generation-4)
   (generation-5 gc-generation-info :number 5 :accessor gc-info-generation-5)
   (generation-6 gc-generation-info :number 6 :accessor gc-info-generation-6)
   (generation-7 gc-generation-info :number 7 :accessor gc-info-generation-7)
   (button-full-gc capi:push-button :text "Collect all" :default-p t))
  (:layouts
   (main-layout capi:column-layout '(allocated
                                     generations-layout
                                     button-full-gc))
   (generations-layout capi:column-layout
                       '(generation-1
                         generation-2
                         generation-3
                         generation-4
                         generation-5
                         generation-6
                         generation-7)
                       :title "Generations"
                       :title-position :frame))
   (:default-initargs :title "Garbage"
  ; :best-width 640
  ; :best-height 480
    ))

(defun main ()
  (capi:display
   (make-instance 'gc-info)))
