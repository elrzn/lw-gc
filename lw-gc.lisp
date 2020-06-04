;;;; lw-gc.lisp

(in-package #:lw-gc)

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
   (title
    capi:title-pane
    :accessor gc-generation-info-title-pane
    :documentation "Display the title of the current generation.")
   (allocated
    capi:title-pane
    :accessor gc-generation-info-allocated-pane
    :documentation
    "Display information of the number of allocated objects for the
current generation.")
   (gc-button
    capi:push-button
    :text "Collect"
    :callback #'(lambda (data interface)
                  (declare (ignore data interface))
                  (harlequin-common-lisp:gc-generation generation-number))
    :documentation
    "Performs a garbage collection of the current generation."))
  (:layouts
   (main-layout capi:row-layout '(title allocated gc-button)))
  (:documentation
   "Contains a graphical representation of a GC region
(generation)."))

(defmethod gc-generation-info-title ((self gc-generation-info))
  "Return the title of the GC-GENERATION-INFO object, based on the
given generation id."
  (format nil "Generation ~a" (gc-generation-info-generation-number self)))

(defmethod gc-generation-info-allocated ((self gc-generation-info))
  "Return the number of objects that are allocated for the given
generation."
  (system:count-gen-num-allocation (gc-generation-info-generation-number self)))

(defmethod initialize-instance :after ((self gc-generation-info) &key)
  (setf (capi:title-pane-text (gc-generation-info-title-pane self))
        (gc-generation-info-title self))
  (setf (capi:title-pane-text (gc-generation-info-allocated-pane self))
        (format nil "~a allocated"
                (gc-generation-info-allocated self))))

(capi:define-interface gc-info ()
  ()
  (:panes
   (allocated
    capi:title-pane
    :text (let* ((info (system:room-values)))
            (format nil "Allocated ~a out of ~a"
                    (getf info :total-allocated)
                    (getf info :total-size)))
    :documentation "Displays the overall allocation for all regions.")
   ;; Is it a good idea to do this here? Would it be better to declare
   ;; this as a fixed array as part of the slots?
   (generation-1 gc-generation-info :number 1 :accessor gc-info-generation-1-pane)
   (generation-2 gc-generation-info :number 2 :accessor gc-info-generation-2-pane)
   (generation-3 gc-generation-info :number 3 :accessor gc-info-generation-3-pane)
   (generation-4 gc-generation-info :number 4 :accessor gc-info-generation-4-pane)
   (generation-5 gc-generation-info :number 5 :accessor gc-info-generation-5-pane)
   (generation-6 gc-generation-info :number 6 :accessor gc-info-generation-6-pane)
   (generation-7 gc-generation-info :number 7 :accessor gc-info-generation-7-pane)
   (button-full-gc capi:push-button
                   :text "Collect all"
                   :default-p t
                   :documentation "Perform a full collection."))
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
  (:documentation
   "The main interface of the application. Contains an overview of all
generations, as well as an option to perform a full cleanup")
  (:default-initargs :title "Garbage"))

(defun main ()
  "Starts the application."
  (capi:display
   (make-instance 'gc-info)))
