;;;; lw-gc.lisp

(in-package #:lw-gc)

;; How can I call methods when declaring an interface?
(defmethod gc-generation-info-title ((self gc-generation-info))
  (format nil "Generation #~a" (generation-number self)))

(defmethod gc-generation-info-allocated ((self gc-generation-info))
  (system:count-gen-num-allocation (gc-generation-info-generation-number self)))

(capi:define-interface gc-generation-info ()
  ((generation-number :initarg :number
                      :reader gc-generation-info-generation-number
                      :type fixnum
                      :initform (error "Generation number is mandatory.")
                      :documentation
                      "The generation number that identifies the
interface. This is injected via the interface parent."))
  (:panes
   (title capi:title-pane
          :text (format nil "Generation ~a" generation-number))
   (allocated capi:title-pane
              :text (format nil "~a allocated"
                            (system:count-gen-num-allocation generation-number)))
   (gc-button capi:push-button
              :text "Collect"
              :callback #'(lambda (data interface)
                            (declare (ignore data interface))
                            (harlequin-common-lisp:gc-generation generation-number))))
  (:layouts
   (main-layout capi:row-layout '(title allocated gc-button))))

(capi:define-interface gc-info ()
  ()
  (:panes
   (allocated capi:title-pane
              :text (let* ((info (system:room-values)))
                      (format nil "Allocated ~a out of ~a"
                              (getf info :total-allocated)
                              (getf info :total-size))))
   (generation-1 gc-generation-info :number 1)
   (generation-2 gc-generation-info :number 2)
   (generation-3 gc-generation-info :number 3)
   (generation-4 gc-generation-info :number 4)
   (generation-5 gc-generation-info :number 5)
   (generation-6 gc-generation-info :number 6)
   (generation-7 gc-generation-info :number 7)
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
