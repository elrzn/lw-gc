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
   (allocated
    capi:title-pane
    :accessor gc-generation-info-allocated-pane
    :documentation
    "Display information of the number of allocated objects for the
current generation.")
   (cons-title-pane
    capi:title-pane
    :accessor gc-generation-info-cons-title-pane
    :title "Cons")
   (non-pointer-title-pane
    capi:title-pane
    :accessor gc-generation-info-non-pointer-title-pane
    :title "Non pointer")
   (other-title-pane
    capi:title-pane
    :accessor gc-generation-info-other-title-pane
    :title "Other")
   (symbol-title-pane
    capi:title-pane
    :accessor gc-generation-info-symbol-title-pane
    :title "Symbol")
   (function-title-pane
    capi:title-pane
    :accessor gc-generation-info-function-title-pane
    :title "Function")
   (weak-title-pane
    capi:title-pane
    :accessor gc-generation-info-weak-title-pane
    :title "Weak")
   (gc-button
    capi:push-button
    :text "Collect"
    :callback-type :none
    :callback #'(lambda ()
                  (harlequin-common-lisp:gc-generation generation-number))
    :documentation
    "Performs a garbage collection of the current generation."))
  (:layouts
   (main-layout
    capi:column-layout
    '(allocated
      fragmentation-state-layout
      gc-button))
   (fragmentation-state-layout
    capi:row-layout
    '(cons-title-pane
      symbol-title-pane
      function-title-pane
      weak-title-pane
      non-pointer-title-pane
      other-title-pane)))
  (:documentation
   "Contains a graphical representation of a GC region (generation).")
  (:default-initargs :title-position :frame))

(defmethod gc-generation-info-title ((self gc-generation-info))
  "Return the title of the GC-GENERATION-INFO object, based on the
given generation id."
  (format nil "Generation ~a" (gc-generation-info-generation-number self)))

(defmethod gc-generation-info-allocated ((self gc-generation-info))
  "Return the number of objects that are allocated for the given
generation."
  (system:count-gen-num-allocation (gc-generation-info-generation-number self)))

(defmethod gc-generation-info-fragmentation-state ((self gc-generation-info))
  (system:gen-num-segments-fragmentation-state (gc-generation-info-generation-number self)))

(defmethod tick ((self gc-generation-info))
  ;; Update fragmentation info.
  (let ((fragmentation (gc-generation-info-fragmentation-state self)))
    (flet ((fetch (key)
             (format nil "~d"
                     (or (cadr (assoc key fragmentation))
                         0))))
      (setf (capi:title-pane-text (gc-generation-info-cons-title-pane self))
            (fetch :cons))
      (setf (capi:title-pane-text (gc-generation-info-symbol-title-pane self))
            (fetch :symbol))
      (setf (capi:title-pane-text (gc-generation-info-function-title-pane self))
            (fetch :function))
      (setf (capi:title-pane-text (gc-generation-info-weak-title-pane self))
            (fetch :weak))
      (setf (capi:title-pane-text (gc-generation-info-non-pointer-title-pane self))
            (fetch :non-pointer))
      (setf (capi:title-pane-text (gc-generation-info-other-title-pane self))
            (fetch :other))))
  (let ((allocated (gc-generation-info-allocated self)))
    ;; Update total allocated info.
    (setf (capi:title-pane-text (gc-generation-info-allocated-pane self))
          (format nil "~a allocated" allocated))
    ;; Only show generation info when it actually holds allocations.
    ;; TODO Need to improve this and hide the title :)
    (funcall (if (zerop allocated) #'capi:hide-pane #'capi:show-pane) self)))

;; DELME This is no longer necessary since GC-INFO will already
;; perform the TICK function for each generation. Consider removing.
(defmethod initialize-instance :after ((self gc-generation-info) &key)
  (tick self))

(capi:define-interface gc-info ()
  ((timer :initarg :timer
          :accessor gc-info-timer
          :initform nil))
  (:panes
   (allocated
    capi:title-pane
    :accessor gc-info-allocated
    :documentation "Displays the overall allocation for all regions.")
   ;; Is it a good idea to do this here? Would it be better to declare
   ;; this as a fixed array as part of the slots?
   (generation-1 gc-generation-info :number 1 :accessor gc-info-generation-1-pane :title "Generation 1")
   (generation-2 gc-generation-info :number 2 :accessor gc-info-generation-2-pane :title "Generation 2")
   (generation-3 gc-generation-info :number 3 :accessor gc-info-generation-3-pane :title "Generation 3")
   (generation-4 gc-generation-info :number 4 :accessor gc-info-generation-4-pane :title "Generation 4")
   (generation-5 gc-generation-info :number 5 :accessor gc-info-generation-5-pane :title "Generation 5")
   (generation-6 gc-generation-info :number 6 :accessor gc-info-generation-6-pane :title "Generation 6")
   (generation-7 gc-generation-info :number 7 :accessor gc-info-generation-7-pane :title "Generation 7")
   (button-full-gc
    capi:push-button
    :text "Collect all"
    :default-p t
    :callback-type :none
    :callback #'(lambda ()
                  (harlequin-common-lisp:gc-generation t))
    :documentation "Perform a full collection.")
   (button-refresh
    capi:push-button
    :text "Refresh"
    :callback-type :interface
    :callback #'(lambda (interface)
                  (tick interface))
    :documentation "Perform a manual UI refresh."))
  (:layouts
   (main-layout
    capi:column-layout
    '(allocated generations-layout button-layout))
   (generations-layout
    capi:column-layout
    '(generation-1
      generation-2
      generation-3
      generation-4
      generation-5
      generation-6
      generation-7))
   (button-layout
    capi:row-layout
    '(button-full-gc button-refresh)))
  (:documentation
   "The main interface of the application. Contains an overview of all
generations, as well as an option to perform a full cleanup")
  (:default-initargs :title "Garbage"))

(defmethod tick ((self gc-info))
  (setf (capi:title-pane-text (gc-info-allocated self))
        (let* ((info (system:room-values)))
          (format nil "Allocated ~a out of ~a"
                  (getf info :total-allocated)
                  (getf info :total-size))))
  ;; TODO Refactor this ugly thing.
  (tick (gc-info-generation-1-pane self))
  (tick (gc-info-generation-2-pane self))
  (tick (gc-info-generation-3-pane self))
  (tick (gc-info-generation-4-pane self))
  (tick (gc-info-generation-5-pane self))
  (tick (gc-info-generation-6-pane self))
  (tick (gc-info-generation-7-pane self)))

(defmethod initialize-instance :after ((self gc-info) &key)
  (tick self))

(defun main ()
  "Starts the application."
  (let ((app (make-instance 'gc-info)))
    ;; Create and setup the timer that will periodically refresh the
    ;; application content.
    (with-slots (timer)
        app
      (setf timer (mp:make-timer #'(lambda () (tick app))))
      (mp:schedule-timer timer 900 2))
    (capi:display app)))
