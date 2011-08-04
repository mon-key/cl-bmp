;;; :FILE-CREATED <Timestamp: #{2011-08-03T23:00:14-04:00Z}#{11313} - by MON>
;;; :FILE cl-bmp/cl-bmp.asd
;;; ==============================

;; ,----
;; | "I am sick to death of knee-jerk anti-LOOPism and I am beginning to
;; |  irrationally regard it as a plot to disable me as a programmer by
;; |  excommunicating my useful tools."
;; |
;; |     :SOURCE "Knee-jerk Anti-LOOPism and other E-mail Phenomena" p 17 
;; `---- :SEE http://ccs.mit.edu/papers/CCSWP150.html



(defpackage #:cl-bmp-build-system (:use :common-lisp :asdf))

(in-package #:cl-bmp-build-system)

(defsystem :cl-bmp
  ;; :name ""
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license "MIT" 
  :description "MS windows bmp OS/2 dib header reader"
  :version "1.0.0"
  :depends-on '()
  :serial t
  :components
  ((:file "package")
   (:file "cl-bmp")
   (:file "cl-bmp-docs")))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
