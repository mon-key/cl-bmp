;;; :FILE-CREATED <Timestamp: #{2011-08-03T22:50:11-04:00Z}#{11313} - by MON>
;;; :FILE cl-bmp/package.lisp
;;; ==============================

;; (in-package #:cl-bmp) ;; for Slime
;; *package*


(defpackage #:cl-bmp (:use #:common-lisp)
            (:export
            ;; cl-bmp/cl-bmp.lisp
            #:make-bmp-header
            #:bmp-header                   ; <CLASS>
            #:bmp-file-size                ; <ACCESSOR>
            #:bmp-file-reserved-1          ; <ACCESSOR>
            #:bmp-file-reserved-2          ; <ACCESSOR>
            #:bmp-file-offset              ; <ACCESSOR>
            #:bmp-image-size-key           ; <ACCESSOR>
            #:bmp-image-width              ; <ACCESSOR>
            #:bmp-image-height             ; <ACCESSOR>
            #:bmp-image-planes             ; <ACCESSOR>
            #:bmp-image-bit-depth          ; <ACCESSOR>
            #:bmp-image-compression        ; <ACCESSOR>
            #:bmp-image-size-image         ; <ACCESSOR>
            #:bmp-image-pixels-per-meter-x ; <ACCESSOR>
            #:bmp-image-pixels-per-meter-y ; <ACCESSOR>
            #:bmp-image-colors-used        ; <ACCESSOR>
            #:bmp-image-colors-important   ; <ACCESSOR>
            ;; 
            #:read-bmp-header
            #:bmp-request-integer))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:



;;; ==============================
;;; EOF
