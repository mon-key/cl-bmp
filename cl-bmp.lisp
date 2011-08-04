;;; :FILE-CREATED <Timestamp: #{2011-08-01T22:38:23-04:00Z}#{11311} - by MON>
;;; :FILE cl-bmp/cl-bmp.lisp
;;; ==============================

(in-package #:cl-bmp)
;; *package*

(defun read-bmp-header (bmp-path-designator)
  (declare (optimize (speed 3)))
  (let ((bytes-read (make-array 52 :element-type '(unsigned-byte 8))))
    (declare ((simple-array (unsigned-byte 8) (52)) bytes-read))
    (with-open-file (bmp-hdr bmp-path-designator
                             :direction :input
                             :element-type '(unsigned-byte 8))
      ;; :NOTE `lispbuilder-sdl::image-is-bmp' does this:
      ;;  (string-equal (cffi:foreign-string-to-lisp str :count 2 :encoding :ascii) "BM")
      ;; :SEE :FILE lispbuilder-20110619-svn/lispbuilder-sdl/sdl/image.lisp
      (unless (and (= (read-byte bmp-hdr t) 66)
                   (= (read-byte bmp-hdr t) 77))
        (error "file magic does not designate a valid windows bmp"))
      (loop 
         for idx from 0 below 52
         for octed-read = (read-byte bmp-hdr)
         do (setf (aref bytes-read idx) octed-read)
         finally (return bytes-read)))))

;;; ==============================
;;; :PASTE-AUTHOR nyef -- Alistair Bridgewater
;;; :PASTE-TITLE Informing loop of integer size -- how to do it idiomatically?
;;; :PASTE 120426 :PASTE-URL (URL `http://paste.lisp.org/+2KX6/1')
(defun bmp-request-integer (array offset length &key little-endian sign-extend)
  (let ((value (loop
                  for i from 0 below length
                  for octet = (aref array (+ offset
                                             (if little-endian
                                                 i
                                                 (- length i 1))))
                  sum (ash octet (* i 8)))))
    (if (and sign-extend
             (logbitp (1- (* length 8)) value))
        (logior (lognot (1- (ash 1 (1- (* length 8))))) value)
        value)))
;;
(define-compiler-macro bmp-request-integer (&whole form array offset length &key little-endian sign-extend)
  ;; :NOTE the 4 is an (unsigned-byte 32) which isn't a fixnum on x86-32
  (if (and (member length '(1 2 4)) 
           (member little-endian '(t nil))
           (member sign-extend '(t nil)))
      `(let* (,@(loop
                   for i from 0 below length
                   for var in '(byte-0 byte-1 byte-2 byte-3)
                   collect `(,var (aref ,array (+ ,offset
                                                  ,(if little-endian
                                                       i
                                                       (- length i 1))))))
              (value ,(elt '(#1=byte-0
                             #2=(dpb byte-1 (byte 8 8) #1#)
                             #3=(dpb byte-2 (byte 8 16) #2#)
                             (dpb byte-3 (byte 8 24) #3#))
                           (1- length))))
         ,(if sign-extend
              `(if (logbitp ,(1- (* length 8)) value)
                   (logior ,(lognot (1- (ash 1 (1- (* length 8))))) value)
                   value)
              'value))
      form))

(defclass bmp-header ()
  ;; (bmp-file-magic                 0-1 | elided after bmp-header-check-type is invoked
  ((bmp-file-size ;; 
    :initarg :bmp-file-size
    :initform 0
    :accessor bmp-file-size)
   (bmp-file-reserved-1 ;; 04-05 
    :initarg :bmp-file-reserved-1   
    :initform 0
    :accessor bmp-file-reserved-1)
   (bmp-file-reserved-2 ;; 06-07
    :initarg :bmp-file-reserved-2
    :initform 0
    :accessor bmp-file-reserved-2)
   (bmp-file-offset ;; 08-11
    :initarg :bmp-file-offset
    :initform 0
    :accessor bmp-file-offset)
   ;;
   (bmp-image-size-key ;; 12-15
    :initarg :bmp-image-size-key
    :initform 0
    ;; AFAICT this is _never_ anywhere near a ub32...
    :accessor bmp-image-size-key)
   (bmp-image-width ;; 16-19
    :initarg :bmp-image-width
    :initform 0
    :accessor bmp-image-width)
   (bmp-image-height ;; 20-23
    :initarg :bmp-image-height   
    :initform 0
    :accessor bmp-image-height)
   (bmp-image-planes ;; 24-25
    :initarg :bmp-image-planes
    :initform 0
    :accessor bmp-image-planes)
   (bmp-image-bit-depth ;; 26-27
    :initarg :bmp-image-bit-depth
    :initform 0
    :accessor bmp-image-bit-depth)
   (bmp-image-compression ;; 28-31
    :initarg :bmp-image-compression
    :initform 0
    :accessor bmp-image-compression)
   (bmp-image-size-image ;; 32-35
    :initarg :bmp-image-size-image
    :initform 0
    :accessor bmp-image-size-image
    :documentation
    #. (format nil
    ":NOTE Are there situations where the following are _not_ true?~%
      (= (+ (bmp-image-size-image <IMG>) 
            (bmp-file-offset      <IMG>))
         (bmp-file-size           <IMG>))~%
      (= (* (bmp-image-height     <IMG>)
            (bmp-image-width      <IMG>))
         (bmp-image-size-image    <IMG>))~%"))
   (bmp-image-pixels-per-meter-x ;; 36-39
    :initarg :bmp-image-pixels-per-meter-x
    :initform 0
    :accessor bmp-image-pixels-per-meter-x)
   (bmp-image-pixels-per-meter-y ;; 40-43 
    :initarg :bmp-image-pixels-per-meter-y
    :initform 0
    :accessor bmp-image-pixels-per-meter-y)
   (bmp-image-colors-used ;; 44-47
    :initarg :bmp-image-colors-used
    :initform 0
    :accessor bmp-image-colors-used)
   (bmp-image-colors-important ;; 48-51
    :initarg :bmp-image-colors-important
    :initform 0
    :accessor bmp-image-colors-important)))

(defun make-bmp-header (bmp-path-designator)
  (let ((read-vec (read-bmp-header bmp-path-designator)))
    (declare ((simple-array (unsigned-byte 8) (52)) read-vec))
    (flet ((request-int (offset length)
             (declare ((mod 53) offset length))
             (bmp-request-integer read-vec offset length :little-endian t)))
      (make-instance 'bmp-header
                     :bmp-file-size                 (request-int 0  4)
                     :bmp-file-reserved-1           (request-int 4  2)
                     :bmp-file-reserved-2           (request-int 6  2)
                     :bmp-file-offset               (request-int 8  4)
                     :bmp-image-size-key            (request-int 12 4)
                     :bmp-image-width               (request-int 16 4)
                     :bmp-image-height              (request-int 20 4)
                     :bmp-image-planes              (request-int 24 2)
                     :bmp-image-bit-depth           (request-int 26 2)
                     :bmp-image-compression         (request-int 28 4)
                     :bmp-image-size-image          (request-int 32 4)
                     :bmp-image-pixels-per-meter-x  (request-int 36 4) ;; :sign-extend t ???
                     :bmp-image-pixels-per-meter-y  (request-int 40 4) ;; :sign-extend t ???
                     :bmp-image-colors-used         (request-int 44 4)
                     :bmp-image-colors-important    (request-int 48 4)))))

 
;;; ==============================
;;; deprecated
(defun %read-bmp-header-v-p-e (bmp-path-designator)
  (let ((bytes-read (make-array 54 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (with-open-file (bmp-hdr bmp-path-designator
                             :direction :input
                             :element-type '(unsigned-byte 8))
      (dotimes (i 54 bytes-read)
        (vector-push (read-byte bmp-hdr) bytes-read)))))

;; :NOTE really we could elide index 0 and 1 b/c they are no longer useful after checking.
(defun %bmp-header-check-type (bmp-header-vec)
  (declare ((simple-array (unsigned-byte 8) (54)) bmp-header-vec))
  (or (and (= (aref bmp-header-vec 0) 66)
           (= (aref bmp-header-vec 1) 77)
           (subseq bmp-header-vec 2 54))
      (error "file magic does not designate a valid windows bmp")))


;;; ==============================
;;; EOF
