;;; :FILE-CREATED <Timestamp: #{2011-08-02T11:59:38-04:00Z}#{11312} - by MON>
;;; :FILE cl-bmp/cl-bmp-docs.lisp
;;; ==============================

(in-package #:cl-bmp)
;; *package*


#|| 

  bmp-file-magic                 0-1 | elided after bmp-header-check-type is invoked
  bmp-file-size                  2-5 | 00-03
  bmp-file-reserved-1            6-7 | 04-05 
  bmp-file-reserved-2            8-9 | 06-07
  bmp-file-offset              10-13 | 08-11
  -------------------------------------------- 14 octets 
  bmp-image-size-key            14-17 | 12-15
  bmp-image-width               18-21 | 16-19
  bmp-image-height              22-25 | 20-23
  bmp-image-planes              26-27 | 24-25
  bmp-image-bit-depth           28-29 | 26-27
  bmp-image-compression         30-33 | 28-31
  bmp-image-size-image          34-37 | 32-35
  bmp-image-pixels-per-meter-x  38-41 | 36-39
  bmp-image-pixels-per-meter-y  42-45 | 40-43 
  bmp-image-colors-used         46-59 | 44-47
  bmp-image-colors-important    50-53 | 48-51
 -------------------------------------------- 40 octets
                                              54 total octets

 ---------------------------------------------------------------
 bmp-file-magic                   2 ASCII chars (#\B #\M)    
 bmp-file-size                    4 octets (unsigned-byte 32)
 bmp-file-reserved-1              2 octets (unsigned-byte 16)
 bmp-file-reserved-2              2 octets (unsigned-byte 16)
 bmp-file-offset                  4 octets (unsigned-byte 32)   
 ------------------------------- 14 octets ---------------------
                                                              ;; what exiftool calls 
 bmp-image-size-key               4 octets (unsigned-byte 32) ;; 
 bmp-image-width                  4 octets (signed-byte 32)   ;; ImageWidth
 bmp-image-height                 4 octets (signed-byte 32)   ;; ImageHeight
 bmp-image-planes                 2 octets (unsigned-byte 16) ;; Planes
 bmp-image-bit-depth              2 octets (unsigned-byte 16) ;; BitDepth
 bmp-image-compression            4 octets (unsigned-byte 32) ;; BI_RGB, BI_RLE8,   BI_RLE4,   BI_BITFIELDS -- SDL 
                                                              ;; None,   8-Bit RLE, 4-Bit RLE, Bitfields, JPEG (#2), PNG (#2)
 bmp-image-size-image             4 octets (unsigned-byte 32) ;; ImageLength 
 bmp-image-pixels-per-meter-x     4 octets (signed-byte 32)   ;; PixelsPerMeterX
 bmp-image-pixels-per-meter-y     4 octets (signed-byte 32)   ;; PixelsPerMeterY
 bmp-image-colors-used            4 octets (unsigned-byte 32) ;; NumColors
 bmp-image-colors-important       4 octets (unsigned-byte 32) ;; NumImportantColors
 ------------------------------- 40 octets ---------------------
                                 54 total octets                                  
 ---------------------------------------------------------------


 (let ((bytes-read (make-array 54 :element-type '(unsigned-byte 8) :fill-pointer 0)))
   (with-open-file (bmp-hdr (merge-pathnames (make-pathname :directory '(:relative "example-images")
                                                            :name "Dict_20th_Cent_design_and_Designers001-100"
                                                            :type "bmp")
                                             *default-pathname-defaults*)
                    :direction :input
                    :element-type '(unsigned-byte 8))
     (dotimes (i 54 bytes-read)
       (vector-push (read-byte bmp-hdr) bytes-read))))
 #(66 77 150 84 0 0 0 0 0 0 54 0 0 0 40 0 0 0 72 0 0 0 100 0 0 0 1 0 24 0 0 0 0
   0 96 84 0 0 131 61 0 0 131 61 0 0 0 0 0 0 0 0 0 0)

  Deconstructing a windows BMP header:
 #(66          ;; #\B |
   77          ;; #\M |-> bmp-file-magic
   150 84 0 0  ;; bmp-file-size
   0 0         ;; bmp-file-reserved-1 
   0 0         ;; bmp-file-reserved-2 
   54 0 0 0    ;; bmp-file-offset
   40 0 0 0    ;; bmp-image-size    'Windows V3'
   72 0 0 0    ;; bmp-image-width   
   100 0 0 0   ;; bmp-image-height   72x100
   1 0         ;; bmp-image-planes
   24 0        ;; bmp-image-bit-depth  
   0 0 0 0     ;; bmp-image-compression
   96 84 0 0   ;; bmp-image-size-image
   131 61 0 0  ;; bmp-image-x-pixels-per-meter
   131 61 0 0  ;; bmp-image-y-pixels-per-meter
   0 0 0 0     ;; bmp-image-clr-used       ;; when NumColors & NumImportantColors are both 0 
   0 0 0 0     ;; bmp-image-clr-important  ;; exiftool reports Use BitDepth and All respectively
   )

 ,----
 | shell> exiftool Dict_20th_Cent_design_and_Designers001-100.bmp
 |        ExifTool Version Number         : 7.89
 |        File Name                       : Dict_20th_Cent_design_and_Designers001-100.bmp
 |        Directory                       : .
 |        File Size                       : 21 kB
 |        File Modification Date/Time     : 2011:08:01 20:57:30-04:00
 |        File Type                       : BMP
 |        MIME Type                       : image/bmp
 |        Image Width                     : 72
 |        Image Height                    : 100
 |        Planes                          : 1
 |        Bit Depth                       : 24
 |        Compression                     : None
 |        Image Length                    : 21600
 |        Pixels Per Meter X              : 15747
 |        Pixels Per Meter Y              : 15747
 |        Num Colors                      : Use BitDepth
 |        Num Important Colors            : All
 |        Image Size                      : 72x100
 `----

||#

;;; ==============================
;;; EOF
