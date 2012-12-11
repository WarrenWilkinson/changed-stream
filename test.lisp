
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CHANGED-STREAM.TEST; Base: 10 -*-

;;; Copyright (c) 20012-2013, Warren Wilkinson.  All rights reserved.

;;; BEGIN_LICENSE:LGPL2
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Library General Public License as published by
;;; the Free Software Foundation; version 2.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public License
;;; along with this library; see the file COPYING.LIB.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; END_LICENSE

(defpackage :changed-stream.test
  (:use :common-lisp :changed-stream)
  (:export run-tests))

(in-package :changed-stream.test)

(defvar *all-tests* nil)
(defstruct results
  (tests 0)
  (failures nil))
(defun results-failure-count (results)
  (length (results-failures results)))
(defun results-successes (results)
  (- (results-tests results)
     (results-failure-count results)))

(defun runtest (fun results)
  (let* ((success t)
         (output (with-output-to-string (*standard-output*)
                   (setf success (funcall fun)))))
    (make-results
     :tests (1+ (results-tests results))
     :failures (if success
                   (results-failures results)
                   (acons fun output (results-failures results))))))

(defun present-failures (results)
  (format t "~%CHANGED-STREAM FAILURES:~%")
  (loop for (fn . problems) in (results-failures results)
        do (format t "~%~a~a~%" fn problems)))
(defun present-results (results)
  (format t "~%CHANGED-STREAM TEST RESULTS:")
  (format t "~%     Tests: ~a~%   Success: ~a~%  Failures: ~a" 
          (results-tests results)
          (results-successes results)
          (results-failure-count results))
  (when (results-failures results)
    (present-failures results)))
  
(defun run-tests ()
  (format t "~%RUNNING CHANGED-STREAM TESTS...")
  (present-results 
   (reduce #'(lambda (test results) (runtest test results))
           *all-tests* :from-end t :initial-value (make-results))))     
(defmacro defpattern (name (diff expected) &rest code)
  `(defun ,name (,diff ,expected)
     (format t ,(concatenate 'string "~% " (string-downcase (symbol-name name))))
     (file-position ,diff 0)
     (let ((success t)
           (storage (make-string (length ,expected) :initial-element #\_)))
       ,@code (format t "~%") success)))

(defmacro problem (msg &rest args)
  `(progn
     (setf success nil)
     (format t ,(concatenate 'string "~%    * " msg)
             ,@args)))
(defpattern pattern-sequential (s expected)
  (dotimes (i (length expected))
    (unless (eq i (file-position s)) 
      (problem "Bad file-position, got ~a instead of ~d" 
               (file-position s) i))
    (let ((char (read-char s nil :eof)))
      (if (eq char :eof)
          (problem "EOF at ~d" i)
          (setf (char storage i) char))))
  (let ((char (read-char s nil :eof)))
    (unless (eq char :eof) (problem "Allowed overread of ~a" char)))
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)" storage expected)))


(defpattern pattern-sequential+peek (s expected)
  (dotimes (i (length expected))
    (unless (eq i (file-position s)) 
      (problem "Bad file-position, got ~a instead of ~d"
               (file-position s) i))
    (let* ((peek (peek-char nil s nil :eof))
           (char (read-char s nil :eof)))
      (unless (eq peek char)
        (problem "Peek return ~a, char return ~a at ~a" peek char i))
      (if (eq char :eof)
          (problem "Peek caused EOF character at ~d" i)
          (setf (char storage i) char))))
  (let ((peek (peek-char nil s nil :eof))
        (char (read-char s nil :eof)))
    (unless (eq peek char)
      (problem "Peek overread ~a and char overread ~a." peek char))
    (unless (eq char :eof) 
      (problem "Peek allowed overread of ~a" char))
    (unless (string= storage expected)
      (problem "Wrong result (got ~s instead of ~s)"
               storage expected))))

(defpattern pattern-file-position-forward (s expected)
  (dotimes (i (length expected))
    (file-position s i)
    (let ((char (read-char s nil :eof)))
      (if (eq char :eof)
          (problem "~%EOF at ~2d" i)
          (setf (char storage i) char))))
  
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)"
             storage expected)))
(defpattern pattern-file-position-backward (s expected)
  (loop for i from (1- (length expected)) downto 0
        do (progn 
             (file-position s i)
             (let ((char (read-char s nil :eof)))
               (if (eq char :eof)
                   (problem "EOF at ~2d" i)
                   (setf (char storage i) char)))))
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)"
             storage expected)))
(defpattern pattern-file-position-backward-with-reads (s expected)
  (loop for i from (1- (length expected)) downto 0
        do (fill storage #\_)
        do (file-position s i)
        do (loop for j from i upto (1- (length expected))
                   as char = (read-char s nil :eof)
                   if (eq char :eof)
                   do (problem "EOF reading from ~2d at ~2d: ~s"
                               i j storage)
                   do (setf (char storage j) char))
        if (not (string= (subseq storage i) (subseq expected i)))
        do (problem "Wrong result (reading from ~2d got ~s instead of ~s)"
                    i storage expected)))

(defpattern pattern-read-sequences (s expected)
  (loop for start from 0 upto (1- (length expected))
        do (loop for end from start upto (1- (length expected))
                 do (file-position s start)
                 do (fill storage #\_)
                 do (read-sequence storage s :start start :end end)
                 if (not (string= (subseq storage start end)
                                  (subseq expected start end)))
                 do (problem "Sequence (~2d, ~2d) got ~a instead of ~s"
                             start end storage expected))))
(defun do-testing (at delete insert is)
  (with-input-from-string (input "0123456789ABCDEF")
    (let ((diff (change-stream input :at at :insert insert :delete delete)))
      (and (pattern-sequential diff is)
           (pattern-sequential+peek diff is)
           (pattern-file-position-forward diff is)
           (pattern-file-position-backward diff is)
           (pattern-file-position-backward-with-reads diff is)
           (pattern-read-sequences diff is)))))
      
(defmacro deftest (name &key (at 0) (delete 0) (insert "") is)
  `(progn 
     (defun ,name () (do-testing ,at ,delete ,insert ,is))
     (pushnew ',name *all-tests*)))

;; Deletion Test
(deftest   changed-stream-no-op :at  0 :delete  0 :insert                    "" :is "0123456789ABCDEF")
(deftest   changed-stream@00D01 :at  0 :delete  1 :insert                    "" :is "123456789ABCDEF")
(deftest   changed-stream@00D02 :at  0 :delete  2 :insert                    "" :is "23456789ABCDEF")
(deftest   changed-stream@00D03 :at  0 :delete  3 :insert                    "" :is "3456789ABCDEF")
(deftest   changed-stream@00D08 :at  0 :delete  8 :insert                    "" :is "89ABCDEF")
(deftest   changed-stream@00D09 :at  0 :delete  9 :insert                    "" :is "9ABCDEF")
(deftest   changed-stream@00D15 :at  0 :delete 15 :insert                    "" :is "F")
(deftest   changed-stream@00D16 :at  0 :delete 16 :insert                    "" :is "")
(deftest   changed-stream@00D17 :at  0 :delete 17 :insert                    "" :is "")
(deftest   changed-stream@00D18 :at  0 :delete 18 :insert                    "" :is "")
(deftest   changed-stream@01D01 :at  1 :delete  1 :insert                    "" :is "023456789ABCDEF")
(deftest   changed-stream@01D02 :at  1 :delete  2 :insert                    "" :is "03456789ABCDEF")
(deftest   changed-stream@01D03 :at  1 :delete  3 :insert                    "" :is "0456789ABCDEF")
(deftest   changed-stream@01D07 :at  1 :delete  7 :insert                    "" :is "089ABCDEF")
(deftest   changed-stream@01D13 :at  1 :delete 13 :insert                    "" :is "0EF")
(deftest   changed-stream@01D14 :at  1 :delete 14 :insert                    "" :is "0F")
(deftest   changed-stream@01D15 :at  1 :delete 15 :insert                    "" :is "0")
(deftest   changed-stream@01D16 :at  1 :delete 16 :insert                    "" :is "0")
(deftest   changed-stream@01D17 :at  1 :delete 17 :insert                    "" :is "0")
(deftest   changed-stream@02D01 :at  2 :delete  1 :insert                    "" :is "013456789ABCDEF")
(deftest   changed-stream@02D02 :at  2 :delete  2 :insert                    "" :is "01456789ABCDEF")
(deftest   changed-stream@02D07 :at  2 :delete  7 :insert                    "" :is "019ABCDEF")
(deftest   changed-stream@02D13 :at  2 :delete 13 :insert                    "" :is "01F")
(deftest   changed-stream@02D14 :at  2 :delete 14 :insert                    "" :is "01")
(deftest   changed-stream@02D15 :at  2 :delete 15 :insert                    "" :is "01")
(deftest   changed-stream@08D01 :at  8 :delete  1 :insert                    "" :is "012345679ABCDEF")
(deftest   changed-stream@08D02 :at  8 :delete  2 :insert                    "" :is "01234567ABCDEF")
(deftest   changed-stream@08D07 :at  8 :delete  7 :insert                    "" :is "01234567F")
(deftest   changed-stream@08D08 :at  8 :delete  8 :insert                    "" :is "01234567")
(deftest   changed-stream@08D09 :at  8 :delete  9 :insert                    "" :is "01234567")
(deftest   changed-stream@14D01 :at 14 :delete  1 :insert                    "" :is "0123456789ABCDF")
(deftest   changed-stream@14D02 :at 14 :delete  2 :insert                    "" :is "0123456789ABCD")
(deftest   changed-stream@14D03 :at 14 :delete  3 :insert                    "" :is "0123456789ABCD")
(deftest   changed-stream@15D01 :at 15 :delete  1 :insert                    "" :is "0123456789ABCDE")
(deftest   changed-stream@15D02 :at 15 :delete  2 :insert                    "" :is "0123456789ABCDE")
(deftest   changed-stream@16D01 :at 16 :delete  1 :insert                    "" :is "0123456789ABCDEF")
(deftest   changed-stream@16D02 :at 16 :delete  2 :insert                    "" :is "0123456789ABCDEF")
(deftest   changed-stream@17D01 :at 17 :delete  1 :insert                    "" :is "0123456789ABCDEF")

;; Insertion Tests
(deftest   changed-stream@00I01 :at  0 :delete  0 :insert                   "x" :is "x0123456789ABCDEF")
(deftest   changed-stream@00I02 :at  0 :delete  0 :insert                  "xy" :is "xy0123456789ABCDEF")
(deftest   changed-stream@00I16 :at  0 :delete  0 :insert    ")!@#$%^&*(abcdef" :is ")!@#$%^&*(abcdef0123456789ABCDEF")
(deftest   changed-stream@00I17 :at  0 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is ")!@#$%^&*(abcdef+0123456789ABCDEF")
(deftest   changed-stream@00I18 :at  0 :delete  0 :insert  ")!@#$%^&*(abcdef+-" :is ")!@#$%^&*(abcdef+-0123456789ABCDEF")
(deftest   changed-stream@01I01 :at  1 :delete  0 :insert                   "x" :is "0x123456789ABCDEF")
(deftest   changed-stream@01I02 :at  1 :delete  0 :insert                  "xy" :is "0xy123456789ABCDEF")
(deftest   changed-stream@01I16 :at  1 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "0)!@#$%^&*(abcdef123456789ABCDEF")
(deftest   changed-stream@01I17 :at  1 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "0)!@#$%^&*(abcdef+123456789ABCDEF")
(deftest   changed-stream@02I01 :at  2 :delete  0 :insert                   "x" :is "01x23456789ABCDEF")
(deftest   changed-stream@02I02 :at  2 :delete  0 :insert                  "xy" :is "01xy23456789ABCDEF")
(deftest   changed-stream@02I16 :at  2 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "01)!@#$%^&*(abcdef23456789ABCDEF")
(deftest   changed-stream@02I17 :at  2 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "01)!@#$%^&*(abcdef+23456789ABCDEF")
(deftest   changed-stream@08I01 :at  8 :delete  0 :insert                   "x" :is "01234567x89ABCDEF")
(deftest   changed-stream@08I02 :at  8 :delete  0 :insert                  "xy" :is "01234567xy89ABCDEF")
(deftest   changed-stream@08I16 :at  8 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "01234567)!@#$%^&*(abcdef89ABCDEF")
(deftest   changed-stream@08I17 :at  8 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "01234567)!@#$%^&*(abcdef+89ABCDEF")
(deftest   changed-stream@14I01 :at 14 :delete  0 :insert                   "x" :is "0123456789ABCDxEF")
(deftest   changed-stream@14I02 :at 14 :delete  0 :insert                  "xy" :is "0123456789ABCDxyEF")
(deftest   changed-stream@14I16 :at 14 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "0123456789ABCD)!@#$%^&*(abcdefEF")
(deftest   changed-stream@14I17 :at 14 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "0123456789ABCD)!@#$%^&*(abcdef+EF")
(deftest   changed-stream@15I01 :at 15 :delete  0 :insert                   "x" :is "0123456789ABCDExF")
(deftest   changed-stream@15I02 :at 15 :delete  0 :insert                  "xy" :is "0123456789ABCDExyF")
(deftest   changed-stream@15I16 :at 15 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "0123456789ABCDE)!@#$%^&*(abcdefF")
(deftest   changed-stream@15I17 :at 15 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "0123456789ABCDE)!@#$%^&*(abcdef+F")
(deftest   changed-stream@16I01 :at 16 :delete  0 :insert                   "x" :is "0123456789ABCDEFx")
(deftest   changed-stream@16I02 :at 16 :delete  0 :insert                  "xy" :is "0123456789ABCDEFxy")
(deftest   changed-stream@16I16 :at 16 :delete  0 :insert    ")!@#$%^&*(abcdef" :is "0123456789ABCDEF)!@#$%^&*(abcdef")
(deftest   changed-stream@16I17 :at 16 :delete  0 :insert   ")!@#$%^&*(abcdef+" :is "0123456789ABCDEF)!@#$%^&*(abcdef+")
(deftest   changed-stream@17I01 :at 17 :delete  0 :insert                   "x" :is "0123456789ABCDEF")
(deftest   changed-stream@18I01 :at 18 :delete  0 :insert                   "x" :is "0123456789ABCDEF")
(deftest   changed-stream@20I01 :at 20 :delete  0 :insert                   "x" :is "0123456789ABCDEF")

;; Replacement Tests
(deftest   changed-stream@00R01 :at  0 :delete  1 :insert                   ")" :is ")123456789ABCDEF")
(deftest   changed-stream@00R02 :at  0 :delete  2 :insert                  ")!" :is ")!23456789ABCDEF")
(deftest   changed-stream@00R08 :at  0 :delete  8 :insert            ")!@#$%^&" :is ")!@#$%^&89ABCDEF")
(deftest   changed-stream@00R14 :at  0 :delete 14 :insert      ")!@#$%^&*(abcd" :is ")!@#$%^&*(abcdEF")
(deftest   changed-stream@00R15 :at  0 :delete 15 :insert     ")!@#$%^&*(abcde" :is ")!@#$%^&*(abcdeF")
(deftest   changed-stream@00R16 :at  0 :delete 16 :insert    ")!@#$%^&*(abcdef" :is ")!@#$%^&*(abcdef")
(deftest   changed-stream@00R17 :at  0 :delete 17 :insert   ")!@#$%^&*(abcdefg" :is ")!@#$%^&*(abcdefg")
(deftest   changed-stream@00R18 :at  0 :delete 18 :insert  ")!@#$%^&*(abcdefgh" :is ")!@#$%^&*(abcdefgh")
(deftest   changed-stream@01R01 :at  1 :delete  1 :insert                   "!" :is "0!23456789ABCDEF")
(deftest   changed-stream@01R08 :at  1 :delete  8 :insert            "!@#$%^&*" :is "0!@#$%^&*9ABCDEF")
(deftest   changed-stream@01R14 :at  1 :delete 14 :insert      "!@#$%^&*(abcde" :is "0!@#$%^&*(abcdeF")
(deftest   changed-stream@01R15 :at  1 :delete 15 :insert     "!@#$%^&*(abcdef" :is "0!@#$%^&*(abcdef")
(deftest   changed-stream@01R16 :at  1 :delete 16 :insert    "!@#$%^&*(abcdefg" :is "0!@#$%^&*(abcdefg")
(deftest   changed-stream@01R17 :at  1 :delete 17 :insert   "!@#$%^&*(abcdefgh" :is "0!@#$%^&*(abcdefgh")
(deftest   changed-stream@08R01 :at  8 :delete  1 :insert                   "*" :is "01234567*9ABCDEF")
(deftest   changed-stream@08R07 :at  8 :delete  7 :insert             "*(abcde" :is "01234567*(abcdeF")
(deftest   changed-stream@08R08 :at  8 :delete  8 :insert            "*(abcdef" :is "01234567*(abcdef")
(deftest   changed-stream@08R09 :at  8 :delete  9 :insert           "*(abcdefg" :is "01234567*(abcdefg")
(deftest   changed-stream@14R01 :at 14 :delete  1 :insert                   "e" :is "0123456789ABCDeF")
(deftest   changed-stream@14R02 :at 14 :delete  2 :insert                  "ef" :is "0123456789ABCDef")
(deftest   changed-stream@14R03 :at 14 :delete  3 :insert                 "efg" :is "0123456789ABCDefg")
(deftest   changed-stream@15R01 :at 15 :delete  1 :insert                   "f" :is "0123456789ABCDEf")
(deftest   changed-stream@15R02 :at 15 :delete  2 :insert                  "fg" :is "0123456789ABCDEfg")
(deftest   changed-stream@16R01 :at 16 :delete  1 :insert                   "g" :is "0123456789ABCDEFg")
(deftest   changed-stream@16R02 :at 16 :delete  2 :insert                  "gh" :is "0123456789ABCDEFgh")

;; Replacement and Deletion Tests
(deftest changed-stream@00D02I01 :at  0 :delete  2 :insert                   ")" :is ")23456789ABCDEF")
(deftest changed-stream@00D04I02 :at  0 :delete  4 :insert                  ")!" :is ")!456789ABCDEF")
(deftest changed-stream@00D08I04 :at  0 :delete  8 :insert                ")!@#" :is ")!@#89ABCDEF")
(deftest changed-stream@00D15I02 :at  0 :delete 15 :insert                  ")!" :is ")!F")
(deftest changed-stream@00D16I02 :at  0 :delete 16 :insert                  ")!" :is ")!")
(deftest changed-stream@00D17I02 :at  0 :delete 17 :insert                  ")!" :is ")!")
(deftest changed-stream@01D02I01 :at  1 :delete  2 :insert                   ")" :is "0)3456789ABCDEF")
(deftest changed-stream@01D14I01 :at  1 :delete 14 :insert                   ")" :is "0)F")
(deftest changed-stream@01D15I01 :at  1 :delete 15 :insert                   ")" :is "0)")
(deftest changed-stream@01D16I01 :at  1 :delete 16 :insert                   ")" :is "0)")
(deftest changed-stream@13D02I01 :at 13 :delete  2 :insert                   ")" :is "0123456789ABC)F")
(deftest changed-stream@14D02I01 :at 14 :delete  2 :insert                   ")" :is "0123456789ABCD)")
(deftest changed-stream@15D02I01 :at 15 :delete  2 :insert                   ")" :is "0123456789ABCDE)")
(deftest changed-stream@16D02I01 :at 16 :delete  2 :insert                   ")" :is "0123456789ABCDEF)")
(deftest changed-stream@17D02I01 :at 17 :delete  2 :insert                   ")" :is "0123456789ABCDEF")

;; Replacement and Insertion Tests
(deftest changed-stream@00D01I02 :at  0 :delete  1 :insert                  ")!" :is ")!123456789ABCDEF")
(deftest changed-stream@00D02I04 :at  0 :delete  2 :insert                ")!@#" :is ")!@#23456789ABCDEF")
(deftest changed-stream@00D04I08 :at  0 :delete  4 :insert            ")!@#$%^&" :is ")!@#$%^&456789ABCDEF")
(deftest changed-stream@00D08I16 :at  0 :delete  8 :insert    ")!@#$%^&*(abcdef" :is ")!@#$%^&*(abcdef89ABCDEF")
(deftest changed-stream@00D15I19 :at  0 :delete 15 :insert ")!@#$%^&*(abcdefxyz" :is ")!@#$%^&*(abcdefxyzF")
(deftest changed-stream@00D16I19 :at  0 :delete 16 :insert ")!@#$%^&*(abcdefxyz" :is ")!@#$%^&*(abcdefxyz")
(deftest changed-stream@00D17I19 :at  0 :delete 17 :insert ")!@#$%^&*(abcdefxyz" :is ")!@#$%^&*(abcdefxyz")
(deftest changed-stream@01D01I02 :at  1 :delete  1 :insert                  ")!" :is "0)!23456789ABCDEF")
(deftest changed-stream@01D02I04 :at  1 :delete  2 :insert                ")!@#" :is "0)!@#3456789ABCDEF")
(deftest changed-stream@13D01I02 :at 13 :delete  1 :insert                  ")!" :is "0123456789ABC)!EF")
(deftest changed-stream@14D01I02 :at 14 :delete  1 :insert                  ")!" :is "0123456789ABCD)!F")
(deftest changed-stream@15D01I02 :at 15 :delete  1 :insert                  ")!" :is "0123456789ABCDE)!")
(deftest changed-stream@16D01I02 :at 16 :delete  1 :insert                  ")!" :is "0123456789ABCDEF)!")
(deftest changed-stream@17D01I02 :at 17 :delete  1 :insert                  ")!" :is "0123456789ABCDEF")
