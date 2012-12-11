
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CHANGED-STREAM; Base: 10 -*-

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

(defpackage :changed-stream
  (:use :common-lisp #+sbcl :sb-gray #+allegro :excl #+clisp :gray #+ecl :gray #+lispworks :stream)
  (:export changed-stream change-stream))

(in-package :changed-stream)

(defclass changed-stream (fundamental-character-input-stream)
  ((stream                    :initarg  :stream                    
                              :reader stream-of)
   (virtual-position          :initform 0
                              :accessor virtual-position)
   (last-unchanged-position   :initarg  :last-unchanged-position  
                              :reader last-unchanged-position)
   (last-replacement-position :initarg  :last-replacement-position 
                              :reader last-replacement-position)
   (last-modified-position    :initarg  :last-modified-position    
                              :reader last-modified-position)
   (removed-characters        :initarg  :removed-characters        
                              :reader removed-characters)
   (insert-string             :initarg  :string  
                              :reader insert-string)))

(defun mod-is-delete-p (stream) (> (removed-characters stream) 0))

(defun change-stream (stream &key (at 0) (insert "") (delete 0))
  (assert (zerop (file-position stream)))
  (let ((last-replace (+ at (min (length insert) delete)))
        (last-mod (+ at (max delete (length insert)))))
    (make-instance 'changed-stream
       :stream stream
       :last-unchanged-position at
       :last-replacement-position last-replace
       :last-modified-position last-mod
       :removed-characters (- delete (length insert)) 
       :string insert)))

(defmacro diffcase ((position stream)
                    (before &rest before-case-code)
                    (replace &rest replace-case-code)
                    (delete &rest delete-case-code)
                    (insert &rest insert-case-code)
                    (after &rest after-case-code))
  (declare (ignore before replace delete insert after))
  (let ((pos (gensym)) (strm (gensym)))
    `(let ((,pos ,position) (,strm ,stream))
       (cond ((< ,pos (last-unchanged-position ,strm)) ,@before-case-code)
             ((< ,pos (last-replacement-position ,strm)) ,@replace-case-code)
             ((< ,pos (last-modified-position ,strm))
              (if (mod-is-delete-p ,strm)
                  (progn ,@delete-case-code)
                  (progn ,@insert-case-code)))
             (t ,@after-case-code)))))

(defmethod stream-file-position ((stream changed-stream) &optional newval)
  (if newval 
      (progn 
        (diffcase (newval stream)
           (before  (file-position (stream-of stream) newval))
           (replace (file-position (stream-of stream) newval))
           (delete  (file-position (stream-of stream)
                                   (+ newval (removed-characters stream))))
           (insert  (file-position (stream-of stream)
                                   (last-replacement-position stream)))
           (after   (file-position (stream-of stream)
                                   (+ newval (removed-characters stream)))))
        (setf (virtual-position stream) newval))
      (virtual-position stream)))

(labels ((virtual-char (stream &optional peek)
           (let ((read-pos (- (virtual-position stream) (last-unchanged-position stream))))
             (or peek (incf (virtual-position stream)))
             (if (>= read-pos (length (insert-string stream))) :eof (char (insert-string stream) read-pos))))
         (real-char (stream)
           (prog1 (read-char (stream-of stream) nil :eof)
             (incf (virtual-position stream))))
         (replacement-char (stream)
           (read-char (stream-of stream) nil :eof)
           (virtual-char stream))
         (internal-seek-end-of-delete (stream)
           (loop repeat (- (+ (removed-characters stream) (virtual-position stream)) (file-position (stream-of stream)))
              do (read-char (stream-of stream) nil :eof))))
  (defmethod stream-read-char ((stream changed-stream))
    (diffcase ((if (mod-is-delete-p stream)
                   (file-position (stream-of stream))
                   (virtual-position stream)) 
               stream)
       (before  (real-char stream))
       (replace (replacement-char stream))
       (delete  (internal-seek-end-of-delete stream) (real-char stream))
       (insert (virtual-char stream))
       (after  (real-char stream))))
  (defmethod stream-peek-char ((stream changed-stream))
      (diffcase ((if (mod-is-delete-p stream)
                     (file-position (stream-of stream))
                     (virtual-position stream)) stream)
        (before  (peek-char nil (stream-of stream) nil :eof))
        (replace (virtual-char stream t))
        (delete  (internal-seek-end-of-delete stream) 
                 (peek-char nil (stream-of stream) nil :eof))
        (insert  (virtual-char stream t))
        (after   (peek-char nil (stream-of stream) nil :eof)))))

(defmethod stream-read-sequence ((stream changed-stream) seq &optional start end)
  (let ((position (virtual-position stream)))
    (let ((write-length (- (or end (length seq)) (or start 0))))
      (let ((before-length (min write-length
                            (max 0 (- (last-unchanged-position stream) position)))))
        (read-sequence seq (stream-of stream)
                       :start start
                       :end (+ start before-length))
        (incf start before-length)
        (incf position before-length)
        (decf write-length before-length)
        (file-position stream position))
      
      (let ((during-length (min write-length 
                            (max 0 (- (last-modified-position stream) position)))))
        (call-next-method stream seq start (+ start during-length))
        (incf start during-length)
        (incf position during-length)
        (decf write-length during-length))
      
      (let ((after-length write-length))
        (read-sequence seq (stream-of stream)
                       :start start
                       :end (+ start after-length))
        (incf start after-length)
        (incf position after-length)
        (decf write-length after-length)
        (file-position stream position)))))
