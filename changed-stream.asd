
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-

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

(defsystem :changed-stream
  :name "changed-stream"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :license "lgpl2"
  :description "A library for non-destructive changes to character streams."
  :long-description "A library for non-destructive 'changes' to character streams. Specify 1) position of 'point', 2) # characters deleted at, 3) new string inserted at point."
  :components ((:file "changed-stream"))
  :in-order-to ((test-op (load-op changed-stream.test))))
  
(defmethod perform ((op asdf:test-op) (system (eql (find-system :changed-stream))))
  (funcall (intern "RUN-TESTS" :changed-stream.test)))
