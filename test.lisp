(defpackage :changed-stream.test
  (:use :common-lisp :changed-stream)
  (:export run-tests))

(in-package :changed-stream.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

;; All these tests assume an input string of 16 characters (0123456789ABCDEF).
;; They modify it with a diff stream and then test that
;;  * reading sequentially
;;  * via file-position forwards
;;  * via file-position backwards
;;  * via file-position then sequential read, 
;;  * via read-sequence
;;  * and via file-position then read-sequence
;; all produce the appropriate result 

;; This part can be emacs...
(flet ((test-operands (at num-deletes num-inserts)
	 (cond ((and (zerop num-deletes) (zerop num-inserts)) "-identity")
	       ((zerop num-deletes) (format nil "@~dI~d" at num-inserts))
	       ((zerop num-inserts) (format nil "@~dD~d" at num-deletes))
	       ((= num-deletes num-inserts) (format nil "@~dR~d" at num-deletes))
	       (t (format nil "@~dD~dI~d" at num-deletes num-inserts)))))
  (defun testname (at num-deletes num-inserts)
    (format nil "difftest~a" (test-operands at num-deletes num-inserts))))

(defvar *all-tests* nil)

(defstruct results
  (tests 0)
  (failures nil))
(defun results-failure-count (results) (length (results-failures results)))
(defun results-successes (results) (- (results-tests results) (results-failure-count results)))

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
  (format t "~%CHANGED-STREAM TEST RESULTS: ~%     Tests: ~a~%   Success: ~a~%  Failures: ~a" 
	  (results-tests results)
	  (results-successes results)
	  (results-failure-count results))
  (when (results-failures results)
    (present-failures results)))
  
(defun run-tests ()
  (format t "~%RUNNING CHANGED-STREAM TESTS...")
  (present-results 
   (reduce #'(lambda (test results) (runtest test results)) *all-tests* :from-end t :initial-value (make-results))))

(defmacro deftester (name (diff expected) &rest code)
  `(defun ,name (,diff ,expected)
     (format t ,(concatenate 'string "~% " (string-downcase (symbol-name name))))
     (file-position ,diff 0)
     (let ((success t)
	   (storage (make-string (length ,expected) :initial-element #\_)))
       ,@code (format t "~%") success)))

(defmacro problem (msg &rest args) `(progn (setf success nil) (format t ,(concatenate 'string "~%    * " msg) ,@args)))

(deftester test-sequential (s expected)
  (dotimes (i (length expected))
    (unless (eq i (file-position s)) (problem "Bad file-position, got ~a instead of ~d" (file-position s) i))
    (let ((char (read-char s nil :eof)))
      (if (eq char :eof)
	  (problem "EOF at ~d" i)
	  (setf (char storage i) char))))
  (let ((char (read-char s nil :eof)))
    (unless (eq char :eof) (problem "Allowed overread of ~a" char)))
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)" storage expected)))

(deftester test-peek (s expected)
  (dotimes (i (length expected))
    (unless (eq i (file-position s)) (problem "Bad file-position, got ~a instead of ~d" (file-position s) i))
    (let* ((peek (peek-char nil s nil :eof))
	   (char (read-char s nil :eof)))
      (unless (eq peek char) (problem "Peek return ~a, char return ~a at ~a" peek char i))
      (if (eq char :eof)
	  (problem "Peek caused EOF character at ~d" i)
	  (setf (char storage i) char))))
  (let ((peek (peek-char nil s nil :eof))
	(char (read-char s nil :eof)))
    (unless (eq peek char) (problem "Peek overread ~a and char overread ~a." peek char))
    (unless (eq char :eof) (problem "Peek allowed overread of ~a" char))
    (unless (string= storage expected)
      (problem "Wrong result (got ~s instead of ~s)" storage expected))))

(deftester test-file-position-forward (s expected)
  (dotimes (i (length expected))
    (file-position s i)
    (let ((char (read-char s nil :eof)))
      (if (eq char :eof)
	  (problem "~%EOF at ~2d" i)
	  (setf (char storage i) char))))
  
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)" storage expected)))

(deftester test-file-position-backward (s expected)
  (loop for i from (1- (length expected)) downto 0
        do (progn 
	     (file-position s i)
	     (let ((char (read-char s nil :eof)))
	       (if (eq char :eof)
		   (problem "EOF at ~2d" i)
		   (setf (char storage i) char)))))
  (unless (string= storage expected)
    (problem "Wrong result (got ~s instead of ~s)" storage expected)))

(deftester test-file-position-backward-with-reads (s expected)
  (loop for i from (1- (length expected)) downto 0
        do (fill storage #\_)
        do (file-position s i)
        do (loop for j from i upto (1- (length expected))
		   as char = (read-char s nil :eof)
		   if (eq char :eof)
		   do (problem "EOF reading from ~2d at ~2d: ~s" i j storage)
		   do (setf (char storage j) char))
        if (not (string= (subseq storage i) (subseq expected i)))
        do (problem "Wrong result (reading from ~2d got ~s instead of ~s)" i storage expected)))

(deftester test-read-sequences (s expected)
  (loop for start from 0 upto (1- (length expected))
        do (loop for end from start upto (1- (length expected))
	         do (file-position s start)
	         do (fill storage #\_);setf random (make-string (length expected) :initial-element #\_))
	         do (read-sequence storage s :start start :end end)
	         if (not (string= (subseq storage start end) (subseq expected start end)))
	         do (problem "Sequence (~2d, ~2d) got ~a instead of ~s" start end storage expected))))
      
(defun do-testing (at delete insert is)
  (with-input-from-string (input "0123456789ABCDEF")
    (let ((diff (change-stream input :at at :insert insert :delete delete)))
      (and (test-sequential diff is)
	   (test-peek diff is)
	   (test-file-position-forward diff is)
	   (test-file-position-backward diff is)
	   (test-file-position-backward-with-reads diff is)
	   (test-read-sequences diff is)))))
      
(defmacro test (name &key (at 0) (delete 0) (insert "") is)
  `(progn 
     (defun ,name () (do-testing ,at ,delete ,insert ,is))
     (pushnew ',name *all-tests*)))

;; Deletion Tests
(test first-test :at 0 :delete 0 :is "0123456789ABCDEF")

(test aoeia :at 0 :delete 1 :is "123456789ABCDEF")
(test ueo :at 0 :delete 2 :is "23456789ABCDEF")
(test iedi :at 0 :delete 3 :is "3456789ABCDEF")
(test pyof :at 0 :delete 8 :is "89ABCDEF")
(test oeu :at 0 :delete 9 :is "9ABCDEF")
(test oeoeu :at 0 :delete 15 :is "F")
(test yf :at 0 :delete 16 :is "")
(test gio :at 0 :delete 17 :is "")
(test jkxo :at 0 :delete 18 :is "")

(test due  :at 1 :delete 1 :is "023456789ABCDEF")
(test uoie :at 1 :delete 2 :is "03456789ABCDEF")
(test doj :at 1 :delete 3 :is "0456789ABCDEF")
(test uae :at 1 :delete 7 :is "089ABCDEF")
(test fyo  :at 1 :delete 13 :is "0EF")
(test ydie :at 1 :delete 14 :is "0F")
(test qjk :at 1 :delete 15 :is "0")
(test aoi :at 1 :delete 16 :is "0")
(test uaoei :at 1 :delete 17 :is "0")

(test nht :at 2 :delete 1 :is "013456789ABCDEF")
(test thiq :at 2 :delete 2 :is "01456789ABCDEF")
(test bqkj :at 2 :delete 7 :is "019ABCDEF")
(test pyhei  :at 2 :delete 13 :is "01F")
(test hude :at 2 :delete 14 :is "01")
(test gfy :at 2 :delete 15 :is "01")

(test uoeaaoei :at 8 :delete 1 :is "012345679ABCDEF")
(test aoiaoi  :at 8 :delete 2 :is "01234567ABCDEF")
(test yp.y.pa :at 8 :delete 7 :is "01234567F")
(test aioiou :at 8 :delete 8 :is "01234567")
(test iaoeiuq :at 8 :delete 9 :is "01234567")

(test cyhfihdi :at 14 :delete 1 :is "0123456789ABCDF")
(test gyfeidi :at 14 :delete 2 :is "0123456789ABCD")
(test gieddiu :at 14 :delete 3 :is "0123456789ABCD")

(test dieoe :at 15 :delete 1 :is "0123456789ABCDE")
(test heeoiu :at 15 :delete 2 :is "0123456789ABCDE")

(test oeuoeuo :at 16 :delete 1 :is "0123456789ABCDEF")
(test douueiu :at 16 :delete 2 :is "0123456789ABCDEF")

(test doeuei :at 17 :delete 1 :is "0123456789ABCDEF")

;; Insertion Tests
(test uodio :at 0 :insert ""   :is   "0123456789ABCDEF")
(test aoiao  :at 0 :insert "x"  :is  "x0123456789ABCDEF")
(test deiude :at 0 :insert "xy" :is "xy0123456789ABCDEF")
(test deiudi :at 0 :insert ")!@#$%^&*(abcdef" :is ")!@#$%^&*(abcdef0123456789ABCDEF")
(test deidei :at 0 :insert ")!@#$%^&*(abcdef+" :is ")!@#$%^&*(abcdef+0123456789ABCDEF")
(test hiuhui :at 0 :insert ")!@#$%^&*(abcdef+-" :is ")!@#$%^&*(abcdef+-0123456789ABCDEF")

(test dhteih :at 1 :insert "x"   :is   "0x123456789ABCDEF")
(test efiei :at 1 :insert "xy"   :is   "0xy123456789ABCDEF")
(test iedeiud :at 1 :insert ")!@#$%^&*(abcdef" :is "0)!@#$%^&*(abcdef123456789ABCDEF")
(test eidipo :at 1 :insert ")!@#$%^&*(abcdef+" :is "0)!@#$%^&*(abcdef+123456789ABCDEF")

(test of.ue :at 2 :insert "x"   :is   "01x23456789ABCDEF")
(test ieuofi :at 2 :insert "xy"   :is   "01xy23456789ABCDEF")
(test dieieo :at 2 :insert ")!@#$%^&*(abcdef" :is "01)!@#$%^&*(abcdef23456789ABCDEF")
(test duoe :at 2 :insert ")!@#$%^&*(abcdef+" :is "01)!@#$%^&*(abcdef+23456789ABCDEF")

(test doueiu :at 8 :insert "x"   :is   "01234567x89ABCDEF")
(test duoiee :at 8 :insert "xy"   :is   "01234567xy89ABCDEF")
(test duoei :at 8 :insert ")!@#$%^&*(abcdef" :is "01234567)!@#$%^&*(abcdef89ABCDEF")
(test duoiuoe :at 8 :insert ")!@#$%^&*(abcdef+" :is "01234567)!@#$%^&*(abcdef+89ABCDEF")

(test oeuoeu :at 14 :insert "x"   :is   "0123456789ABCDxEF")
(test uoieuoeuoeu :at 14 :insert "xy"   :is   "0123456789ABCDxyEF")
(test hiuhuou :at 14 :insert ")!@#$%^&*(abcdef" :is "0123456789ABCD)!@#$%^&*(abcdefEF")
(test houuoaiq :at 14 :insert ")!@#$%^&*(abcdef+" :is "0123456789ABCD)!@#$%^&*(abcdef+EF")

(test duiaoeue :at 15 :insert "x"   :is   "0123456789ABCDExF")
(test duooeuoe :at 15 :insert "xy"   :is   "0123456789ABCDExyF")
(test duouoeiu :at 15 :insert ")!@#$%^&*(abcdef" :is "0123456789ABCDE)!@#$%^&*(abcdefF")
(test doueuouoeu :at 15 :insert ")!@#$%^&*(abcdef+" :is "0123456789ABCDE)!@#$%^&*(abcdef+F")

(test oeiudouaieu :at 16 :insert "x"   :is   "0123456789ABCDEFx")
(test duoioau :at 16 :insert "xy"   :is   "0123456789ABCDEFxy")
(test daoieuoap :at 16 :insert ")!@#$%^&*(abcdef" :is "0123456789ABCDEF)!@#$%^&*(abcdef")
(test tuhudid :at 16 :insert ")!@#$%^&*(abcdef+" :is "0123456789ABCDEF)!@#$%^&*(abcdef+")

(test thhdiud :at 17 :insert "x"   :is   "0123456789ABCDEF")
(test huidhei :at 18 :insert "x"   :is   "0123456789ABCDEF")
(test hdedoddu :at 20 :insert "x"   :is   "0123456789ABCDEF")
#|
;; Replace tests
(test :at 0 :delete 1  :insert ")"                  :is  ")123456789ABCDEF")
(test :at 0 :delete 2  :insert ")!"                 :is  ")!23456789ABCDEF")
(test :at 0 :delete 8  :insert ")!@#$%^&"           :is  ")!@#$%^&89ABCDEF")
(test :at 0 :delete 14 :insert ")!@#$%^&*(abcd"     :is  ")!@#$%^&*(abcdEF")
(test :at 0 :delete 15 :insert ")!@#$%^&*(abcde"    :is  ")!@#$%^&*(abcdeF")
(test :at 0 :delete 16 :insert ")!@#$%^&*(abcdef"   :is  ")!@#$%^&*(abcdef")
(test :at 0 :delete 17 :insert ")!@#$%^&*(abcdefg"  :is  ")!@#$%^&*(abcdefg")
(test :at 0 :delete 18 :insert ")!@#$%^&*(abcdefgh" :is  ")!@#$%^&*(abcdefgh")

(test :at 1 :delete 1  :insert "!"                 :is  "0!23456789ABCDEF")
(test :at 1 :delete 8  :insert "!@#$%^&*"          :is  "0!@#$%^&*9ABCDEF")
(test :at 1 :delete 14 :insert "!@#$%^&*(abcde"    :is  "0!@#$%^&*(abcdeF")
(test :at 1 :delete 15 :insert "!@#$%^&*(abcdef"   :is  "0!@#$%^&*(abcdef")
(test :at 1 :delete 16 :insert "!@#$%^&*(abcdefg"  :is  "0!@#$%^&*(abcdefg")
(test :at 1 :delete 17 :insert "!@#$%^&*(abcdefgh" :is  "0!@#$%^&*(abcdefgh")

(test :at 8 :delete 1  :insert "*"                 :is  "01234567*9ABCDEF")
(test :at 8 :delete 7  :insert "*(abcde"           :is  "01234567*(abcdeF")
(test :at 8 :delete 8  :insert "*(abcdef"          :is  "01234567*(abcdef")
(test :at 8 :delete 9  :insert "*(abcdefg"         :is  "01234567*(abcdefg")

(test :at 14 :delete 1  :insert "e"                :is  "0123456789ABCDeF")
(test :at 14 :delete 2  :insert "ef"               :is  "0123456789ABCDef")
(test :at 14 :delete 3  :insert "efg"              :is  "0123456789ABCDefg")

(test :at 15 :delete 1  :insert "f"                :is  "0123456789ABCDEf")
(test :at 15 :delete 2  :insert "fg"               :is  "0123456789ABCDEfg")

(test :at 16 :delete 1  :insert "g"                :is  "0123456789ABCDEFg")
(test :at 16 :delete 2  :insert "gh"               :is  "0123456789ABCDEFgh")

;; Replace & Delete Tests
(test :at 0 :delete 2  :insert ")"                  :is  ")23456789ABCDEF")
(test :at 0 :delete 4  :insert ")!"                 :is  ")!456789ABCDEF")
(test :at 0 :delete 8  :insert ")!@#"               :is  ")!@#89ABCDEF")
(test :at 0 :delete 15  :insert ")!"                :is  ")!F")
(test :at 0 :delete 16  :insert ")!"                :is  ")!")
(test :at 0 :delete 17  :insert ")!"                :is  ")!")

(test :at 1 :delete 2  :insert ")"                  :is  "0)3456789ABCDEF")
(test :at 1 :delete 14  :insert ")"                  :is  "0)F")
(test :at 1 :delete 15  :insert ")"                  :is  "0)")
(test :at 1 :delete 16  :insert ")"                  :is  "0)")

(test :at 13 :delete 2  :insert ")"                  :is  "0123456789ABC)F")
(test :at 14 :delete 2  :insert ")"                  :is  "0123456789ABCD)")
(test :at 15 :delete 2  :insert ")"                  :is  "0123456789ABCDE)")
(test :at 16 :delete 2  :insert ")"                  :is  "0123456789ABCDEF)")
(test :at 17 :delete 2  :insert ")"                  :is  "0123456789ABCDEF")

;; Replace & Insert Tests
(test :at 0 :delete 1  :insert ")!"                    :is  ")!123456789ABCDEF")
(test :at 0 :delete 2  :insert ")!@#"                  :is  ")!@#23456789ABCDEF")
(test :at 0 :delete 4  :insert ")!@#$%^&"              :is  ")!@#$%^&456789ABCDEF")
(test :at 0 :delete 8  :insert ")!@#$%^&*(abcdef"      :is  ")!@#$%^&*(abcdef89ABCDEF")
(test :at 0 :delete 15  :insert ")!@#$%^&*(abcdefxyz"  :is  ")!@#$%^&*(abcdefxyzF")
(test :at 0 :delete 16  :insert ")!@#$%^&*(abcdefxyz"  :is  ")!@#$%^&*(abcdefxyz")
(test :at 0 :delete 17  :insert ")!@#$%^&*(abcdefxyz"  :is  ")!@#$%^&*(abcdefxyz")

(test :at 1 :delete 1  :insert ")!"                    :is  "0)!23456789ABCDEF")
(test :at 1 :delete 2  :insert ")!@#"                  :is  "0)!@#3456789ABCDEF")

(test :at 13 :delete 1  :insert ")!"                    :is  "0123456789ABC)!EF")
(test :at 14 :delete 1  :insert ")!"                    :is  "0123456789ABCD)!F")
(test :at 15 :delete 1  :insert ")!"                    :is  "0123456789ABCDE)!")
(test :at 16 :delete 1  :insert ")!"                    :is  "0123456789ABCDEF)!")
(test :at 17 :delete 1  :insert ")!"                    :is  "0123456789ABCDEF")

|#
