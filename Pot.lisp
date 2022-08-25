;;;; Helper Functions
(defun take-input-as-list (start fn stream) (loop for i from start to (read stream)
                                                  collect (funcall fn stream)))
(defun print-list-newline (list) (format nil "~{~a~%~}" list))
(defun vector-last (vector) (elt vector (- (length vector) 1)))
(defun read-as-string (stream) (write-to-string (read stream)))
(defmacro finish (read-fn start)
  `(format t "~a" (solve (take-input-as-list ,start #',read-fn *standard-input*))))
(defun test (solve read-fn start)
  (loop for (a b) string in *tests*
        for i from 1
        do (let ((input (with-input-from-string (str a) (take-input-as-list start #'read-fn str))))
             (format t "Test ~a: ~a~%Input: ~a~%Output: ~a~%Answer: ~a~%~%"
                     i (string= b (write-to-string (funcall solve input))) a (funcall solve input) b))))
;;;; Define the tests here
(defparameter *tests*
  '(
    ("2 212 1253"
     "1953566")
    ("5 23 17 43 52 22"
     "102")
    ("3 213 102 45"
     "10385")))

;;;; Solve Actual Problem
(defun helper (text)
  (expt (parse-integer (delete (vector-last text) text :from-end t :count 1))
        (digit-char-p (vector-last text))))
(defun solve (input)
  (reduce #'+ (mapcar #'helper input)))

;;;; Solving Problem
(finish read-as-string 1)
