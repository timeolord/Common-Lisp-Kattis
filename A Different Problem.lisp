;;;; Helper Functions
(defun take-input-as-list (start fn stream) (loop for i from start to (read stream)
                                                  collect (funcall fn stream)))
(defun take-input-as-list-of-list (stream) (loop for line = (read-line-as-list stream) while line collect line))
(defun print-list-newline (list) (format nil "~{~a~^~%~}" list))
(defun vector-last (vector) (elt vector (- (length vector) 1)))
(defun read-as-string (stream) (write-to-string (read stream)))
(defmacro finish ()
  `(format t "~a" (output (solve *standard-input*))))
(defmacro test ()
  `(loop for (a b) string in *tests*
         for i from 1
         do (let* ((out (with-input-from-string (str a) (output (solve str)))))
              (format t "Test ~a: ~a~%Input: ~%~a~%Output: ~%~a~%Answer: ~%~a~%~%"
                      i (string= b out) a out b))))
(defun read-line-as-list (stream)
  (let ((s (read-line stream nil nil)))
    (if s (with-input-from-string (str s)
            (loop for word = (read str nil nil) while word
                  collect word))
        nil)))
;;;; Define the tests here
(defparameter *tests*
  '(
    ("10 12
71293781758123 72784
1 12345677654321"
     "2
71293781685339
12345677654320")))

;;;; Solve Actual Problem
(defun helper (text)
  (expt (parse-integer (delete (vector-last text) text :from-end t :count 1))
        (digit-char-p (vector-last text))))
(defun input (stream)
  (take-input-as-list-of-list stream))
(defun solve (stream)
  (let ((input (input stream)))
    (loop for (a b) in input
          collect (abs (- a b)))))
(defun output (answer)
  (print-list-newline answer))

;;;; Solving Problem
(finish)

