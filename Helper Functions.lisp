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
         do (let ((out (with-input-from-string (str a) (output (solve str)))))
              (format t "Test ~a: ~a~%Input: ~%~a~%Output: ~%~a~%Answer: ~%~a~%~%"
                      i (string= b out) a out b))))
(defun read-line-as-list (stream)
  (let ((s (read-line stream nil nil)))
    (if s (with-input-from-string (str s)
            (loop for word = (read str nil nil) while word
                  collect word))
        nil)))
(defun print-matrix (matrix)
  (destructuring-bind (x y) (array-dimensions matrix)
    (loop for i from 0 below x do
          (progn
            (loop for j from 0 below y do
                  (format t "~a " (aref matrix i j)))
            (terpri)))))
(defun print-mirror (matrix)
  (destructuring-bind (x y) (array-dimensions matrix)
    (loop for i from 0 below x do
          (progn
            (if (or (= i 0) (= i (- x 1)))
                (progn
                  (format t "~v:@<~A~>" 4 "")
                  (loop for j from 1 below (- y 1) do
                        (format t "~v:@<~A~>" 4 (aref matrix i j))))
                (loop for j from 0 below y do
                      (format t "~v:@<~A~>" 4 (aref matrix i j))))
            (terpri)))))
(defun create-mirror (n m)
  (let ((arr (make-array `(,(+ n 2) ,(+ m 2)))) (num 0))
    (loop for r from 1 to n do
          (setf (aref arr r 0) (incf num)))
    (loop for c from 1 to m do
          (setf (aref arr (+ n 1) c) (incf num)))
    (loop for r from n downto 1 do
          (setf (aref arr r (+ m 1)) (incf num)))
    (loop for c from m downto 1 do
          (setf (aref arr 0 c) (incf num)))
    arr))
(defmacro between)
(defun set-mirror (arr n m v)
  (setf (aref arr n m) v))
(defun traverse (mirror x y angle)
  (if (or (or (> x 3) (< x 1)))))
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
