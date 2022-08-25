(defun take-input-as-list () (loop for i from 1 to (read)
                                        collect (read)))
(defun print-list-newline (list) (loop for x in list do (format t "~a~%" x)))
(print-list-newline (reverse (take-input-as-list)))
