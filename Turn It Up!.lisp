(defun take-input-as-list (start fn) (loop for i from start to (read)
                                        collect (funcall fn)))

(defun print-list-newline (list) (loop for x in list do (format t "~a~%" x)))

(defun run-input (counter)
  (loop for x in (take-input-as-list 1 #'read-line)
        do (if (string= x "Skru op!")
               (if (/= counter 10) (incf counter))  
               (if (/= counter 0) (decf counter))))
  (format t "~a" counter))
(run-input 7)
