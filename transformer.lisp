(defparameter *puzzle* 
  (list)
)

(defun filePrint (filename)
  (with-open-file (in filename)
    (when in 
      (loop for char = (read-char in nil)
        while char 
        for digit = (digit-char-p char)
        when digit
        do (setf *puzzle* (append *puzzle* (list digit))))
    )))

(filePrint "easyPreset.txt")
(format t "~A~%" *puzzle*)
