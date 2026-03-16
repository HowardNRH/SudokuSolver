(defun filePrint (filename)
  (with-open-file (in filename)
    (when in 
      (loop for char = (read-char in nil)
        while char 
        do (format t "~c" char))
      (format t "~%")
    )))

(filePrint "easyPreset.txt")
