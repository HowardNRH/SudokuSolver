(defparameter *puzzle* 
 '(5 0 7 6 0 0 0 8 9
   6 0 0 2 0 7 4 0 0
   9 0 0 3 5 0 0 0 6
   7 6 3 4 0 2 0 0 0
   0 8 4 0 0 0 6 3 7
   0 0 0 7 0 6 8 4 0
   0 7 1 0 2 4 0 0 0
   0 0 0 0 7 0 9 0 8
   0 2 0 0 0 5 1 7 0)
)

(defun findEmpty (puzzle)
  (position 0 puzzle))

(defun setVal (puzzle pos value)
  (let ((new (copy-list puzzle)))
    (setf (nth pos new) value) new))

(defun fancyPrint (puzzle)
  (loop for i from 0 below 81 do
    (format t "~a " (nth i puzzle))
    (when (= (mod (1+ i) 9) 0)
      (format t "~%"))))

(format t "Puzzle: ~A~%" *puzzle*)
(fancyPrint *puzzle*)
