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


(defun makeRows (puzzle)
  (loop for i from 0 below 81 by 9
    collect (subseq puzzle i (+ i 9))))

(defun makeCols (rows)
  (loop for c from 0 below 9
    collect
    (loop for r from 0 below 9
      collect (nth c (nth r rows)))))

(defun makeGrids (rows)
  (loop for gr from 0 below 3 append
    (loop for gc from 0 below 3 collect
      (loop for r from (* gr 3) below (+ (* gr 3) 3) append
        (loop for c from (* gc 3) below (+ (* gc 3) 3)
          collect (nth c (nth r rows)))))))

(defun update (puzzle)
  (let* ((rows (makeRows puzzle))
         (cols (makeCols rows))
         (grids (makeGrids rows)))
    (list rows cols grids)))

(format t "Rows: ~A~%" (first (update *puzzle*)))
(format t "~%Cols: ~A~%" (second (update *puzzle*)))
(format t "~%Grids: ~A~%" (third (update *puzzle*)))

;rows is (first (update *puzzle*))
;cols is (second (update *puzzle*))
;grids is (third (update *puzzle*))
