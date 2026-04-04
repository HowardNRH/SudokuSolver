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


; posistion is from 0-80, left to right, top to bottom
(defun possibleVals (puzzle pos)
  (if (/= (nth pos puzzle) 0)
    nil
      (let* ((rows (makeRows puzzle))
             (cols (makeCols rows))
             (grids (makeGrids rows))

             (r (floor pos 9))
             (c (mod pos 9))
             (g (+ (* (floor r 3) 3)
                   (floor c 3)))

             (used (remove 0 (remove-duplicates (append (nth r rows) (nth c cols) (nth g grids))))))

        (set-difference '(1 2 3 4 5 6 7 8 9) used))))



(defun validList (list)
  (let ((nums (remove 0 list)))
    (= (length nums)
       (length (remove-duplicates nums)))))

(defun validPuzzle (puzzle)
  (let* ((rows (makeRows puzzle))
         (cols (makeCols rows))
         (grids (makeGrids rows)))

    (and
      (every #'validList rows)
      (every #'validList cols)
      (every #'validList grids))))


(defun findEmpty (puzzle)
  (position 0 puzzle))

(defun setVal (puzzle pos value)
  (let ((new (copy-list puzzle)))
    (setf (nth pos new) value)
    new))



(defun solver (puzzle)

  (unless (validPuzzle puzzle)
    (return-from solver nil))

  (let ((pos (findEmpty puzzle)))

    (if (null pos)
        puzzle

        (dolist (val (possibleVals puzzle pos))
          (let ((sol (solver (setVal puzzle pos val))))
            
            (when sol
              (return sol)))))))


(format t "~A~%" (solver *puzzle*))
