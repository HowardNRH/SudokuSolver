; Nick Howard
; April 4, 2025
; CS2100 Project
; Common Lisp Sudoku SOlver


; set up puzzle
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




; list into 3 other lists, rows, cols, and grids
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




; possible values from a specific posistion
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





; validators
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



; helper functions for solver
(defun findEmpty (puzzle)
  (position 0 puzzle))

(defun setVal (puzzle pos value)
  (let ((new (copy-list puzzle)))
    (setf (nth pos new) value)
    new))




; the man, the myth, the legend 
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



; fancily print puzzle
(defun fancyPrint (puzzle)
  (loop for i from 0 below 81 do
    (format t "~a " (nth i puzzle))
    (when (= (mod (1+ i) 9) 0)
      (format t "~%"))))




(format t "~%Original Puzzle:~%")
(fancyPrint *puzzle*)
(format t "~%Solved Puzzle:~%")
(fancyPrint (solver *puzzle*))
