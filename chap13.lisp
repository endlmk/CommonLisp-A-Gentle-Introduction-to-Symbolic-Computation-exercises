(defun subprop (symbol item prop)
  (setf (get symbol prop) (remove item (get symbol prop))))

(defun forge-meeting (p1 p2)
  (subprop p1 p2 'has-met)
  (subprop p2 p1 'has-met))

(defun my-get (key prop)
  (second (member prop (symbol-plist key))))

(defun hasprop (symbol prop)
  (not (null (find prop (symbol-plist symbol)))))

(setf '*hist-array* (make-array n))
(setf '*total-points* 0)

(defun new-histogram (n)
  (setf *hist-array* (make-array n))
  (setf *total-points* 0))

(defun record-value (n)
  (cond ((or (< n 0) (>= n (length *hist-array*))) (format t "~&Invalid Value."))
	(t (incf (aref *hist-array* n)) (incf *total-points*))))

(defun print-hist-line (n)
  (format t "~2D [~3D] " n (aref *hist-array* n))
  (dotimes (i (aref *hist-array* n))
    (format t "*"))
  (format t "~%"))

(defun print-histogram ()
  (dotimes (i 200)
    (record-value (random (length *hist-array*))))
  (dotimes (i (length *hist-array*))
    (print-hist-line i)))

(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
	"enlpo pib slafml pvv bfwkj"))

(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))


(defun make-substitution (f s)
  (setf (gethash f *decipher-table*) s)
  (setf (gethash s *encipher-table*) f))

(defun undo-substitution (f s)
  (setf (gethash f *decipher-table*) nil)
  (setf (gethash s *encipher-table*) nil))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (enc)
  (let ((w (make-string (length enc) :initial-element #\Space)))
    (dotimes (i (length enc) w)
      (if (gethash (aref enc i) *decipher-table*)
	  (setf (aref w i) (gethash (aref enc i) *decipher-table*))))))

(defun show-line (enc)
  (format t "~%~A~%~A~%" enc (decipher-string enc)))

(defun show-text (x)
  (dolist (src x)
    (show-line src)))

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))

(defun read-letter (s)
  (cond ((equal s 'end) 'end)
	((equal s 'undo) 'undo)
	(t (get-first-char s))))

(defun sub-letter (c)
  (when (gethash c *decipher-table*)
    (format t "~A has been deciphered already.~%" c)
    (return-from sub-letter nil))
  (format t "What dose decipher to?~%")
  (let ((d (read-letter (read))))
    (when (gethash d *encipher-table*)
      (format t "~A has been enciphered already.~%" d)
      (return-from sub-letter nil))
    (make-substitution c d)))

(defun undo-letter ()
  (format t "Undo which letter?")
  (let ((u (read-letter (read))))
    (cond ((not (characterp u)) (format t "~&Invalid Input."))
	  ((gethash u *decipher-table*) (undo-substitution u (gethash u *decipher-table*)))
	  (t (format t "~A wasn't dechiphered!" u)))))
  

(defun solve (ct)
  (do ((resp nil))
      ((equal resp 'end))
    (show-text ct)
    (format t "Substitute which letter?")
    (setf resp (read-letter (read)))
    (cond ((characterp resp) (sub-letter resp))
	  ((equal resp 'undo) (undo-letter))
	  ((equal resp 'end) nil)
	  (t (format t "~&Invalid Input.")))))
