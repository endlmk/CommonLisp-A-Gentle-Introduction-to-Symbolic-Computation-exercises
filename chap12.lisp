(defstruct node
  (name)
  (question)
  (yes-case)
  (no-case))

(setf *node-list* nil)

(defun init ()
  (setf *node-list* nil)) 

(defun add-node (name question yes no)
  (let ((node (make-node :name name
                         :question question
                         :yes-case yes
                         :no-case no)))
    (push node *node-list*)
    name))

(defun find-node (name)
  (find-if #'(lambda (n) (equal (node-name n) name)) *node-list*))

(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)

(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)
(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")
(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)
(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)
(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)

(defun process-node (name)
  (let ((n (find-node name)))
    (cond ((null n) (format t "The node hasn't been defined.") nil)
	  (t (if (y-or-n-p "~&~A" (node-question n))
		 (node-yes-case n)
		 (node-no-case n))))))

(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((or (stringp current-node) (null current-node)) (format t "~&~A" current-node))))

(defun interactive-add-node ()
  (let* ((name (prompt-for "~&Input name"))
	 (question (prompt-for "~&Input question"))
	 (y (prompt-for "Input yes case"))
	 (n (prompt-for "Input no case")))
    (push (make-node :name name
		     :question question
		     :yes-case y
		     :no-case n)
	  *node-list*)))

(defun prompt-for (str)
  (format t "~A" str)
  (read))

(defstruct (captain
	    (:print-function print-captain))
  (name nil)
  (age nil)
  (ship nil))

(defun print-captain (x stream depth)
  (format stream "<#CAPTAIN ~A>"
	  (captain-name x)))
