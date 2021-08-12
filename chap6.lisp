(defun right-side (l)
  (rest (member '-vs- l)))

(defun left-side (l)
  (reverse (rest (member '-vs- (reverse l)))))

(defun count-common (l r)
  (length (intersection l r)))

(defun compare (x)
  (let ((l (left-side x))
	(r (right-side x)))
    (cons (count-common l r) '(common features))))

(setf nerd-states
      '((sleeping eating)
	(eating waiting-for-a-computer)
	(waiting-for-a-computer programming)
	(programming debugging)
	(debugging sleeping)))

(defun nerdus (state)
  (second (assoc state nerd-states)))

(defun sleepless-nerdus (state)
  (let ((after (nerdus state)))
    (cond ((equal after 'sleeping) (nerdus 'sleeping))
	  (t after))))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(setf rooms
      '((living-room (north front-stairs) (south dining-room) (east kitchen))
	(upstairs-bedroom (west library) (south front-stairs))
        (dining-room (north living-room) (east pantry) (west downstairs-bedroom))
	(kitchen (west living-room) (south pantry))
	(pantry (north kitchen) (west dining-room))
	(downstairs-bedroom (north back-stairs) (east dining-room))
	(back-stairs (south downstairs-bedroom) (north library))
	(front-stairs (north upstairs-bedroom) (south living-room))
	(library (east upstairs-bedroom) (south back-stairs))))

(defun choices (place)
  (rest (assoc place rooms)))

(defun look (direction place)
  (second (assoc direction (choices place))))

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC."
  (setf loc place))

(defun how-many-choices ()
  (length (choice loc)))

(defun upstairsp (place)
  (or (equal place 'library) (equal place 'bedroom)))

(defun onstairsp (place)
  (or (equal place 'flont-stairs) (equal place 'back-stairs)))

(defun where ()
  (cond ((onstairsp loc) (append '(Robbie is on the) (list loc)))
	((upstairsp loc) (append '(Robbie is upstairs in the) (list loc)))
	(t (append '(Robbie is downstairs in the) (list loc)))))

(defun move (direction)
  (let ((next (look direction loc)))
    (cond (next (set-robbie-location next) (where))
	  (t '(ouch! Robbie hit a wall)))))
	      
