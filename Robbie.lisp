(defvar rooms)
(defvar loc)
(defvar upstairs-room)
(defvar stairs)

;;; Table defining rooms
(setf rooms

      '((living-room        (north front-stairs)
                            (south dining-room)
                            (east kitchen))

	(upstairs-bedroom   (west library)
                            (south front-stairs))

	(dining-room        (north living-room)
                            (east pantry)
                            (west downstairs-bedroom))

	(kitchen            (west living-room)
                            (south pantry))

	(pantry             (north kitchen)
                            (west dining-room))

	(downstairs-bedroom (north back-stairs)
                            (east dining-room))

	(back-stairs        (south downstairs-bedroom)
                            (north library))

	(front-stairs       (north upstairs-bedroom)
                            (south living-room))

	(library            (east upstairs-bedroom)
                            (south back-stairs))))

(setf upstairs-room '(library upstairs-bedroom))

(setf stairs '(front-stairs back-stairs))

(defun choices (room)
  "Returns the table of permissible
directions Robbie may take."
  (rest (assoc room rooms)))

(defun look (direction room)
  "Returns where Robbie would end
up if he moved in that DIRECTION from that ROOM."
  (second (assoc direction (choices room))))

(setf loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting
the variable LOC."
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (room)
  "Returns T if its input is an
upstairs loccation."
  (if (member room upstairs-room) t nil))

(defun onstairsp (l)
  (if (member l stairs) t nil))

(defun where ()
  "Returns where is Robbie now."
  (if (onstairsp loc)
      (list 'robbie 'is 'on 'the loc)
      (list 'robbie 'is (if (upstairsp loc) 'upstairs 'downstairs) 'in 'the loc)))

(defun move (direction)
  "Move Robbie in DIRECTION."
  (let ((new-loc (look direction loc)))
    (cond ((null new-loc) '(ouch! robbie hit a wall.))
	  (t (set-robbie-location new-loc) (where)))))
