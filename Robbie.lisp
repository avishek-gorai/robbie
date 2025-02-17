(defvar rooms "Map of the rooms.")
(defvar location "Current location of Robbie.")
(defvar upstairs-room "List of rooms that are upstairs.")
(defvar stairs "List of stairs.")

;;; Table defining rooms
(setf rooms (quote
             ((living-room        (north front-stairs)
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
                                  (south back-stairs)))))

(setf upstairs-room (quote (library upstairs-bedroom)))

(setf stairs (quote (front-stairs back-stairs)))

(setf location (quote pantry))

(defun choices (room)
  "Returns the table of permissible directions Robbie may take."
  (rest (assoc room rooms)))

(defun look (direction room)
  "Returns where Robbie would end up if he moved in that DIRECTION from that ROOM."
  (second (assoc direction (choices room))))

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOCATION."
  (setf location place))

(defun how-many-choices ()
  "Returns the possible places that Robbie can go."
  (length (choices location)))

(defun upstairsp (room)
  "Returns T if its input is an upstairs loccation."
  (if (member room upstairs-room) t nil))

(defun onstairsp (location)
  "Returns T is if Robbie is on the stairs."
  (if (member location stairs) t nil))

(defun where ()
  "Returns Robbie's current location."
  (if (onstairsp location)
      (format t "Robbie is on the ~A.~&" location)
      (format t "Robbie is ~A in the ~A.~&"
              (if (upstairsp location) (quote upstairs) (quote downstairs)) location)))

(defun move (direction)
  "Move Robbie in the given DIRECTION."
  (let ((new-location (look direction location)))
    (cond ((null new-location) (format t "Ouch! Robbie hit a wall.~&"))
	  (t (set-robbie-location new-location) (where)))))
