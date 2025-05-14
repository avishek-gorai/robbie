;;; robbie.lisp -- Main program.
;;; Copyright (C) 2025 Avishek Gorai <avishekgorai@myyahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package robbie)


(defvar *rooms* (quote
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
                                  (south back-stairs))))
  "Map of the rooms.")

(defvar upstairs-room (quote (library upstairs-bedroom)) "List of rooms that are upstairs.")

(defvar stairs (quote (front-stairs back-stairs)) "List of stairs.")

(defvar location (quote pantry)  "Current location of Robbie.")


(defun choices (room)
  "Returns the table of permissible directions Robbie may take."
  (rest (assoc room *rooms*)))


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
