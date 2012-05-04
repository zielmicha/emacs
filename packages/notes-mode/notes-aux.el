
;;;
;;; notes-aux.el
;;; auxiliary functions for notes-mode and friends
;;; $Id: notes-aux.el,v 1.10 2000/03/24 21:36:33 johnh Exp $
;;;
;;; Copyright (C) 1994,1995,1998 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License, version 2.
;;;


;;;
;;; generic-{beginning,end}-of-defun
;;; I use in tex-mode and notes-mode
;;;

;;;###autoload
(defun generic-beginning-of-defun (regexp)
  "* Go to the beginning of defun identified by REGEXP."
  (re-search-backward regexp 0 'to-limit)
)

;;;###autoload
(defun generic-end-of-defun (regexp)
  "* Go to the end of defun identified by REGEXP."
  (let
      ((restore-point (point)))
    (if (looking-at regexp)
	(goto-char (match-end 0)))
    ;; find next section and leave cursor at section beginning
    (if (re-search-forward regexp (point-max) 'to-limit)
	(re-search-backward regexp 0 t)
      ;(goto-char restore-point)
      ))
)


;;;###autoload
(defun match-substring (string count &optional default empty-default)
  "Given STRING, return the COUNT-th element from the last match.
Returns DEFAULT if there is no such match,
or if the match is empty and EMPTY-DEFAULT is non-nil."
  (if (and (match-beginning count)
	   (or (not empty-default)
	       (> (match-end count) (match-beginning count))))
      (substring string (match-beginning count) (match-end count))
    default))


;;;
;;; get-{beginning,end}-of-line
;;; Simple functions for a simple world.
;;;

;;;###autoload
(defun get-beginning-of-line ()
  "Return the boln as a position."
  (save-excursion
    (beginning-of-line)
    (point)))

;;;###autoload
(defun get-end-of-line ()
  "Return the boln as a position."
  (save-excursion
    (end-of-line)
    (point)))


;;;###autoload
;(defun notes-format-date (&optional calendar-date)
;  "Format the calendar-date-style DATE up to be a notes-format date.
;If no DATE is specified, use today's date."
;  (require 'calendar)
;  (let* ((date (if calendar-date
;		   calendar-date
;		 (calendar-current-date)))
;	 (month (car date))
;	 (day (nth 1 date))
;	 (year (nth 2 date)))
;    (format "%02d%02d%02d" (- year 1900) month day)))
(defun notes-format-date (&optional time)
  "Format the TIME up to be a notes-format date.
If no TIME is specified, use today's date."
  (require 'notes-variables)
  (if (null time)
      (setq time (current-time)))
  (format-time-string notes-file-form time))

(defun notes-file-to-epoch (file)
  "* Convert a notes FILE to an epoch time."
  (string-match notes-file-regexp file)
  (let
      ((y (string-to-int (substring file (match-beginning 1) (match-end 1))))
       (m (string-to-int (substring file (match-beginning 2) (match-end 2))))
       (d (string-to-int (substring file (match-beginning 3) (match-end 3)))))
    (if (< y 1900)
	(setq y (+ y 1900)))
    (if (< y 1970)
	(setq y (+ y 100)))
    (encode-time 0 0 12 d m y)))

(defun notes-file-to-url (file &optional tag)
  "* Convert a notes FILE to a URL with an optional TAG."
  (let
      ((epoch (notes-file-to-epoch file)))
    (concat
     notes-url-prefix
     (format-time-string notes-int-form epoch)
     "/"
     (format-time-string notes-file-form epoch)
     (if tag "#* " "")
     tag)))

(provide 'notes-aux)
