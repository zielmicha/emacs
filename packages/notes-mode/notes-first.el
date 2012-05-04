
;;;
;;; notes-first.el
;;; $Id: notes-first.el,v 1.5 2006/01/14 23:11:08 johnh Exp $
;;;
;;; Copyright (C) 2000-2006 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License, version 2.
;;;

;; This whole perl5 thing is because rms insists
;; that the USER specify where perl is, not that configure
;; do that at emacs build time.  Grumble.
(defvar notes-first-perl5-binary "perl"
  "Location of the perl binary to invoke notesinit.  (Must be perl v5.)")

;;;
(defun notes-first-use-init ()
  "Set up notes mode for the first time for a new user."
  ;; note that we CAN'T assume the contents of notes-variables is loaded.
  (if (y-or-n-p "Setup notes-mode with defaults ")
      (notes-first-run-notes-init)
    (error (concat "Please run " notes-utility-dir "/notesinit by hand in a shell to customize defaults."))))

;; xxx: eventually we might do something more sophisticated here
;; (like asking the user questions directly).
(defun notes-first-run-notes-init ()
  "Run notesinit with defaults"
  (let*
      ((notes-init-cmd (concat notes-utility-dir "/notesinit")))
    (message (concat "Running \"" notes-first-perl5-binary notes-init-cmd "\" to set up notes-mode."))
    (call-process notes-first-perl5-binary nil nil nil notes-init-cmd "-D"))
  ;; ok, things are setup, but we want to lead the user to what to do next
  ;; => start up on today's note
  (message "Notes are now set up.  Run M-x notes-index-todays-link to start."))

(provide 'notes-first)

