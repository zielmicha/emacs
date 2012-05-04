
;;;
;;; notes-bootstrap.el
;;; $Id: notes-bootstrap.el,v 1.2 2000/06/03 17:23:29 johnh Exp $
;;;
;;; Copyright (C) 1997 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License.
;;;

(defun notes-load-path ()
  "I couldn't get emacs -e '(princ load-path)' to work."
  (interactive)
  (mapcar
   (function
    (lambda (a)
      (princ a)
      (princ "\n")
      ))
   load-path)
)

