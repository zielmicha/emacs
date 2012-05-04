
;;;
;;; notes-xemacs.el
;;; $Id: notes-xemacs.el,v 1.2 1998/11/04 18:40:27 johnh Exp $
;;;
;;; Copyright (C) 1998 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License.
;;;

;;
;; Xemacs-specific parts of notes-mode.
;;

(defun notes-platform-bind-mouse (map generic-key fn)
  "Map from (FSF) emacs symbols to xemacs for notes-mode.  (Sigh.)"
  (let
      ((xemacs-key
	(cond
	 ((eq generic-key 'mouse-2) [(button2)])
	 ((eq generic-key 'S-mouse-2) [(shift button2)]))))
    (define-key map xemacs-key fn)))


(defun notes-platform-font-lock ()
  "Notes platform-specific font-lock mode."
  (require 'font-lock)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(notes-font-lock-keywords nil)))


(defun notes-platform-init ()
  "Init platform-specific stuff for notes-mode."
  (if notes-platform-inited
      t
    (setq notes-platform-inited t)
    (make-face notes-bold-face)
    (if (not (face-differs-from-default-p notes-bold-face))
	    (copy-face 'bold notes-bold-face))

    ))

(provide 'notes-xemacs)
