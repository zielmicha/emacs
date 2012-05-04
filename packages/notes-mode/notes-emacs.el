
;;;
;;; notes-emacs.el
;;; $Id: notes-emacs.el,v 1.3 2007/11/06 02:41:21 johnh Exp $
;;;
;;; Copyright (C) 1998 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License.
;;;

;;
;; (FSF) emacs-specific parts of notes-mode.
;;

(defun notes-platform-bind-mouse (map generic-key fn)
  "Map emacs symbols (a no-op)."
  (define-key map (vconcat (list generic-key)) fn))


(defun notes-platform-font-lock (new-font-lock-keywords)
  "Notes platform-specific font-lock mode."
  (require 'font-lock)
  (if (>= emacs-major-version 20)
      (progn
	;; emacs-20
	(make-local-variable 'font-lock-defaults)
	(setq font-lock-defaults
	      (list
	       new-font-lock-keywords
	       t
	       nil
	       nil
	       'beginning-of-line)))
    ;; emacs-19
    (make-local-variable 'font-lock-no-comments)
    (setq font-lock-no-comments t)
    (make-local-variable 'font-lock-keywords)
    (setq font-lock-keywords new-font-lock-keywords)
    (font-lock-mode 1)))


(defun notes-platform-init ()
  "Init platform-specific stuff for notes-mode."
  (if notes-platform-inited
      t
    (setq notes-platform-inited t)
    (if (eq notes-bold-face 'notes-bold-face)
	(copy-face 'bold notes-bold-face))))

(provide 'notes-emacs)



