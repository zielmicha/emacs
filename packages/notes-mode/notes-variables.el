
;;;
;;; notes-variables.el
;;; $Id: notes-variables.el,v 1.30 2007/11/06 02:45:55 johnh Exp $
;;;
;;; Copyright (C) 1994-2000 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License, version 2.
;;;

;;
;; This file lists all parameters you might wish to change in
;; notes{-index,}-mode.  The best way to handle this in your
;; .emacs file is to do
;;	(require 'notes-variables)
;;	(setq your-variable-to-change 'your-new value)
;;

;; xxx: if part of emacs, this should be probably be set to exec-directory (?)
(defvar notes-utility-dir "/home/johnh/NOTES/BIN"
  "Location of notes utility programs")

;;
;; Notice:  several notes parameters are defined in your
;; ~/.notesrc file.  These are not specified here.
;; See mkconfig for details.
;; We fetch them here.
;;
;; To make this fast, we cache the configuration in a .notesrc.el
;; file.  We only have to invoke mkconfig when that file is out-of-date.
;; This optimization is very important because notes-variables is 
;; required every time emacs is started.
;;
(save-excursion
  (if (null (file-exists-p (concat notes-utility-dir "/mkconfig")))
      (progn
	;;
	;; A common user error is that people don't
	;; follow the installation instructions.
	;; Part of installation is chaning my local paths (with
	;; johnh in them) to whatever you use on your system.
	;; If the following error is triggered, it's probably
	;; because the user didn't RTFM (or even TF README)
	;; and just tried to run notes-mode in place.
	;; DON'T DO THAT!  Follow the installation instructions.
	;;
	(error "notes-mode is incorrectly installed.  Consult the INSTALL section of README.")))
  (let*
      ((source-file (expand-file-name "~/.notesrc"))
       (cache-file (expand-file-name "~/.notesrc.el"))
       (cache-buf (set-buffer (find-file-noselect cache-file))))
    (if (and 
	 (not (file-exists-p source-file))
	 (not noninteractive))
	(progn
	  (require 'notes-first)
	  (notes-first-use-init)))
    (if (and  ; requirements for a valid cache-file
	 (file-exists-p cache-file)
	 (if (file-exists-p source-file)
	     (file-newer-than-file-p cache-file source-file)
	   t)
	 (file-newer-than-file-p cache-file (concat notes-utility-dir "/mkconfig")))
	t ; cache is up-to-date
      ;; otherwise, refresh the cache
      (erase-buffer)
      (call-process (concat notes-utility-dir "/mkconfig") nil t nil "elisp")
      (save-buffer cache-buf)
      (set-file-modes cache-file 420)) ; protect it => mode 0644
    (eval-current-buffer)
    (kill-buffer cache-buf)))


(setq auto-mode-alist
      (cons (cons
	     (concat notes-int-glob "/" notes-file-glob ".?$")
	     'notes-mode)
	    auto-mode-alist))

;;; xxx: most of these should use defcustom or something similar, I presume.
(defvar notes-w3-alternate-url 'browse-url
  "* A function to call when notes-w3-url cannot handle a complex URL.
It now goes through the emacs browse-url package,
but you could also set it manually (say, to w3-fetch).")

(defvar notes-use-font-lock t
  "* Enable notes fontification.")

(defvar notes-use-outline-mode t
  "* Enable outline-minor-mode in all notes buffers?")

(defvar notes-index-fontify-dates nil
  "* Fontify dates in notes-index-mode.
Turning this off for large notes-index's can improve performance.")

(defvar notes-bold-face 'notes-bold-face
  "* Face to use for notes-index-mode and notes-mode subjects.
The default face is copied from 'bold.")

(defvar notes-font-lock-keywords
  '(("^\\* .*$" . notes-bold-face)
    ("^\\-+$" . notes-bold-face)
    ;; ("^[0-9]+\\-[A-Za-z]+\\-[0-9]+ [A-Za-z]+$" . font-lock-bold-face)
    ;; NEEDSWORK:  should also highlight URLs, maybe?
   )
  "* Font-lock keywords for notes mode.")

(defvar notes-index-font-lock-keywords
  '(("^[^:]*:" . notes-bold-face)
    ("\\<[0-9]*\\>" . mouse-face)
   )
  "* Font-lock keywords for notes-index mode.")

(defvar notes-mode-complete-subjects t
  "* Enable subject completion in notes mode?")

(defvar notes-w3-follow-link-mouse-other-window t
  "* Should notes-w3-follow-link-mouse open another window?")

(defvar notes-subject-table nil
  "List of notes-subjects needed for subject completion.
Reloaded by loading the notes-index file.")

(defvar notes-mode-initialization-program "mknew"
  "Program to run to initialize a new notes file.  Must be in notes-bin-dir.
If nil, no initialization is done.")

(defvar notes-encryption-key-id nil
  "Keyid of PGP key for the current user.
Useful if your \\[user-full-name] doesn't match a unique key.
Should have a leading 0x.")

(defvar notes-electric-prevnext 2
  "Amount of electricity in prevnext for notes-mode.
nil: don't auto-update anything.
1: update prevnext, but don't autosave the old buffer
2: update prevnext and autosave the old buffer.")

(defvar notes-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "*In XEmacs or Lucid Emacs?.")

;;;
;;; prep the load path using the notes-lisp-dir
;;; code stolen from the auctex styles files (specifically tex-site.el)
;;;		-- Kannan
;;;		   Wed Apr  7 09:40:27 EDT 1999
;;;
(if (boundp 'notes-lisp-dir)
    (or (assoc notes-lisp-dir (mapcar 'list load-path)) ;No `member' yet.
	(assoc (substring notes-lisp-dir 0 -1) ;Without trailing slash.
	       (mapcar 'list load-path))
	(setq load-path (cons notes-lisp-dir load-path))))

(if notes-running-xemacs
    (require 'notes-xemacs)
  (require 'notes-emacs))

(defvar notes-platform-inited nil
  "Have we inited our platform (xemacs/emacs)?")

;;;
;;; autoloads
;;;


;;;### (autoloads (notes-index-mode) "notes-index-mode" "notes-index-mode.el" (12248 45843))
;;; Generated autoloads from notes-index-mode.el

(autoload (quote notes-index-mode) "notes-index-mode" "\
Notes-index-mode with mouse support.

You may wish to change notes-bold-face and notes-use-font-lock.

Key bindings are:
\\{notes-index-mode-map}" t nil)

;;;###autoload
(autoload (quote notes-index-todays-link) "notes-index-mode" "\
* Open the notes file for today." t nil)

;;;***

;;;### (autoloads (notes-w3-follow-link-mouse notes-w3-follow-link notes-w3-file) "notes-url" "notes-url.el" (12248 46828))
;;; Generated autoloads from notes-url.el

(autoload (quote notes-w3-url) "notes-url" "\
Find a link to an ftp site - simple transformation to ange-ftp format.
Takes the URL as an argument.  Optionally you specify
WHERE the information should appear (either 'otherwindow or not)." nil nil)

(autoload (quote notes-w3-follow-link) "notes-url" "\
* Follow the URL at the point.
NEEDSWORK:  should handle (by ignoring) an optional \"URL:\" tag." t nil)

(autoload (quote notes-w3-follow-link-mouse) "notes-url" "\
* Follow the URL where the mouse is." t nil)

;;;***


;;;### (autoloads (notes-underline-line notes-end-of-defun notes-beginning-of-defun) "notes-mode" "notes-mode.el" (12250 9363))
;;; Generated autoloads from notes-mode.el

(autoload (quote notes-underline-line) "notes-mode" "\
*Create a row of dashes as long as this line, or adjust the current underline." t nil)

;;;***

(autoload 'notes-mode "notes-mode" "autoloaded notes-mode" t nil)

(run-hooks 'notes-variables-load-hooks)
(provide 'notes-variables)
