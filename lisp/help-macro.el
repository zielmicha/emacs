;;; help-screen.el --- Makes command line help such as help-for-help

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Lynn Slater <lrs@indetech.com>
;; Created: : Mon Oct  1 11:42:39 1990
;; Adapted-By: ESR
;; Last Modified By: Lynn Slater x2048
;; Last Modified On: Mon Sep 23 14:40:19 1991

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; 	This file supplies the macro make-help-screen which constructs
;; single character dispatching with browsable help such as that provided
;; by help-for-help. This can be used to make many modes easier to use; for
;; example, the Gnu Emacs Empire Tool uses this for every "nested" mode map
;; called from the main mode map.

;;-> ***********************  Example of use *********************************

;;->(make-help-screen help-for-empire-redistribute-map
;;->		  "c:civ m:mil p:population f:food ?"
;;->		  "You have discovered the GEET redistribution commands
;;->   From here, you can use the following options:
;;->
;;->c	Redistribute civs from overfull sectors into connected underfull ones
;;->	  The functions typically named by empire-ideal-civ-fcn control
;;->          based in part on empire-sector-civ-threshold  
;;->m	Redistribute military using levels given by empire-ideal-mil-fcn
;;->p	Redistribute excess population to highways for max pop growth
;;->	  Excess is any sector so full babies will not be born.
;;->f	Even out food on highways to highway min and leave levels
;;->	  This is good to pump max food to all warehouses/dist pts
;;->
;;->
;;->Use \\[help-for-empire-redistribute-map] for help on redistribution.
;;->Use \\[help-for-empire-extract-map] for help on data extraction.
;;->Please use \\[describe-key] to find out more about any of the other keys."
;;->		  empire-shell-redistribute-map)

;;->  (define-key c-mp "\C-h" 'help-for-empire-redistribute-map)
;;->  (define-key c-mp help-character 'help-for-empire-redistribute-map)

;;; Change Log:
;;
;; 22-Jan-1991		Lynn Slater x2048	
;;    Last Modified: Mon Oct  1 11:43:52 1990 #3 (Lynn Slater)
;;    documented better

;;; Code:

(provide 'help-screen)
(require 'backquote)

(defmacro make-help-screen (fname help-line help-text helped-map)
  "Constructs function FNAME that when invoked shows HELP-LINE and if a help
character is requested, shows HELP-TEXT. The user is prompted for a character
from the HELPED-MAP and the corresponding interactive function is executed."
  (` (defun (, fname) ()
	   (, help-text)
	   (interactive)
	   (let ((line-prompt
		  (substitute-command-keys (, help-line))))
	     (message line-prompt)
	     (let ((char (read-char)))
	       (if (or (= char ??) (= char help-char))
		   (save-window-excursion
		     (switch-to-buffer-other-window "*Help*")
		     (erase-buffer)
		     (insert (documentation (quote (, fname))))
		     (goto-char (point-min))
		     (while (memq char (cons help-char '(?? ?\C-v ?\ ?\177 ?\M-v)))
		       (if (memq char '(?\C-v ?\ ))
			   (scroll-up))
		       (if (memq char '(?\177 ?\M-v))
			   (scroll-down))
		       (message "%s%s: "
				line-prompt
				(if (pos-visible-in-window-p (point-max))
				    "" " or Space to scroll"))
		       (let ((cursor-in-echo-area t))
			 (setq char (read-char))))))
	       (let ((defn (cdr (assq (downcase char) (, helped-map)))))
		 (if defn
		     (if (keymapp defn)
			 (error "sorry, this command cannot be run from the help screen. Start over.")
		       (call-interactively defn))
		   (ding))))))
     ))

;;; help-screen.el

