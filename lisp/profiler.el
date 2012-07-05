;;; profiler.el --- GNU Emacs profiler -*- lexical-binding: t -*-

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))

(defgroup profiler nil
  "GNU Emacs profiler."
  :group 'lisp
  :prefix "profiler-")



;;; Utilities

(defun profiler-ensure-string (object)
  (if (stringp object)
      object
    (format "%s" object)))

(defun profiler-format (fmt &rest args)
  (loop for (width align subfmt) in fmt
	for arg in args
	for str = (typecase subfmt
		    (cons   (apply 'profiler-format subfmt arg))
		    (string (format subfmt arg))
		    (t	    (profiler-ensure-string arg)))
	for len = (length str)
	if (< width len)
	collect (substring str 0 width) into frags
	else
	collect
	(let ((padding (make-string (- width len) ? )))
	  (ecase align
	    (left (concat str padding))
	    (right (concat padding str))))
	into frags
	finally return (apply #'concat frags)))

(defun profiler-header-line-format (fmt &rest args)
  (let* ((header (apply 'profiler-format fmt args))
	 (escaped (replace-regexp-in-string "%" "%%" header)))
    (concat " " escaped)))

(defun profiler-title (mode)
  (ecase mode
    (sample "Sampling profiler")
    (trace  "Tracing profiler")
    (memory "Memory Profiler")))



;;; Snapshot Data Structure

(defstruct (profiler-slot (:type list)
			  (:constructor nil))
  backtrace count)

(macrolet
    ((def (name)
	  `(defsubst ,(intern (format "profiler-snapshot-%s" name)) (snapshot)
	     (plist-get snapshot ',name))))
  (def mode)
  (def start-time)
  (def stop-time)
  (def slots)
  (def sample-interval)
  (def sample-count))



;;; Tree Data Structure

(defstruct (profiler-tree (:constructor nil))
  entry parent $depth children)

(defun profiler-tree-leaf-p (tree)
  (null (profiler-tree-children tree)))

(defun profiler-tree-depth (tree)
  (let ((parent (profiler-tree-parent tree)))
    (if (not parent)
	0
      (or (profiler-tree-$depth tree)
	  (setf (profiler-tree-$depth tree)
		(1+ (profiler-tree-depth parent)))))))

(defun profiler-tree-find-child (tree entry)
  (dolist (child (profiler-tree-children tree))
    (when (eq (profiler-tree-entry child) entry)
      (return child))))

;;; Backtrace Tree

(defstruct (profiler-backtrace-tree
	    (:include profiler-tree)
	    (:constructor profiler-make-backtrace-tree))
  (time 0) time-percent)

(defun profiler-backtrace-tree< (a b)
  (cond ((eq a t) t)
	((eq b t) nil)
	(t (< (profiler-backtrace-tree-time a)
	      (profiler-backtrace-tree-time b)))))

(defun profiler-backtrace-tree> (a b)
  (not (profiler-backtrace-tree< a b)))

(defun profiler-build-backtrace-tree (snapshot)
  "Construct a backtrace tree of SNAPSHOT."
  (let ((sample-interval (profiler-snapshot-sample-interval snapshot))
	(sample-count (profiler-snapshot-sample-count snapshot))
	(slots (profiler-snapshot-slots snapshot))
	(tree (profiler-make-backtrace-tree)))
    (dolist (slot slots)
      (let* ((backtrace (profiler-slot-backtrace slot))
	     (count (profiler-slot-count slot))
	     (time (* count sample-interval))
	     (node tree))
	(dolist (entry backtrace)
	  (let ((child (profiler-tree-find-child node entry)))
	    (unless child
	      (setq child (profiler-make-backtrace-tree :parent node
							:entry entry))
	      (push child (profiler-tree-children node)))
	    (incf (profiler-backtrace-tree-time child) time)
	    (setf (profiler-backtrace-tree-time-percent child)
		  (/ (* (profiler-backtrace-tree-time child) 100)
		     sample-interval
		     sample-count))
	    (setq node child)))))
    (profiler-sort-backtrace-tree tree)
    tree))

(defun profiler-sort-backtrace-tree (tree)
  (let ((children (profiler-tree-children tree)))
    (setf (profiler-tree-children tree)
	  (sort children 'profiler-backtrace-tree>))
    (mapc 'profiler-sort-backtrace-tree children)))



;;; Report View

(defcustom profiler-report-closed-mark "+"
  "An indicator of closed trees."
  :type 'string
  :group 'profiler)

(defcustom profiler-report-open-mark "-"
  "An indicator of open trees."
  :type 'string
  :group 'profiler)

(defcustom profiler-report-leaf-mark " "
  "An indicator of leaves."
  :type 'string
  :group 'profiler)

(defvar profiler-report-backtrace-format
  '((60 left)
    (14 right ((9 right)
	       (5 right "%s%%")))))

(defvar profiler-report-snapshot nil
  "A buffer-local profiler snapshot data.")

(defvar profiler-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"  'profiler-report-next)
    (define-key map "p"  'profiler-report-previous)
    (define-key map "q"  'quit-window)
    (define-key map "\r" 'profiler-report-toggle)
    (define-key map "\t" 'profiler-report-toggle)
    (define-key map "f"  'profiler-report-find-function)
    (define-key map "j"  'profiler-report-find-function)
    (define-key map [mouse-2] 'profiler-report-find-function)
    (define-key map "d"  'profiler-report-describe-function)
    map))

(defun profiler-report-make-entry-link (entry)
  (if (eq entry t)
      "Others"
    (propertize (profiler-ensure-string entry)
		'face 'link
		'mouse-face 'highlight
		'help-echo "mouse-2 or RET jumps to definition")))

(defun profiler-report-make-name-part (tree)
  (let* ((entry (profiler-tree-entry tree))
	 (depth (profiler-tree-depth tree))
	 (indent (make-string (* (1- depth) 2) ? ))
	 (mark (if (profiler-tree-leaf-p tree)
		   profiler-report-leaf-mark
		 profiler-report-closed-mark))
	 (link (profiler-report-make-entry-link entry)))
    (format "%s%s %s" indent mark link)))

(defun profiler-report-insert-backtrace-tree (tree)
  (let* ((name-part (profiler-report-make-name-part tree))
	 (time (profiler-backtrace-tree-time tree))
	 (time-percent (profiler-backtrace-tree-time-percent tree))
	 (line (profiler-format profiler-report-backtrace-format
				name-part (list time time-percent))))
    (insert (propertize (concat line "\n") 'tree tree))))

(defun profiler-report-insert-tree (tree)
  (etypecase tree
    (profiler-backtrace-tree
     (profiler-report-insert-backtrace-tree tree))))

(defun profiler-report-insert-tree-children (tree)
  (dolist (child (profiler-tree-children tree))
    (profiler-report-insert-tree child)))

(defun profiler-report-make-buffer-name (snapshot)
  (let ((mode-str
	 (profiler-title (profiler-snapshot-mode snapshot)))
	(time-str
	 (format-time-string "%Y-%m-%dT%T%z"
			     (profiler-snapshot-stop-time snapshot))))
    (format "*%s Report (%s)*" mode-str time-str)))

(defun profiler-report-setup-buffer (snapshot)
  (let* ((buf-name (profiler-report-make-buffer-name snapshot))
	 (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (profiler-report-mode)
      (setq profiler-report-snapshot snapshot))
    buffer))

(defun profiler-report-mode ()
  "Profiler Report Mode."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'profiler-report-snapshot)
  (use-local-map profiler-report-mode-map)
  (setq buffer-read-only t
	buffer-undo-list t
	truncate-lines t
	major-mode 'profiler-report-mode
	mode-name "Profiler Report"))

(defun profiler-report-tree-at-point ()
  (get-text-property (point) 'tree))

(defun profiler-report-move-to-entry ()
  (let ((point (next-single-property-change (line-beginning-position) 'face)))
    (if point
	(goto-char point)
      (back-to-indentation))))

(defun profiler-report-next ()
  (interactive)
  (forward-line)
  (profiler-report-move-to-entry))

(defun profiler-report-previous ()
  (interactive)
  (forward-line -1)
  (profiler-report-move-to-entry))

(defun profiler-report-expand ()
  "Expand the pointed tree."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward (concat profiler-report-closed-mark " ")
			  (line-end-position) t)
      (let ((tree (profiler-report-tree-at-point)))
	(when tree
	  (let ((buffer-read-only nil))
	    (replace-match (concat profiler-report-open-mark " "))
	    (forward-line)
	    (profiler-report-insert-tree-children tree)
	    t))))))

(defun profiler-report-collapse ()
  "Collpase the pointed tree."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward (concat profiler-report-open-mark " ")
			  (line-end-position) t)
      (let* ((tree (profiler-report-tree-at-point))
	     (depth (profiler-tree-depth tree))
	     (start (line-beginning-position 2))
	     d)
	(when tree
	  (let ((buffer-read-only nil))
	    (replace-match (concat profiler-report-closed-mark " "))
	    (while (and (eq (forward-line) 0)
			(let ((child (get-text-property (point) 'tree)))
			  (and child
			       (numberp (setq d (profiler-tree-depth child)))))
			(> d depth)))
	    (delete-region start (line-beginning-position)))))
      t)))

(defun profiler-report-toggle ()
  "Toggle the pointed tree."
  (interactive)
  (or (profiler-report-expand)
      (profiler-report-collapse)))

(defun profiler-report-find-function (&optional event)
  "Find function of the pointed entry."
  (interactive (list last-nonmenu-event))
  (if event (posn-set-point (event-end event)))
  (let ((tree (profiler-report-tree-at-point)))
    (when tree
      (let ((entry (profiler-tree-entry tree)))
	(find-function entry)))))

(defun profiler-report-describe-function ()
  "Describe function of the pointed entry."
  (interactive)
  (let ((tree (profiler-report-tree-at-point)))
    (when tree
      (let ((entry (profiler-tree-entry tree)))
	(require 'help-fns)
	(describe-function entry)))))



;;; Reporting Commands

(defun profiler-report-backtrace-tree ()
  "Display a backtrace tree of the current snapshot."
  (interactive)
  (setq header-line-format
	(profiler-header-line-format profiler-report-backtrace-format
				     "Name" (list "Time (ms)" "")))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (profiler-report-insert-tree-children
     (profiler-build-backtrace-tree
      profiler-report-snapshot))
    (goto-char (point-min))
    (profiler-report-move-to-entry)))

;;;###autoload
(defun profiler-report ()
  "Take a snapshot and display a backtrace tree of the snapshot."
  (interactive)
  (let* ((snapshot (profiler-take-snapshot))
	 (buffer (profiler-report-setup-buffer snapshot)))
    (with-current-buffer buffer
      (profiler-report-backtrace-tree))
    (pop-to-buffer buffer)))



;;; Profiling Commands

(defcustom profiler-sample-interval 10
  "Default sample interval in millisecond."
  :type 'integer
  :group 'profiler)

(defun profiler-read-mode ()
  (intern (completing-read "Mode: " '("sample" "trace" "memory")
			   nil t nil nil "sample")))

;;;###autoload
(defun profiler-start (mode)
  (interactive (list (profiler-read-mode)))
  (ecase mode
    (sample (profiler-start-sampling profiler-sample-interval)))
  (message "%s started" (profiler-title mode)))

;;;###autoload
(defun profiler-stop ()
  (interactive)
  (let ((mode (profiler-mode)))
    (ecase mode
      (sample (profiler-stop-sampling))
      ((nil) (error "Profiler is not started")))
    (message "%s stopped" (profiler-title mode))))

(provide 'profiler)
;;; profiler.el ends here
