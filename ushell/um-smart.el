;;; em-smart.el --- smart display of output

;; Copyright (C) 1999, 2000 Free Software Foundation

;; Author: John Wiegley <johnw@gnu.org>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(provide 'em-smart)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-smart nil
  "This module combines the facility of normal, modern shells with
some of the edit/review concepts inherent in the design of Plan 9's
9term.  See the docs for more details.

Most likely you will have to turn this option on and play around with
it to get a real sense of how it works."
  :tag "Smart display of output"
  :link '(info-link "(ushell)Smart display of output")
  :group 'ushell-module)

;;; Commentary:

;; The best way to get a sense of what this code is trying to do is by
;; using it.  Basically, the philosophy represents a blend between the
;; ease of use of modern day shells, and the review-before-you-proceed
;; mentality of Plan 9's 9term.
;;
;; @ When you invoke a command, it is assumed that you want to read
;;   the output of that command.
;;
;; @ If the output is not what you wanted, it is assumed that you will
;;   want to edit, and then resubmit a refined version of that
;;   command.
;;
;; @ If the output is valid, pressing any self-inserting character key
;;   will jump to end of the buffer and insert that character, in
;;   order to begin entry of a new command.
;;
;; @ If you show an intention to edit the previous command -- by
;;   moving around within it -- then the next self-inserting
;;   characters will insert *there*, instead of at the bottom of the
;;   buffer.
;;
;; @ If you show an intention to review old commands, such as M-p or
;;   M-r, point will jump to the bottom of the buffer before invoking
;;   that command.
;;
;; @ If none of the above has happened yet (i.e., your point is just
;;   sitting on the previous command), you can use SPACE and BACKSPACE
;;   (or DELETE) to page forward and backward *through the output of
;;   the last command only*.  It will constrain the movement of the
;;   point and window so that the maximum amount of output is always
;;   displayed at all times.
;;
;; @ While output is being generated from a command, the window will
;;   be constantly reconfigured (until it would otherwise make no
;;   difference) in order to always show you the most output from the
;;   command possible.  This happens if you change window sizes,
;;   scroll, etc.
;;
;; @ Like I said, it's not really comprehensible until you try it! ;)
;;
;; One disadvantage of this module is that it increases Ushell's
;; memory consumption by a factor of two or more.  With small commands
;; (such as pwd), where the screen is mostly full, consumption can
;; increase by orders of magnitude.

;;; User Variables:

(defcustom ushell-smart-load-hook '(ushell-smart-initialize)
  "*A list of functions to call when loading `ushell-smart'."
  :type 'hook
  :group 'ushell-smart)

(defcustom ushell-smart-unload-hook
  (list
   (function
    (lambda ()
      (remove-hook 'window-configuration-change-hook
		   'ushell-refrush-windows))))
  "*A hook that gets run when `ushell-smart' is unloaded."
  :type 'hook
  :group 'ushell-smart)

(defcustom ushell-review-quick-commands nil
  "*If t, always review commands.
Reviewing means keeping point on the text of the command that was just
invoked, to allow corrections to be made easily.

If set to nil, quick commands won't be reviewed.  A quick command is a
command that produces no output, and exits successfully.

If set to `not-even-short-output', then the definition of \"quick
command\" is extended to include commands that produce output, iff
that output can be presented in its entirely in the Ushell window."
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (const :tag "Not even short output"
			not-even-short-output))
  :group 'ushell-smart)

(defcustom ushell-smart-display-navigate-list
  '(insert-parentheses
    mouse-yank-at-click
    mouse-yank-secondary
    yank-pop
    yank-rectangle
    yank)
  "*A list of commands which cause Ushell to jump to the end of buffer."
  :type '(repeat function)
  :group 'ushell-smart)

(defcustom ushell-smart-space-goes-to-end t
  "*If non-nil, space will go to end of buffer when point-max is visible.
That is, if a command is running and the user presses SPACE at a time
when the end of the buffer is visible, point will go to the end of the
buffer and smart-display will be turned off (that is, subsequently
pressing backspace will not cause the buffer to scroll down).

This feature is provided to make it very easy to watch the output of a
long-running command, such as make, where it's more desirable to see
the output go by than to review it afterward.

Setting this variable to nil means that space and backspace will
always have a consistent behavior, which is to move back and forth
through displayed output.  But it also means that enabling output
tracking requires the user to manually move point to the end of the
buffer using \\[end-of-buffer]."
  :type 'boolean
  :group 'ushell-smart)

(defcustom ushell-where-to-jump 'begin
  "*This variable indicates where point should jump to after a command.
The options are `begin', `after' or `end'."
  :type '(radio (const :tag "Beginning of command" begin)
		(const :tag "After command word" after)
		(const :tag "End of command" end))
  :group 'ushell-smart)

;;; Internal Variables:

(defvar ushell-smart-displayed nil)
(defvar ushell-smart-command-done nil)
(defvar ushell-currently-handling-window nil)

;;; Functions:

(defun ushell-smart-initialize ()
  "Setup Ushell smart display."
  (unless ushell-non-interactive-p
    ;; override a few variables, since they would interfere with the
    ;; smart display functionality.
    (set (make-local-variable 'ushell-scroll-to-bottom-on-output) nil)
    (set (make-local-variable 'ushell-scroll-to-bottom-on-input) nil)
    (set (make-local-variable 'ushell-scroll-show-maximum-output) t)

    (make-local-hook 'window-scroll-functions)
    (add-hook 'window-scroll-functions 'ushell-smart-scroll-window nil t)
    (add-hook 'window-configuration-change-hook 'ushell-refrush-windows)

    (make-local-hook 'ushell-output-filter-functions)
    (add-hook 'ushell-output-filter-functions 'ushell-refrush-windows t t)

    (make-local-hook 'pre-command-hook)
    (make-local-hook 'after-change-functions)
    (add-hook 'after-change-functions 'ushell-disable-after-change nil t)

    (make-local-hook 'ushell-input-filter-functions)
    (add-hook 'ushell-input-filter-functions 'ushell-smart-display-setup nil t)

    (make-local-variable 'ushell-smart-command-done)
    (make-local-hook 'ushell-post-command-hook)
    (add-hook 'ushell-post-command-hook
	      (function
	       (lambda ()
		 (setq ushell-smart-command-done t))) t t)

    (unless (eq ushell-review-quick-commands t)
      (add-hook 'ushell-post-command-hook
		'ushell-smart-maybe-jump-to-end nil t))))

(defun ushell-smart-scroll-window (wind start)
  "Scroll the given Ushell window accordingly."
  (unless ushell-currently-handling-window
    (let ((inhibit-point-motion-hooks t)
	  (ushell-currently-handling-window t))
      (save-selected-window
	(select-window wind)
	(ushell-smart-redisplay)))))

(defun ushell-refrush-windows (&optional frame)
  "Refrush all visible Ushell buffers."
  (let (affected)
    (walk-windows
     (function
      (lambda (wind)
	(with-current-buffer (window-buffer wind)
	  (if ushell-mode
	      (let (window-scroll-functions)
		(ushell-smart-scroll-window wind (window-start))
		(setq affected t))))))
     0 frame)
    (if affected
	(let (window-scroll-functions)
	  (ushell-redisplay)))))

(defun ushell-smart-display-setup ()
  "Set the point to somewhere in the beginning of the last command."
  (cond
   ((eq ushell-where-to-jump 'begin)
    (goto-char ushell-last-input-start))
   ((eq ushell-where-to-jump 'after)
    (goto-char (next-single-property-change
		ushell-last-input-start 'arg-end))
    (if (= (point) (- ushell-last-input-end 2))
	(forward-char)))
   ((eq ushell-where-to-jump 'end)
    (goto-char (1- ushell-last-input-end)))
   (t
    (error "Invalid value for `ushell-where-to-jump'")))
  (setq ushell-smart-command-done nil)
  (add-hook 'pre-command-hook 'ushell-smart-display-move nil t)
  (ushell-refrush-windows))

(defun ushell-disable-after-change (b e l)
  "Disable smart display mode if the buffer changes in any way."
  (when ushell-smart-command-done
    (remove-hook 'pre-command-hook 'ushell-smart-display-move t)
    (setq ushell-smart-command-done nil)))

(defun ushell-smart-maybe-jump-to-end ()
  "Jump to the end of the input buffer.
This is done whenever a command exits sucessfully and both the command
and the end of the buffer are still visible."
  (when (and (= ushell-last-command-status 0)
	     (if (eq ushell-review-quick-commands 'not-even-short-output)
		 (and (pos-visible-in-window-p (point-max))
		      (pos-visible-in-window-p ushell-last-input-start))
	       (= (count-lines ushell-last-input-end
			       ushell-last-output-end) 0)))
    (goto-char (point-max))
    (remove-hook 'pre-command-hook 'ushell-smart-display-move t)))

(defun ushell-smart-redisplay ()
  "Display as much output as possible, smartly."
  (if (eobp)
      (save-excursion
	(recenter -1)
	;; trigger the redisplay now, so that we catch any attempted
	;; point motion; this is to cover for a redisplay bug
	(ushell-redisplay))
    (let ((top-point (point)))
      (and (memq 'ushell-smart-display-move pre-command-hook)
	   (>= (point) ushell-last-input-start)
	   (< (point) ushell-last-input-end)
	   (set-window-start (selected-window)
			     (line-beginning-position) t))
      (if (pos-visible-in-window-p (point-max))
	  (save-excursion
	    (goto-char (point-max))
	    (recenter -1)
	    (unless (pos-visible-in-window-p top-point)
	      (goto-char top-point)
	      (set-window-start (selected-window)
				(line-beginning-position) t)))))))

(defun ushell-smart-goto-end ()
  "Like `end-of-buffer', but do not push a mark."
  (interactive)
  (goto-char (point-max)))

(defun ushell-smart-display-move ()
  "Handle self-inserting or movement commands intelligently."
  (let (clear)
    (if (or current-prefix-arg
	    (and (> (point) ushell-last-input-start)
		 (< (point) ushell-last-input-end))
	    (>= (point) ushell-last-output-end))
	(setq clear t)
      (cond
       ((eq this-command 'self-insert-command)
	(if (eq last-command-char ? )
	    (if (and ushell-smart-space-goes-to-end
		     ushell-current-command)
		(if (not (pos-visible-in-window-p (point-max)))
		    (setq this-command 'scroll-up)
		  (setq this-command 'ushell-smart-goto-end))
	      (setq this-command 'scroll-up))
	  (setq clear t)
	  (goto-char (point-max))))
       ((eq this-command 'delete-backward-char)
	(setq this-command 'ignore)
	(if (< (point) ushell-last-input-start)
	    (ushell-show-output)
	  (if (pos-visible-in-window-p ushell-last-input-start)
	      (progn
		(ignore-errors
		  (scroll-down))
		(ushell-show-output))
	    (scroll-down)
	    (if (pos-visible-in-window-p ushell-last-input-end)
		(ushell-show-output)))))
       ((or (memq this-command ushell-smart-display-navigate-list)
	    (and (eq this-command 'ushell-send-input)
		 (not (and (>= (point) ushell-last-input-start)
			   (< (point) ushell-last-input-end)))))
	(setq clear t)
	(goto-char (point-max)))))
    (if clear
	(remove-hook 'pre-command-hook 'usell-smart-display-move t))))

;;; Code:

;;; em-smart.el ends here
