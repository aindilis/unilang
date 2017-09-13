;;; em-rebind.el --- rebind keys when point is at current input

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

(provide 'em-rebind)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-rebind nil
  "This module allows for special keybindings that only take effect
while the point is in a region of input text.  By default, it binds
C-a to move to the beginning of the input text (rather than just the
beginning of the line), and C-p and C-n to move through the input
history, C-u kills the current input text, etc.  It also, if
`ushell-confine-point-to-input' is non-nil, does not allow certain
commands to cause the point to leave the input area, such as
`backward-word', `previous-line', etc.  This module intends to mimic
the behavior of normal shells while the user editing new input text."
  :tag "Rebind keys at input"
  :group 'ushell-module)

;;; Commentary:

;;; User Variables:

(defcustom ushell-rebind-load-hook '(ushell-rebind-initialize)
  "*A list of functions to call when loading `ushell-rebind'."
  :type 'hook
  :group 'ushell-rebind)

(defcustom ushell-rebind-keys-alist
  '(([(control ?a)] . ushell-bol)
    ([home]         . ushell-bol)
    ([(control ?d)] . ushell-delchar-or-maybe-eof)
    ([backspace]    . ushell-delete-backward-char)
    ([delete]       . ushell-delete-backward-char)
    ([(control ?w)] . backward-kill-word)
    ([(control ?u)] . ushell-kill-input))
  "*Bind some keys differently if point is in input text."
  :type '(repeat (cons (vector :tag "Keys to bind"
			       (repeat :inline t sexp))
		       (function :tag "Command")))
  :group 'ushell-rebind)

(defcustom ushell-confine-point-to-input t
  "*If non-nil, do not allow the point to leave the current input.
This is more difficult to do nicely in Emacs than one might think.
Basically, the `point-left' attribute is added to the input text, and
a function is placed on that hook to take the point back to
`ushell-last-output-end' every time the user tries to move away.  But
since there are many cases in which the point _ought_ to move away
\(for programmatic reasons), the variable
`ushell-cannot-leave-input-list' defines commands which are affected
from this rule.  However, this list is by no means as complete as it
probably should be, so basically all one can hope for is that other
people will left the point alone in the Ushell buffer.  Sigh."
  :type 'boolean
  :group 'ushell-rebind)

(defcustom ushell-error-if-move-away t
  "*If non-nil, consider it an error to try to move outside current input.
This is default behavior of shells like bash."
  :type 'boolean
  :group 'ushell-rebind)

(defcustom ushell-remap-previous-input t
  "*If non-nil, remap input keybindings on previous prompts as well."
  :type 'boolean
  :group 'ushell-rebind)

(defcustom ushell-cannot-leave-input-list
  '(beginning-of-line-text
    beginning-of-line
    move-to-column
    move-to-column-force
    move-to-left-margin
    move-to-tab-stop
    forward-char
    backward-char
    delete-char
    delete-backward-char
    backward-delete-char
    backward-delete-char-untabify
    kill-paragraph
    backward-kill-paragraph
    kill-sentence
    backward-kill-sentence
    kill-sexp
    backward-kill-sexp
    kill-word
    backward-kill-word
    kill-region
    forward-list
    backward-list
    forward-page
    backward-page
    forward-point
    forward-paragraph
    backward-paragraph
    backward-prefix-chars
    forward-sentence
    backward-sentence
    forward-sexp
    backward-sexp
    forward-to-indentation
    backward-to-indentation
    backward-up-list
    forward-word
    backward-word
    forward-line
    previous-line
    next-line
    forward-visible-line
    forward-comment
    forward-thing)
  "*A list of commands that cannot leave the input area."
  :type '(repeat function)
  :group 'ushell-rebind)

;; Internal Variables:

(defvar ushell-input-keymap)
(defvar ushell-previous-point)
(defvar ushell-lock-keymap)

;;; Functions:

(defun ushell-rebind-initialize ()
  "Initialize the inputing code."
  (unless ushell-non-interactive-p
    (make-local-hook 'ushell-mode-hook)
    (add-hook 'ushell-mode-hook 'ushell-setup-input-keymap nil t)
    (make-local-hook 'pre-command-hook)
    (make-local-variable 'ushell-previous-point)
    (add-hook 'pre-command-hook 'ushell-save-previous-point nil t)
    (make-local-hook 'post-command-hook)
    (make-local-variable 'overriding-local-map)
    (add-hook 'post-command-hook 'ushell-rebind-input-map nil t)
    (set (make-local-variable 'ushell-lock-keymap) nil)
    (define-key ushell-command-map [(meta ?l)] 'ushell-lock-local-map)))

(defun ushell-lock-local-map (&optional arg)
  "Lock or unlock the current local keymap.
Within a prefix arg, set the local keymap to its normal value, and
lock it at that."
  (interactive "P")
  (if (or arg (not ushell-lock-keymap))
      (progn
	(use-local-map ushell-mode-map)
	(setq ushell-lock-keymap t)
	(message "Local keymap locked in normal mode"))
    (use-local-map ushell-input-keymap)
    (setq ushell-lock-keymap nil)
    (message "Local keymap unlocked: obey context")))

(defun ushell-save-previous-point ()
  "Save the location of point before the next command is run."
  (setq ushell-previous-point (point)))

(defsubst ushell-point-within-input-p (pos)
  "Test whether POS is within an input range."
  (let (begin)
    (or (and (>= pos ushell-last-output-end)
	     ushell-last-output-end)
	(and ushell-remap-previous-input
	     (setq begin
		   (save-excursion
		     (ushell-bol)
		     (and (not (bolp)) (point))))
	     (>= pos begin)
	     (<= pos (line-end-position))
	     begin))))

(defun ushell-rebind-input-map ()
  "Rebind the input keymap based on the location of the cursor."
  (ignore-errors
    (unless ushell-lock-keymap
      (if (ushell-point-within-input-p (point))
	  (use-local-map ushell-input-keymap)
	(let (begin)
	  (if (and ushell-confine-point-to-input
		   (setq begin
			 (ushell-point-within-input-p ushell-previous-point))
		   (memq this-command ushell-cannot-leave-input-list))
	      (progn
		(use-local-map ushell-input-keymap)
		(goto-char begin)
		(if (and ushell-error-if-move-away
			 (not (eq this-command 'kill-region)))
		    (beep)))
	    (use-local-map ushell-mode-map)))))))

(defun ushell-setup-input-keymap ()
  "Setup the input keymap to be used during input editing."
  (make-local-variable 'ushell-input-keymap)
  (setq ushell-input-keymap (make-sparse-keymap))
  (set-keymap-parent ushell-input-keymap ushell-mode-map)
  (let ((bindings ushell-rebind-keys-alist))
    (while bindings
      (define-key ushell-input-keymap (caar bindings)
	(cdar bindings))
      (setq bindings (cdr bindings)))))

(defun ushell-delete-backward-char (n &optional killflag)
  "Delete the last character, unless it's part of the output."
  (interactive "P")
  (let ((count (prefix-numeric-value n)))
    (if (ushell-point-within-input-p (- (point) count))
	(delete-backward-char count n)
      (beep))))

(defun ushell-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to subprocess.
Sends an EOF only if point is at the end of the buffer and there is no
input."
  (interactive "p")
  (let ((proc (ushell-interactive-process)))
    (if (eobp)
	(cond
	 ((/= (point) ushell-last-output-end)
	  (beep))
	 (proc
	  (process-send-eof))
	 (t
	  (ushell-life-is-too-much)))
      (usell-delete-backward-char (- arg)))))

;;; Code:

;;; em-rebind.el ends here
