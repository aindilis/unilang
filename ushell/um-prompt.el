;;; em-prompt.el --- command prompts

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

(provide 'em-prompt)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-prompt nil
  "This module provides command prompts, and navigation between them,
as is common with most shells."
  :tag "Command prompts"
  :group 'ushell-module)

;;; Commentary:

;; Most of the prompt navigation commands of `comint-mode' are
;; supported, such as C-c C-n, C-c C-p, etc.

;;; User Variables:

(defcustom ushell-prompt-load-hook '(ushell-prompt-initialize)
  "*A list of functions to call when loading `ushell-prompt'."
  :type 'hook
  :group 'ushell-prompt)

(defcustom ushell-prompt-function
  (function
   (lambda ()
     (concat (ushell/pwd)
	     (if (= (user-uid) 0) " # " " $ "))))
  "*A function that returns the Ushell prompt string.
Make sure to update `ushell-prompt-regexp' so that it will match your
prompt."
  :type 'function
  :group 'ushell-prompt)

(defcustom ushell-prompt-regexp "^[^#$\n]* [#$] "
  "*A regexp which fully matches your ushell prompt.
This setting is important, since it affects how ushell will interpret
the lines that are passed to it.
If this variable is changed, all Ushell buffers must be exited and
re-entered for it to take effect."
  :type 'regexp
  :group 'ushell-prompt)

(defcustom ushell-highlight-prompt t
  "*If non-nil, Ushell should highlight the prompt."
  :type 'boolean
  :group 'ushell-prompt)

(defface ushell-prompt-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:bold t)))
  "*The face used to highlight prompt strings.
For highlighting other kinds of strings -- similar to shell mode's
behavior -- simply use an output filer which changes text properties."
  :group 'ushell-prompt)

(defcustom ushell-before-prompt-hook nil
  "*A list of functions to call before outputting the prompt."
  :type 'hook
  :options '(ushell-begin-on-new-line)
  :group 'ushell-prompt)

(defcustom ushell-after-prompt-hook nil
  "*A list of functions to call after outputting the prompt.
Note that if `ushell-scroll-show-maximum-output' is non-nil, then
setting `ushell-show-maximum-output' here won't do much.  It depends
on whether the user wants the resizing to happen while output is
arriving, or after."
  :type 'hook
  :options '(ushell-show-maximum-output)
  :group 'ushell-prompt)

;;; Functions:

(defun ushell-prompt-initialize ()
  "Initialize the prompting code."
  (unless ushell-non-interactive-p
    (make-local-hook 'ushell-post-command-hook)
    (add-hook 'ushell-post-command-hook 'ushell-emit-prompt nil t)

    (make-local-variable 'ushell-prompt-regexp)
    (if ushell-prompt-regexp
	(set (make-local-variable 'paragraph-start) ushell-prompt-regexp))

    (set (make-local-variable 'ushell-skip-prompt-function)
	 'ushell-skip-prompt)

    (define-key ushell-command-map [(control ?n)] 'ushell-next-prompt)
    (define-key ushell-command-map [(control ?p)] 'ushell-previous-prompt)))

(defun ushell-emit-prompt ()
  "Emit a prompt if ushell is being used interactively."
  (run-hooks 'ushell-before-prompt-hook)
  (if (not ushell-prompt-function)
      (set-marker ushell-last-output-end (point))
    (let ((prompt (funcall ushell-prompt-function)))
      (and ushell-highlight-prompt
	   (add-text-properties 0 (length prompt)
				'(read-only t
				  face ushell-prompt-face
				  rear-nonsticky (face read-only))
				prompt))
      (ushell-interactive-print prompt)))
  (run-hooks 'ushell-after-prompt-hook))

(defun ushell-backward-matching-input (regexp arg)
  "Search backward through buffer for match for REGEXP.
Matches are searched for on lines that match `ushell-prompt-regexp'.
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (ushell-regexp-arg "Backward input matching (regexp): "))
  (let* ((re (concat ushell-prompt-regexp ".*" regexp))
	 (pos (save-excursion (end-of-line (if (> arg 0) 0 1))
			      (if (re-search-backward re nil t arg)
				  (point)))))
    (if (null pos)
	(progn (message "Not found")
	       (ding))
      (goto-char pos)
      (ushell-bol))))

(defun ushell-forward-matching-input (regexp arg)
  "Search forward through buffer for match for REGEXP.
Matches are searched for on lines that match `ushell-prompt-regexp'.
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (ushell-regexp-arg "Forward input matching (regexp): "))
  (ushell-backward-matching-input regexp (- arg)))

(defun ushell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
See `ushell-prompt-regexp'."
  (interactive "p")
  (forward-paragraph n)
  (ushell-skip-prompt))

(defun ushell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer.
See `ushell-prompt-regexp'."
  (interactive "p")
  (ushell-next-prompt (- (1+ n))))

(defun ushell-skip-prompt ()
  "Skip past the text matching regexp `ushell-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (line-end-position)))
    (if (and (looking-at usell-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

;;; Code:

;;; em-prompt.el ends here
