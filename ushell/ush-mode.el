;;; ush-mode.el --- user interface

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation

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

(provide 'ush-mode)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-mode nil
  "This module contains code for handling input from the user."
  :tag "User interface"
  :group 'ushell)

;;; Commentary:

;; Basically, Ushell is used just like shell mode (<M-x shell>).  The
;; keystrokes for navigating the buffer, and accessing the command
;; history, are identical.  Unlike shell mode, however, Ushell mode's
;; governing process is Emacs itself.  With shell mode, an inferior
;; shell process is executed that communicates with Emacs via comint
;; -- a mode for handling sub-process interaction.  Ushell mode, on
;; the other hand, is a truly native Emacs shell.  No subprocess are
;; invoked except the ones requested by the user at the prompt.
;;
;; After entering a command, use <RET> to invoke it ([Command
;; invocation]) .  If there is a command on disk, it will be executed
;; as in a normal shell.  If there is no command by that name on disk,
;; but a Lisp function with that name is defined, the Lisp function
;; will be called, using the arguments passed on the command line.
;;
;; Some of the other features of the command interaction mode are:
;;
;; @ <M-RET> can be used to accumulate further commands while a
;;   command is currently running.  Since all input is passed to the
;;   subprocess being executed, there is no automatic input queueing
;;   as there is with other shells.
;;
;; @ <C-c C-t> can be used to truncate the buffer if it grows too
;;   large.
;;
;; @ <C-c C-r> will move point to the beginning of the output of the
;;   last command.  With a prefix argument, it will narrow to view
;;   only that output.
;;
;; @ <C-c C-o> will delete the output from the last command.
;;
;; @ <C-c C-f> will move forward a complete shell argument.
;;
;; @ <C-c C-b> will move backward a complete shell argument.

(require 'ush-module)
(require 'ush-cmd)
(require 'ush-io)
(require 'ush-var)

;;; User Variables:

(defcustom ushell-mode-unload-hook nil
  "*A hook that gets run when `ushell-mode' is unloaded."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-mode-hook nil
  "*A hook that gets run when `ushell-mode' is entered."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-first-time-mode-hook nil
  "*A hook that gets run the first time `ushell-mode' is entered.
That is to say, the first time during an Emacs session."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-exit-hook '(ushell-query-kill-processes)
  "*A hook that is run whenever `ushell' is exited.
This hook is only run if exiting actually kills the buffer."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-kill-on-exit t
  "*If non-nil, kill the Ushell buffer on the `exit' command.
Otherwise, the buffer will simply be buried."
  :type 'boolean
  :group 'ushell-mode)

(defcustom ushell-input-filter-functions nil
  "*Functions to call before input is processed.
The input is contained in the region from `ushell-last-input-start' to
`ushell-last-input-end'."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-send-direct-to-subprocesses nil
  "*If t, send any input immediately to a subprocess."
  :type 'boolean
  :group 'ushell-mode)

(defcustom ushell-expand-input-functions nil
  "*Functions to call before input is parsed.
Each function is passed two arguments, which bounds the region of the
current input text."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-scroll-to-bottom-on-input nil
  "*Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.

See `ushell-preinput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Ushell windows" nil)
		(const :tag "Scroll all windows showing the buffer" all)
		(const :tag "Scroll only the selected window" this))
  :group 'ushell-mode)

(defcustom ushell-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.  If `others',
scroll only those that are not the selected window.

See variable `ushell-scroll-show-maximum-output' and function
`ushell-postoutput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Ushell windows" nil)
		(const :tag "Scroll all windows showing the buffer" all)
		(const :tag "Scroll only the selected window" this)
		(const :tag "Scroll all windows other than selected" this))
  :group 'ushell-mode)

(defcustom ushell-scroll-show-maximum-output t
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `ushell-scroll-to-bottom-on-output' and function
`ushell-postoutput-scroll-to-bottom'."
  :type 'boolean
  :group 'ushell-mode)

(defcustom ushell-buffer-maximum-lines 1024
  "*The maximum size in lines for ushell buffers.
Ushell buffers are truncated from the top to be no greater than this
number, if the function `ushell-truncate-buffer' is on
`ushell-output-filter-functions'."
  :type 'integer
  :group 'ushell-mode)

(defcustom ushell-output-filter-functions
  '(ushell-handle-control-codes
    ushell-watch-for-password-prompt)
  "*Functions to call before output is displayed.
These functions are only called for output that is displayed
interactively, and not for output which is redirected."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-preoutput-filter-functions nil
  "*Functions to call before output is inserted into the buffer.
These functions get one argument, a string containing the text to be
inserted.  They return the string as it should be inserted."
  :type 'hook
  :group 'ushell-mode)

(defcustom ushell-password-prompt-regexp
  "[Pp]ass\\(word\\|phrase\\).*:\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process.
This is used by `ushell-watch-for-password-prompt'."
  :type 'regexp
  :group 'ushell-mode)

(defcustom ushell-skip-prompt-function nil
  "*A function called from beginning of line to skip the prompt."
  :type '(choice (const nil) function)
  :group 'ushell-mode)

(defcustom ushell-status-in-modeline t
  "*If non-nil, let the user know a command is running in the modeline."
  :type 'boolean
  :group 'ushell-mode)

(defvar ushell-first-time-p t
  "A variable which is non-nil the first time Ushell is loaded.")

;; Internal Variables:

;; these are only set to `nil' initially for the sake of the
;; byte-compiler, when compiling other files which `require' this one
(defvar ushell-mode nil)
(defvar ushell-mode-map nil)
(defvar ushell-command-running-string "--")
(defvar ushell-command-map nil)
(defvar ushell-command-prefix nil)
(defvar ushell-last-input-start nil)
(defvar ushell-last-input-end nil)
(defvar ushell-last-output-start nil)
(defvar ushell-last-output-block-begin nil)
(defvar ushell-last-output-end nil)

(defvar ushell-currently-handling-window nil)
(defvar ushell-mode-syntax-table nil)
(defvar ushell-mode-abbrev-table nil)

(define-abbrev-table 'ushell-mode-abbrev-table ())

(eval-when-compile
  (unless (ushell-under-xemacs-p)
    (defalias 'characterp 'ignore)
    (defalias 'char-int 'ignore)))

(if (not ushell-mode-syntax-table)
    (let ((i 0))
      (setq ushell-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " ushell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " ushell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " ushell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " ushell-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " ushell-mode-syntax-table)
      (modify-syntax-entry ?\t "    " ushell-mode-syntax-table)
      (modify-syntax-entry ?\f "    " ushell-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " ushell-mode-syntax-table)
      ;; Give CR the same syntax as newline, for selective-display.
      (modify-syntax-entry ?\^m ">   " ushell-mode-syntax-table)
;;;   (modify-syntax-entry ?\; "<   " ushell-mode-syntax-table)
      (modify-syntax-entry ?` "'   " ushell-mode-syntax-table)
      (modify-syntax-entry ?' "'   " ushell-mode-syntax-table)
      (modify-syntax-entry ?, "'   " ushell-mode-syntax-table)
      ;; Used to be singlequote; changed for flonums.
      (modify-syntax-entry ?. "_   " ushell-mode-syntax-table)
      (modify-syntax-entry ?- "_   " ushell-mode-syntax-table)
      (modify-syntax-entry ?| ".   " ushell-mode-syntax-table)
      (modify-syntax-entry ?# "'   " ushell-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " ushell-mode-syntax-table)
      (modify-syntax-entry ?\\ "/   " ushell-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " ushell-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " ushell-mode-syntax-table)
      (modify-syntax-entry ?\{ "(}  " ushell-mode-syntax-table)
      (modify-syntax-entry ?\} "){  " ushell-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " ushell-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " ushell-mode-syntax-table)
      ;; All non-word multibyte characters should be `symbol'.
      (if (ushell-under-xemacs-p)
	  (map-char-table
	   (function
	    (lambda (key val)
	      (and (characterp key)
		   (>= (char-int key) 256)
		   (/= (char-syntax key) ?w)
		   (modify-syntax-entry key "_   "
					ushell-mode-syntax-table))))
	   (standard-syntax-table))
	(map-char-table
	 (function
	  (lambda (key val)
	    (and (>= key 256)
		 (/= (char-syntax key) ?w)
		 (modify-syntax-entry key "_   "
				      ushell-mode-syntax-table))))
	 (standard-syntax-table)))))

;;; User Functions:

;;;###autoload
(defun ushell-mode ()
  "Emacs shell interactive mode.

\\{ushell-mode-map}"
  (kill-all-local-variables)

  (setq major-mode 'ushell-mode)
  (setq mode-name "Ushell")
  (set (make-local-variable 'ushell-mode) t)

  (make-local-variable 'ushell-mode-map)
  (setq ushell-mode-map (make-sparse-keymap))
  (use-local-map ushell-mode-map)

  (when ushell-status-in-modeline
    (make-local-variable 'ushell-command-running-string)
    (let ((fmt (ushell-copy-list mode-line-format)))
      (make-local-variable 'mode-line-format)
      (setq mode-line-format fmt))
    (let ((modeline (memq 'mode-line-modified mode-line-format)))
      (if modeline
	  (setcar modeline 'ushell-command-running-string))))

  (define-key ushell-mode-map [return] 'ushell-send-input)
  (define-key ushell-mode-map [(control ?m)] 'ushell-send-input)
  (define-key ushell-mode-map [(control ?j)] 'ushell-send-input)
  (define-key ushell-mode-map [(meta return)] 'ushell-queue-input)
  (define-key ushell-mode-map [(meta control ?m)] 'ushell-queue-input)
  (define-key ushell-mode-map [(meta control ?l)] 'ushell-show-output)

  (set (make-local-variable 'ushell-command-prefix)
       (make-symbol "ushell-command-prefix"))
  (fset ushell-command-prefix (make-sparse-keymap))
  (set (make-local-variable 'ushell-command-map)
       (symbol-function ushell-command-prefix))
  (define-key ushell-mode-map [(control ?c)] ushell-command-prefix)

  ;; without this, find-tag complains about read-only text being
  ;; modified
  (if (eq (key-binding [(meta ?.)]) 'find-tag)
      (define-key ushell-mode-map [(meta ?.)] 'ushell-find-tag))
  (define-key ushell-command-map [(meta ?o)] 'ushell-mark-output)
  (define-key ushell-command-map [(meta ?d)] 'ushell-toggle-direct-send)

  (define-key ushell-command-map [(control ?a)] 'ushell-bol)
  (define-key ushell-command-map [(control ?b)] 'ushell-backward-argument)
  (define-key ushell-command-map [(control ?e)] 'ushell-show-maximum-output)
  (define-key ushell-command-map [(control ?f)] 'ushell-forward-argument)
  (define-key ushell-command-map [return]       'ushell-copy-old-input)
  (define-key ushell-command-map [(control ?m)] 'ushell-copy-old-input)
  (define-key ushell-command-map [(control ?o)] 'ushell-kill-output)
  (define-key ushell-command-map [(control ?r)] 'ushell-show-output)
  (define-key ushell-command-map [(control ?t)] 'ushell-truncate-buffer)
  (define-key ushell-command-map [(control ?u)] 'ushell-kill-input)
  (define-key ushell-command-map [(control ?w)] 'backward-kill-word)
  (define-key ushell-command-map [(control ?y)] 'ushell-repeat-argument)

  (setq local-abbrev-table ushell-mode-abbrev-table)
  (set-syntax-table ushell-mode-syntax-table)

  (set (make-local-variable 'dired-directory) default-directory)
  (set (make-local-variable 'list-buffers-directory)
       (expand-file-name default-directory))

  ;; always set the tab width to 8 in Ushell buffers, since external
  ;; commands which do their own formatting almost always expect this
  (set (make-local-variable 'tab-width) 8)

  ;; don't ever use auto-fill in Ushell buffers
  (setq auto-fill-function nil)

  ;; always display everything from a return value
  (if (boundp 'print-length)
      (set (make-local-variable 'print-length) nil))
  (if (boundp 'print-level)
      (set (make-local-variable 'print-level) nil))

  ;; set require-final-newline to nil; otherwise, all redirected
  ;; output will end with a newline, whether or not the source
  ;; indicated it!
  (set (make-local-variable 'require-final-newline) nil)

  (set (make-local-variable 'max-lisp-eval-depth)
       (max 3000 max-lisp-eval-depth))
  (set (make-local-variable 'max-specpdl-size)
       (max 6000 max-lisp-eval-depth))

  (set (make-local-variable 'ushell-last-input-start) (point-marker))
  (set (make-local-variable 'ushell-last-input-end) (point-marker))
  (set (make-local-variable 'ushell-last-output-start) (point-marker))
  (set (make-local-variable 'ushell-last-output-end) (point-marker))
  (set (make-local-variable 'ushell-last-output-block-begin) (point))

  (let ((modules-list (ushell-copy-list ushell-modules-list)))
    (make-local-variable 'ushell-modules-list)
    (setq ushell-modules-list modules-list))

  ;; load extension modules into memory.  This will cause any global
  ;; variables they define to be visible, since some of the core
  ;; modules sometimes take advantage of their functionality if used.
  (ushell-for module ushell-modules-list
    (let ((module-fullname (symbol-name module))
	  module-shortname)
      (if (string-match "^ushell-\\(.*\\)" module-fullname)
	  (setq module-shortname
		(concat "em-" (match-string 1 module-fullname))))
      (unless module-shortname
	(error "Invalid Ushell module name: %s" module-fullname))
      (unless (featurep (intern module-shortname))
	(load module-shortname))))

  (unless (file-exists-p ushell-directory-name)
    (ushell-make-private-directory ushell-directory-name t))

  ;; load core Ushell modules for this session
  (ushell-for module (ushell-subgroups 'ushell)
    (run-hooks (intern-soft (concat (symbol-name module)
				    "-load-hook"))))

  ;; load extension modules for this session
  (ushell-for module ushell-modules-list
    (let ((load-hook (intern-soft (concat (symbol-name module)
					  "-load-hook"))))
      (if (and load-hook (boundp load-hook))
	  (run-hooks load-hook))))

  (make-local-hook 'pre-command-hook)

  (if ushell-send-direct-to-subprocesses
      (add-hook 'pre-command-hook 'ushell-intercept-commands t t))

  (if ushell-scroll-to-bottom-on-input
      (add-hook 'pre-command-hook 'ushell-preinput-scroll-to-bottom t t))

  (when ushell-scroll-show-maximum-output
    (set (make-local-variable 'scroll-conservatively) 1000))

  (when ushell-status-in-modeline
    (make-local-hook 'ushell-pre-command-hook)
    (add-hook 'ushell-pre-command-hook 'ushell-command-started nil t)
    (make-local-hook 'ushell-post-command-hook)
    (add-hook 'ushell-post-command-hook 'ushell-command-finished nil t))

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    (function
	     (lambda ()
	       (run-hooks 'ushell-exit-hook))) t t)

  (if ushell-first-time-p
      (run-hooks 'ushell-first-time-mode-hook))
  (run-hooks 'ushell-mode-hook)
  (run-hooks 'ushell-post-command-hook))

(put 'ushell-mode 'mode-class 'special)

(ushell-deftest mode major-mode
  "Major mode is correct"
  (eq major-mode 'ushell-mode))

(ushell-deftest mode ushell-mode-variable
  "`ushell-mode' is true"
  (eq ushell-mode t))

(ushell-deftest var window-height
  "LINES equals window height"
  (let ((ushell-stringify-t t))
    (ushell-command-result-p "= $LINES (window-height)" "t\n")))

(defun ushell-command-started ()
  "Indicate in the modeline that a command has started."
  (setq ushell-command-running-string "**")
  (force-mode-line-update))

(defun ushell-command-finished ()
  "Indicate in the modeline that a command has finished."
  (setq ushell-command-running-string "--")
  (force-mode-line-update))

(ushell-deftest mode command-running-p
  "Modeline shows no command running"
  (or (ushell-under-xemacs-p)
      (not ushell-status-in-modeline)
      (and (memq 'ushell-command-running-string mode-line-format)
	   (equal ushell-command-running-string "--"))))

;;; Internal Functions:

(defun ushell-toggle-direct-send ()
  (interactive)
  (if ushell-send-direct-to-subprocesses
      (progn
	(setq ushell-send-direct-to-subprocesses nil)
	(remove-hook 'pre-command-hook 'ushell-intercept-commands t)
	(message "Sending subprocess input on RET"))
    (setq ushell-send-direct-to-subprocesses t)
    (add-hook 'pre-command-hook 'ushell-intercept-commands t t)
    (message "Sending subprocess input directly")))

(defun ushell-self-insert-command (N)
  (interactive "i")
  (process-send-string
   (ushell-interactive-process)
   (char-to-string (if (symbolp last-command-char)
		       (get last-command-char 'ascii-character)
		     last-command-char))))

(defun ushell-intercept-commands ()
  (when (and (ushell-interactive-process)
	     (not (and (integerp last-input-event)
		       (memq last-input-event '(?\C-x ?\C-c)))))
    (let ((possible-events (where-is-internal this-command))
	  (name (symbol-name this-command))
	  (intercept t))
      ;; Assume that any multikey combination which does NOT target an
      ;; Ushell command, is a combo the user wants invoked rather than
      ;; sent to the underlying subprocess.
      (unless (and (> (length name) 7)
		   (equal (substring name 0 7) "ushell-"))
	(while possible-events
	  (if (> (length (car possible-events)) 1)
	      (setq intercept nil possible-events nil)
	    (setq possible-events (cdr possible-events)))))
      (if intercept
	  (setq this-command 'ushell-self-insert-command)))))

(defun ushell-find-tag (&optional tagname next-p regexp-p)
  "A special version of `find-tag' that ignores read-onlyness."
  (interactive)
  (require 'etags)
  (let ((inhibit-read-only t)
	(no-default (eobp))
	(find-tag-default-function 'ignore))
    (setq tagname (car (find-tag-interactive "Find tag: ")))
    (find-tag tagname next-p regexp-p)))

(defun ushell-move-argument (limit func property arg)
  "Move forward ARG arguments."
  (catch 'ushell-incomplete
    (ushell-parse-arguments (save-excursion (ushell-bol) (point))
			    (line-end-position)))
  (let ((pos (save-excursion
	       (funcall func 1)
	       (while (and (> arg 0) (/= (point) limit))
		 (if (get-text-property (point) property)
		     (setq arg (1- arg)))
		 (if (> arg 0)
		     (funcall func 1)))
	       (point))))
    (goto-char pos)
    (if (and (eq func 'forward-char)
	     (= (1+ pos) limit))
	(forward-char 1))))

(ushell-deftest arg forward-arg
  "Move across command arguments"
  (ushell-insert-command "echo $(+ 1 (- 4 3)) \"alpha beta\" file" 'ignore)
  (let ((here (point)) begin valid)
    (ushell-bol)
    (setq begin (point))
    (ushell-forward-argument 4)
    (setq valid (= here (point)))
    (ushell-backward-argument 4)
    (prog1
	(and valid (= begin (point)))
      (ushell-bol)
      (delete-region (point) (point-max)))))

(defun ushell-forward-argument (&optional arg)
  "Move forward ARG arguments."
  (interactive "p")
  (ushell-move-argument (point-max) 'forward-char 'arg-end arg))

(defun ushell-backward-argument (&optional arg)
  "Move backward ARG arguments."
  (interactive "p")
  (ushell-move-argument (point-min) 'backward-char 'arg-begin arg))

(defun ushell-repeat-argument (&optional arg)
  (interactive "p")
  (let ((begin (save-excursion
		 (ushell-backward-argument arg)
		 (point))))
    (kill-ring-save begin (point))
    (yank)))

(defun ushell-bol ()
  "Goes to the beginning of line, then skips past the prompt, if any."
  (interactive)
  (beginning-of-line)
  (and ushell-skip-prompt-function
       (funcall ushell-skip-prompt-function)))

(defsubst ushell-push-command-mark ()
  "Push a mark at the end of the last input text."
  (push-mark (1- ushell-last-input-end) t))

(custom-add-option 'ushell-pre-command-hook 'ushell-push-command-mark)

(defsubst ushell-goto-input-start ()
  "Goto the start of the last command input.
Putting this function on `ushell-pre-command-hook' will mimic Plan 9's
9term behavior."
  (goto-char ushell-last-input-start))

(custom-add-option 'ushell-pre-command-hook 'ushell-push-command-mark)

(defsubst ushell-interactive-print (string)
  "Print STRING to the ushell display buffer."
  (ushell-output-filter nil string))

(defsubst ushell-begin-on-new-line ()
  "This function outputs a newline if not at beginning of line."
  (save-excursion
    (goto-char ushell-last-output-end)
    (or (bolp)
	(ushell-interactive-print "\n"))))

(defsubst ushell-reset (&optional no-hooks)
  "Output a prompt on a new line, aborting any current input.
If NO-HOOKS is non-nil, then `ushell-post-command-hook' won't be run."
  (goto-char (point-max))
  (setq ushell-last-input-start (point-marker)
	ushell-last-input-end (point-marker)
	ushell-last-output-start (point-marker)
	ushell-last-output-block-begin (point)
	ushell-last-output-end (point-marker))
  (ushell-begin-on-new-line)
  (unless no-hooks
    (run-hooks 'ushell-post-command-hook)
    (goto-char (point-max))))

(defun ushell-parse-command-input (beg end &optional args)
  "Parse the command input from BEG to END.
The difference is that `ushell-parse-command' expects a complete
command string (and will error if it doesn't get one), whereas this
function will inform the caller whether more input is required.

If nil is returned, more input is necessary (probably because a
multi-line input string wasn't terminated properly).  Otherwise, it
will return the parsed command."
  (let (delim command)
    (if (setq delim
	      (catch 'ushell-incomplete
		(ignore
		 (setq command (ushell-parse-command (cons beg end)
						     args t)))))
	(ignore
	 (message "Expecting completion of delimeter %c ..."
		  (if (listp delim)
		      (car delim)
		    delim)))
      command)))

(defun ushell-update-markers (pmark)
  "Update the input and output markers relative to point and PMARK."
  (set-marker ushell-last-input-start pmark)
  (set-marker ushell-last-input-end (point))
  (set-marker ushell-last-output-end (point)))

(defun ushell-queue-input (&optional use-region)
  "Queue the current input text for execution by Ushell.
Particularly, don't send the text to the current process, even if it's
waiting for input."
  (interactive "P")
  (ushell-send-input use-region t))

(ushell-deftest mode queue-input
  "Queue command input"
  (ushell-insert-command "sleep 2")
  (ushell-insert-command "echo alpha" 'ushell-queue-input)
  (let ((count 10))
    (while (and ushell-current-command
		(> count 0))
      (sit-for 1 0)
      (setq count (1- count))))
  (ushell-match-result "alpha\n"))

(defun ushell-send-input (&optional use-region queue-p no-newline)
  "Send the input received to Ushell for parsing and processing..
After `ushell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `ushell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.

If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.

If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.

If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let ((proc-running-p (and (ushell-interactive-process)
			     (not queue-p)))
	(inhibit-point-motion-hooks t)
	after-change-functions)
    (unless (and proc-running-p
		 (not (eq (process-status
			   (ushell-interactive-process)) 'run)))
      (if (or proc-running-p
	      (>= (point) ushell-last-output-end))
	  (goto-char (point-max))
	(let ((copy (ushell-get-old-input use-region)))
	  (goto-char ushell-last-output-end)
	  (insert-and-inherit copy)))
      (unless (or no-newline
		  (and ushell-send-direct-to-subprocesses
		       proc-running-p))
	(insert-before-markers-and-inherit ?\n))
      (if proc-running-p
	  (progn
	    (ushell-update-markers ushell-last-output-end)
	    (if (or ushell-send-direct-to-subprocesses
		    (= ushell-last-input-start ushell-last-input-end))
		(unless no-newline
		  (process-send-string (ushell-interactive-process) "\n"))
	      (process-send-region (ushell-interactive-process)
				   ushell-last-input-start
				   ushell-last-input-end)))
	(if (= ushell-last-output-end (point))
	    (run-hooks 'ushell-post-command-hook)
	  (let (input)
	    (ushell-condition-case err
		(progn
		  (setq input (buffer-substring-no-properties
			       ushell-last-output-end (1- (point))))
		  (run-hook-with-args 'ushell-expand-input-functions
				      ushell-last-output-end (1- (point)))
		  (let ((cmd (ushell-parse-command-input
			      ushell-last-output-end (1- (point)))))
		    (when cmd
		      (ushell-update-markers ushell-last-output-end)
		      (setq input (buffer-substring-no-properties
				   ushell-last-input-start
				   (1- ushell-last-input-end)))
		      (run-hooks 'ushell-input-filter-functions)
		      (and (catch 'ushell-terminal
			     (ignore
			      (if (ushell-invoke-directly cmd input)
				  (eval cmd)
				(ushell-eval-command cmd input))))
			   (ushell-life-is-too-much)))))
	      (quit
	       (ushell-reset t)
	       (run-hooks 'ushell-post-command-hook)
	       (signal 'quit nil))
	      (error
	       (ushell-reset t)
	       (ushell-interactive-print
		(concat (error-message-string err) "\n"))
	       (run-hooks 'ushell-post-command-hook)
	       (insert-and-inherit input)))))))))

; (ushell-deftest proc send-to-subprocess
;   "Send input to a subprocess"
;   ;; jww (1999-12-06): what about when bc is unavailable?
;   (if (not (ushell-search-path "bc"))
;       t
;     (ushell-insert-command "bc")
;     (ushell-insert-command "1 + 2")
;     (sit-for 1 0)
;     (forward-line -1)
;     (prog1
; 	(looking-at "3\n")
;       (ushell-insert-command "quit")
;       (sit-for 1 0))))

(defsubst ushell-kill-new ()
  "Add the last input text to the kill ring."
  (kill-ring-save ushell-last-input-start ushell-last-input-end))

(custom-add-option 'ushell-input-filter-functions 'ushell-kill-new)

(defun ushell-output-filter (process string)
  "Send the output from PROCESS (STRING) to the interactive display.
This is done after all necessary filtering has been done."
  (let ((oprocbuf (if process (process-buffer process)
		    (current-buffer)))
	(inhibit-point-motion-hooks t)
	after-change-functions)
    (let ((functions ushell-preoutput-filter-functions))
      (while (and functions string)
	(setq string (funcall (car functions) string))
	(setq functions (cdr functions))))
    (if (and string oprocbuf (buffer-name oprocbuf))
	(let ((obuf (current-buffer))
	      opoint obeg oend)
	  (set-buffer oprocbuf)
	  (setq opoint (point))
	  (setq obeg (point-min))
	  (setq oend (point-max))
	  (let ((buffer-read-only nil)
		(nchars (length string))
		(ostart nil))
	    (widen)
	    (goto-char ushell-last-output-end)
	    (setq ostart (point))
	    (if (<= (point) opoint)
		(setq opoint (+ opoint nchars)))
	    (if (< (point) obeg)
		(setq obeg (+ obeg nchars)))
	    (if (<= (point) oend)
		(setq oend (+ oend nchars)))
	    (insert-before-markers string)
	    (if (= (window-start (selected-window)) (point))
		(set-window-start (selected-window)
				  (- (point) nchars)))
	    (if (= (point) ushell-last-input-end)
		(set-marker ushell-last-input-end
			    (- ushell-last-input-end nchars)))
	    (set-marker ushell-last-output-start ostart)
	    (set-marker ushell-last-output-end (point))
	    (force-mode-line-update))
	  (narrow-to-region obeg oend)
	  (goto-char opoint)
	  (ushell-run-output-filters)
	  (set-buffer obuf)))))

(defun ushell-run-output-filters ()
  "Run the `ushell-output-filter-functions' on the current output."
  (save-current-buffer
    (run-hooks 'ushell-output-filter-functions))
  (setq ushell-last-output-block-begin
	(marker-position ushell-last-output-end)))

;;; jww (1999-10-23): this needs testing
(defun ushell-preinput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Movement occurs if point in the selected window is not after the
process mark, and `this-command' is an insertion command.  Insertion
commands recognised are `self-insert-command', `yank', and
`hilit-yank'.  Depends on the value of
`ushell-scroll-to-bottom-on-input'.

This function should be a pre-command hook."
  (if (memq this-command '(self-insert-command yank hilit-yank))
      (let* ((selected (selected-window))
	     (current (current-buffer))
	     (scroll ushell-scroll-to-bottom-on-input))
	(if (< (point) ushell-last-output-end)
	    (if (eq scroll 'this)
		(goto-char (point-max))
	      (walk-windows
	       (function
		(lambda (window)
		  (when (and (eq (window-buffer window) current)
			     (or (eq scroll t) (eq scroll 'all)))
		    (select-window window)
		    (goto-char (point-max))
		    (select-window selected))))
	       nil t))))))

;;; jww (1999-10-23): this needs testing
(defun ushell-postoutput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Does not scroll if the current line is the last line in the buffer.
Depends on the value of `ushell-scroll-to-bottom-on-output' and
`ushell-scroll-show-maximum-output'.

This function should be in the list `ushell-output-filter-functions'."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (scroll ushell-scroll-to-bottom-on-output))
    (unwind-protect
	(walk-windows
	 (function
	  (lambda (window)
	    (if (eq (window-buffer window) current)
		(progn
		  (select-window window)
		  (if (and (< (point) ushell-last-output-end)
			   (or (eq scroll t) (eq scroll 'all)
			       ;; Maybe user wants point to jump to end.
			       (and (eq scroll 'this)
				    (eq selected window))
			       (and (eq scroll 'others)
				    (not (eq selected window)))
			       ;; If point was at the end, keep it at end.
			       (>= (point) ushell-last-output-start)))
		      (goto-char ushell-last-output-end))
		  ;; Optionally scroll so that the text
		  ;; ends at the bottom of the window.
		  (if (and ushell-scroll-show-maximum-output
			   (>= (point) ushell-last-output-end))
		      (save-excursion
			(goto-char (point-max))
			(recenter -1)))
		  (select-window selected)))))
	 nil t)
      (set-buffer current))))

(custom-add-option 'ushell-output-filter-functions
		   'ushell-postoutput-scroll-to-bottom)

(defun ushell-beginning-of-input ()
  "Return the location of the start of the previous input."
  ushell-last-input-start)

(defun ushell-beginning-of-output ()
  "Return the location of the end of the previous output block."
  ushell-last-input-end)

(defun ushell-end-of-output ()
  "Return the location of the end of the previous output block."
  (if (ushell-using-module 'ushell-prompt)
      ushell-last-output-start
    ushell-last-output-end))

(defun ushell-kill-output ()
  "Kill all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (save-excursion
    (goto-char (ushell-beginning-of-output))
    (insert "*** output flushed ***\n")
    (delete-region (point) (ushell-end-of-output))))

(ushell-deftest io flush-output
  "Flush previous output"
  (ushell-insert-command "echo alpha")
  (ushell-kill-output)
  (and (ushell-match-result (regexp-quote "*** output flushed ***\n"))
       (forward-line)
       (= (point) ushell-last-output-start)))

(defun ushell-show-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (goto-char (ushell-beginning-of-output))
  (set-window-start (selected-window)
		    (save-excursion
		      (goto-char (ushell-beginning-of-input))
		      (line-beginning-position)))
  (if arg
      (narrow-to-region (ushell-beginning-of-output)
			(ushell-end-of-output)))
  (ushell-end-of-output))

(defun ushell-mark-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (push-mark (ushell-show-output arg)))

(defun ushell-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (if (> (point) ushell-last-output-end)
      (kill-region ushell-last-output-end (point))
    (let ((here (point)))
      (ushell-bol)
      (kill-region (point) here))))

(defun ushell-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (interactive)
  (if (interactive-p)
      (widen))
  (goto-char (point-max))
  (recenter -1))

(defun ushell-get-old-input (&optional use-current-region)
  "Return the command input on the current line."
  (if use-current-region
      (buffer-substring (min (point) (mark))
			(max (point) (mark)))
    (save-excursion
      (beginning-of-line)
      (and ushell-skip-prompt-function
	   (funcall ushell-skip-prompt-function))
      (let ((beg (point)))
	(end-of-line)
	(buffer-substring beg (point))))))

(defun ushell-copy-old-input ()
  "Insert after prompt old input at point as new input to be edited."
  (interactive)
  (let ((input (ushell-get-old-input)))
    (goto-char ushell-last-output-end)
    (insert-and-inherit input)))

(ushell-deftest mode run-old-command
  "Re-run an old command"
  (ushell-insert-command "echo alpha")
  (goto-char ushell-last-input-start)
  (string= (ushell-get-old-input) "echo alpha"))

(defun ushell/exit ()
  "Leave or kill the Ushell buffer, depending on `ushell-kill-on-exit'."
  (throw 'ushell-terminal t))

(defun ushell-life-is-too-much ()
  "Kill the current buffer (or bury it).  Good-bye Ushell."
  (interactive)
  (if (not ushell-kill-on-exit)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun ushell-truncate-buffer ()
  "Truncate the buffer to `ushell-buffer-maximum-lines'.
This function could be on `ushell-output-filter-functions' or bound to
a key."
  (interactive)
  (save-excursion
    (goto-char ushell-last-output-end)
    (let ((lines (count-lines 1 (point)))
	  (inhibit-read-only t))
      (forward-line (- ushell-buffer-maximum-lines))
      (beginning-of-line)
      (let ((pos (point)))
	(if (bobp)
	    (if (interactive-p)
		(error "Buffer too short to truncate"))
	  (delete-region (point-min) (point))
	  (if (interactive-p)
	      (message "Truncated buffer from %d to %d lines (%.1fk freed)"
		       lines ushell-buffer-maximum-lines
		       (/ pos 1024.0))))))))

(custom-add-option 'ushell-output-filter-functions
		   'ushell-truncate-buffer)

(defun ushell-send-invisible (str)
  "Read a string without echoing.
Then send it to the process running in the current buffer."
  (interactive "P")                     ; Defeat snooping via C-x ESC ESC
  (let ((str (read-passwd
	      (format "Password: "
		      (process-name (ushell-interactive-process))))))
    (if (stringp str)
	(process-send-string (ushell-interactive-process)
			     (concat str "\n"))
      (message "Warning: text will be echoed"))))

(defun ushell-watch-for-password-prompt ()
  "Prompt in the minibuffer for password and send without echoing.
This function uses `ushell-send-invisible' to read and send a password to the
buffer's process if STRING contains a password prompt defined by
`ushell-password-prompt-regexp'.

This function could be in the list `ushell-output-filter-functions'."
  (when (ushell-interactive-process)
    (save-excursion
      (goto-char ushell-last-output-block-begin)
      (beginning-of-line)
      (if (re-search-forward ushell-password-prompt-regexp
			     ushell-last-output-end t)
	  (ushell-send-invisible nil)))))

(custom-add-option 'ushell-output-filter-functions
		   'ushell-watch-for-password-prompt)

(defun ushell-handle-control-codes ()
  "Act properly when certain control codes are seen."
  (save-excursion
    (let ((orig (point)))
      (goto-char ushell-last-output-block-begin)
      (unless (eolp)
	(beginning-of-line))
      (while (< (point) ushell-last-output-end)
	(let ((char (char-after)))
	  (cond
	   ((eq char ?\r)
	    (if (< (1+ (point)) ushell-last-output-end)
		(if (memq (char-after (1+ (point)))
			  '(?\n ?\r))
		    (delete-char 1)
		  (let ((end (1+ (point))))
		    (beginning-of-line)
		    (delete-region (point) end)))
	      (add-text-properties (point) (1+ (point))
				   '(invisible t))
	      (forward-char)))
	   ((eq char ?\a)
	    (delete-char 1)
	    (beep))
	   ((eq char ?\C-h)
	    (delete-backward-char 1)
	    (delete-char 1))
	   (t
	    (forward-char))))))))

(custom-add-option 'ushell-output-filter-functions
		   'ushell-handle-control-codes)

;;; Code:

;;; us-mode.el ends here
