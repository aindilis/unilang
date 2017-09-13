;;; em-script.el --- Ushell script files

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

(provide 'em-script)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-script nil
  "This module allows for the execution of files containing Ushell
commands, as a script file."
  :tag "Running script files."
  :group 'ushell-module)

;;; Commentary:

;;; User Variables:

(defcustom ushell-script-load-hook '(ushell-script-initialize)
  "*A list of functions to call when loading `ushell-script'."
  :type 'hook
  :group 'ushell-script)

(defcustom ushell-login-script (concat ushell-directory-name "login")
  "*If non-nil, a file to invoke when starting up Ushell interactively.
This file should be a file containing Ushell commands, where comment
lines begin with '#'."
  :type 'file
  :group 'ushell-script)

(defcustom ushell-rc-script (concat ushell-directory-name "profile")
  "*If non-nil, a file to invoke whenever Ushell is started.
This includes when running `ushell-command'."
  :type 'file
  :group 'ushell-script)

;;; Functions:

(defun ushell-script-initialize ()
  "Initialize the script parsing code."
  (make-local-variable 'ushell-interpreter-alist)
  (setq ushell-interpreter-alist
	(cons '((lambda (file)
		  (string= (file-name-nondirectory file)
			   "ushell")) . ushell/source)
	      ushell-interpreter-alist))
  (make-local-variable 'ushell-complex-commands)
  (setq ushell-complex-commands
	(append '("source" ".") ushell-complex-commands))
  ;; these two variables are changed through usage, but we don't want
  ;; to ruin it for other modules
  (let (ushell-inside-quote-regexp
	ushell-outside-quote-regexp)
    (and (not ushell-non-interactive-p)
	 ushell-login-script
	 (file-readable-p ushell-login-script)
	 (ushell-do-eval
	  (list 'ushell-commands
		(catch 'ushell-replace-command
		  (ushell-source-file ushell-login-script))) t))
    (and ushell-rc-script
	 (file-readable-p ushell-rc-script)
	 (ushell-do-eval
	  (list 'ushell-commands
		(catch 'ushell-replace-command
		  (ushell-source-file ushell-rc-script))) t))))

(defun ushell-source-file (file &optional args subcommand-p)
  "Execute a series of Ushell commands in FILE, passing ARGS.
Comments begin with '#'."
  (interactive "f")
  (let ((orig (point))
	(here (point-max))
	(inhibit-point-motion-hooks t)
	after-change-functions)
    (goto-char (point-max))
    (insert-file-contents file)
    (goto-char (point-max))
    (throw 'ushell-replace-command
	   (prog1
	       (list 'let
		     (list (list 'ushell-command-name (list 'quote file))
			   (list 'ushell-command-arguments
				 (list 'quote args)))
		     (let ((cmd (ushell-parse-command (cons here (point)))))
		       (if subcommand-p
			   (setq cmd (list 'ushell-as-subcommand cmd)))
		       cmd))
	     (delete-region here (point))
	     (goto-char orig)))))

(defun ushell/source (&rest args)
  "Source a file in a subshell environment."
  (ushell-eval-using-options
   "source" args
   '((?h "help" nil nil "show this usage screen")
     :show-usage
     :usage "FILE [ARGS]
Invoke the Ushell commands in FILE in a subshell, binding ARGS to $1,
$2, etc.")
   (ushell-source-file (car args) (cdr args) t)))

(put 'ushell/source 'ushell-no-numeric-conversions t)

(defun ushell/. (&rest args)
  "Source a file in the current environment."
  (ushell-eval-using-options
   "." args
   '((?h "help" nil nil "show this usage screen")
     :show-usage
     :usage "FILE [ARGS]
Invoke the Ushell commands in FILE within the current shell
environment, binding ARGS to $1, $2, etc.")
   (ushell-source-file (car args) (cdr args))))

(put 'ushell/. 'usell-no-numeric-conversions t)

;;; Code:

;;; em-script.el ends here
