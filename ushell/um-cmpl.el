;;; em-cmpl.el --- completion using the TAB key

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

(provide 'em-cmpl)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-cmpl nil
  "This module provides a programmable completion function bound to
the TAB key, which allows for completing command names, file names,
variable names, arguments, etc."
  :tag "Argument completion"
  :group 'ushell-module)

;;; Commentary:

;; Ushell, by using the pcomplete package, provides a full
;; programmable completion facility that is comparable to shells like
;; tcsh or zsh.
;;
;; Completions are context-sensitive, which means that pressing <TAB>
;; after the command 'rmdir' will result in a list of directories,
;; while doing so after 'rm' will result in a list of all file
;; entries.
;;
;; Many builtin completion rules are provided, for commands such as
;; `cvs', or RedHat's `rpm' utility.  Adding new completion rules is
;; no more difficult than writing a plain Lisp functions, and they can
;; be debugged, profiled, and compiled using exactly the same
;; facilities (since in fact, they *are* just Lisp functions).  See
;; the definition of the function `pcomplete/make' for an example of
;; how to write a completion function.
;;
;; The completion facility is very easy to use.  Just press TAB.  If
;; there are a large number of possible completions, a buffer will
;; appear showing a list of them.  Completions may be selected from
;; that buffer using the mouse.  If no completion is selected, and the
;; user starts doing something else, the display buffer will
;; automatically disappear.
;;
;; If the list of possible completions is very small, Ushell will
;; "cycle" through them, selecting a different entry each time <TAB>
;; is pressed.  <S-TAB> may be used to cycle in the opposite
;; direction.
;;
;; Glob patterns can also be cycled.  For example, entering 'echo
;; x*<tab>' will cycle through all the filenames beginning with 'x'.
;; This is done because the glob list is treated as though it were a
;; list of possible completions.  Pressing <C-c SPC> will insert all
;; of the matching glob patterns at point.
;;
;; If a Lisp form is being entered, <TAB> will complete the Lisp
;; symbol name, in exactly the same way that <M-TAB> does in Emacs
;; Lisp mode.
;;
;; The list of possible completions can be viewed at any point by
;; pressing <M-?>.
;;
;; Finally, context-related help can be accessed by pressing <C-c i>.
;; This only works well if the completion function has provided Ushell
;; with sufficient pointers to locate the relevant help text.

;;; User Variables:

(defcustom ushell-cmpl-load-hook '(ushell-cmpl-initialize)
  "*A list of functions to run when `ushell-cmpl' is loaded."
  :type 'hook
  :group 'ushell-cmpl)

(defcustom ushell-show-lisp-completions nil
  "*If non-nil, include Lisp functions in the command completion list.
If this variable is nil, Lisp completion can still be done in command
position by using M-TAB instead of TAB."
  :type 'boolean
  :group 'ushell-cmpl)

(defcustom ushell-show-lisp-alternatives t
  "*If non-nil, and no other completions found, show Lisp functions.
Setting this variable means nothing if `ushell-show-lisp-completions'
is non-nil."
  :type 'boolean
  :group 'ushell-cmpl)

(defcustom ushell-no-completion-during-jobs t
  "*If non-nil, don't allow completion while a process is running."
  :type 'boolean
  :group 'ushell-cmpl)

(defcustom ushell-command-completions-alist
  '(("acroread" . "\\.pdf\\'")
    ("xpdf"     . "\\.pdf\\'")
    ("ar"       . "\\.[ao]\\'")
    ("gcc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("g++"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("cc"       . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("CC"       . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("acc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("bcc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("objdump"  . "\\(\\`[^.]*\\|\\.[ao]\\)\\'")
    ("nm"       . "\\(\\`[^.]*\\|\\.[ao]\\)\\'")
    ("gdb"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("dbx"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("sdb"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("adb"      . "\\`\\([^.]*\\|a\\.out\\)\\'"))
  "*An alist that defines simple argument type correlations.
This is provided for common commands, as a simplistic alternative
to writing a completion function."
  :type '(repeat (cons string regexp))
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-file-ignore "~\\'"
  (documentation-property 'pcomplete-file-ignore
			  'variable-documentation)
  :type (get 'pcomplete-file-ignore 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-dir-ignore
  (format "\\`\\(\\.\\.?\\|CVS\\)%c\\'" directory-sep-char)
  (documentation-property 'pcomplete-dir-ignore
			  'variable-documentation)
  :type (get 'pcomplete-dir-ignore 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-ignore-case (ushell-under-windows-p)
  (documentation-property 'pcomplete-ignore-case
			  'variable-documentation)
  :type (get 'pcomplete-ignore-case 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-autolist nil
  (documentation-property 'pcomplete-autolist
			  'variable-documentation)
  :type (get 'pcomplete-autolist 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-suffix-list (list directory-sep-char ?:)
  (documentation-property 'pcomplete-suffix-list
			  'variable-documentation)
  :type (get 'pcomplete-suffix-list 'custom-type)
  :group 'pcomplete)

(defcustom ushell-cmpl-recexact nil
  (documentation-property 'pcomplete-recexact
			  'variable-documentation)
  :type (get 'pcomplete-recexact 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-man-function 'man
  (documentation-property 'pcomplete-man-function
			  'variable-documentation)
  :type (get 'pcomplete-man-function 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-compare-entry-function 'file-newer-than-file-p
  (documentation-property 'pcomplete-compare-entry-function
			  'variable-documentation)
  :type (get 'pcomplete-compare-entry-function 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-expand-before-complete nil
  (documentation-property 'pcomplete-expand-before-complete
			  'variable-documentation)
  :type (get 'pcomplete-expand-before-complete 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-cycle-completions t
  (documentation-property 'pcomplete-cycle-completions
			  'variable-documentation)
  :type (get 'pcomplete-cycle-completions 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-cycle-cutoff-length 5
  (documentation-property 'pcomplete-cycle-cutoff-length
			  'variable-documentation)
  :type (get 'pcomplete-cycle-cutoff-length 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-restore-window-delay 1
  (documentation-property 'pcomplete-restore-window-delay
			  'variable-documentation)
  :type (get 'pcomplete-restore-window-delay 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-command-completion-function
  (function
   (lambda ()
     (pcomplete-here (ushell-complete-commands-list))))
  (documentation-property 'pcomplete-command-completion-function
			  'variable-documentation)
  :type (get 'pcomplete-command-completion-function 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-command-name-function
  'ushell-completion-command-name
  (documentation-property 'pcomplete-command-name-function
			  'variable-documentation)
  :type (get 'pcomplete-command-name-function 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-default-completion-function
  (function
   (lambda ()
     (while (pcomplete-here
	     (pcomplete-dirs-or-entries
	      (cdr (assoc (funcall ushell-cmpl-command-name-function)
			  ushell-command-completions-alist)))))))
  (documentation-property 'pcomplete-default-completion-function
			  'variable-documentation)
  :type (get 'pcomplete-default-completion-function 'custom-type)
  :group 'ushell-cmpl)

(defcustom ushell-cmpl-use-paring t
  (documentation-property 'pcomplete-use-paring 'variable-documentation)
  :type (get 'pcomplete-use-paring 'custom-type)
  :group 'ushell-cmpl)

;;; Functions:

(defun ushell-cmpl-initialize ()
  "Initialize the completions module."
  (unless (fboundp 'pcomplete)
    (load "pcmpl-auto" t t))
  (set (make-local-variable 'pcomplete-command-completion-function)
       ushell-command-completion-function)
  (set (make-local-variable 'pcomplete-command-name-function)
       ushell-cmpl-command-name-function)
  (set (make-local-variable 'pcomplete-default-completion-function)
       ushell-default-completion-function)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ushell-complete-parse-arguments)
  (set (make-local-variable 'pcomplete-file-ignore)
       ushell-cmpl-file-ignore)
  (set (make-local-variable 'pcomplete-dir-ignore)
       ushell-cmpl-dir-ignore)
  (set (make-local-variable 'pcomplete-ignore-case)
       ushell-cmpl-ignore-case)
  (set (make-local-variable 'pcomplete-autolist)
       ushell-cmpl-autolist)
  (set (make-local-variable 'pcomplete-suffix-list)
       ushell-cmpl-suffix-list)
  (set (make-local-variable 'pcomplete-recexact)
       ushell-cmpl-recexact)
  (set (make-local-variable 'pcomplete-man-function)
       ushell-cmpl-man-function)
  (set (make-local-variable 'pcomplete-compare-entry-function)
       ushell-cmpl-compare-entry-function)
  (set (make-local-variable 'pcomplete-expand-before-complete)
       ushell-cmpl-expand-before-complete)
  (set (make-local-variable 'pcomplete-cycle-completions)
       ushell-cmpl-cycle-completions)
  (set (make-local-variable 'pcomplete-cycle-cutoff-length)
       ushell-cmpl-cycle-cutoff-length)
  (set (make-local-variable 'pcomplete-restore-window-delay)
       ushell-cmpl-restore-window-delay)
  (set (make-local-variable 'pcomplete-use-paring)
       ushell-cmpl-use-paring)
  ;; `pcomplete-arg-quote-list' should only be set after all the
  ;; load-hooks for any other extension modules have been run, which
  ;; is true at the time `ushell-mode-hook' is run
  (make-local-hook 'ushell-mode-hook)
  (add-hook 'ushell-mode-hook
	    (function
	     (lambda ()
	       (set (make-local-variable 'pcomplete-arg-quote-list)
		    ushell-special-chars-outside-quoting))) nil t)
  (make-local-hook 'pcomplete-quote-arg-hook)
  (add-hook 'pcomplete-quote-arg-hook 'ushell-quote-backslash nil t)
  (define-key ushell-mode-map [(meta tab)] 'lisp-complete-symbol)
  (define-key ushell-mode-map [(meta control ?i)] 'lisp-complete-symbol)
  (define-key ushell-command-map [(meta ?h)] 'ushell-completion-help)
  (define-key ushell-command-map [tab] 'pcomplete-expand-and-complete)
  (define-key ushell-command-map [(control ?i)]
    'pcomplete-expand-and-complete)
  (define-key ushell-command-map [space] 'pcomplete-expand)
  (define-key ushell-command-map [? ] 'pcomplete-expand)
  (define-key ushell-mode-map [tab] 'pcomplete)
  (define-key ushell-mode-map [(control ?i)] 'pcomplete)
  ;; jww (1999-10-19): Will this work on anything but X?
  (if (ushell-under-xemacs-p)
      (define-key ushell-mode-map [iso-left-tab] 'pcomplete-reverse)
    (define-key ushell-mode-map [(shift iso-lefttab)] 'pcomplete-reverse)
    (define-key ushell-mode-map [(shift control ?i)] 'pcomplete-reverse))
  (define-key ushell-mode-map [(meta ??)] 'pcomplete-list))

(defun ushell-completion-command-name ()
  "Return the command name, possibly sans globbing."
  (let ((cmd (file-name-nondirectory (pcomplete-arg 'first))))
    (setq cmd (if (and (> (length cmd) 0)
		       (eq (aref cmd 0) ushell-explicit-command-char))
		  (substring cmd 1)
		cmd))
    (if (ushell-under-windows-p)
	(file-name-sans-extension cmd)
      cmd)))

(defun ushell-completion-help ()
  (interactive)
  (if (= (point) ushell-last-output-end)
      (describe-prefix-bindings)
    (call-interactively 'pcomplete-help)))

(defun ushell-complete-parse-arguments ()
  "Parse the command line arguments for `pcomplete-argument'."
  (when (and ushell-no-completion-during-jobs
	     (ushell-interactive-process))
    (insert-and-inherit "\t")
    (throw 'pcompleted t))
  (let ((end (point-marker))
	(begin (save-excursion (ushell-bol) (point)))
	(posns (list t))
	args delim)
    (when (memq this-command '(pcomplete-expand
			       pcomplete-expand-and-complete))
      (run-hook-with-args 'ushell-expand-input-functions begin end)
      (if (= begin end)
	  (end-of-line))
      (setq end (point-marker)))
    (if (setq delim
	      (catch 'ushell-incomplete
		(ignore
		 (setq args (ushell-parse-arguments begin end)))))
	(cond ((memq (car delim) '(?\{ ?\<))
	       (setq begin (1+ (cadr delim))
		     args (ushell-parse-arguments begin end)))
	      ((eq (car delim) ?\()
	       (lisp-complete-symbol)
	       (throw 'pcompleted t))
	      (t
	       (insert-and-inherit "\t")
	       (throw 'pcompleted t))))
    (when (get-text-property (1- end) 'comment)
      (insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((pos begin))
      (while (< pos end)
	(if (get-text-property pos 'arg-begin)
	    (nconc posns (list pos)))
	(setq pos (1+ pos))))
    (setq posns (cdr posns))
    (assert (= (length args) (length posns)))
    (let ((a args)
	  (i 0)
	  l final)
      (while a
	(if (and (consp (car a))
		 (eq (caar a) 'ushell-operator))
	    (setq l i))
	(setq a (cdr a) i (1+ i)))
      (and l
	   (setq args (nthcdr (1+ l) args)
		 posns (nthcdr (1+ l) posns))))
    (assert (= (length args) (length posns)))
    (when (and args (eq (char-syntax (char-before end)) ? ))
      (nconc args (list ""))
      (nconc posns (list (point))))
    (cons (mapcar
	   (function
	    (lambda (arg)
	      (let ((val
		     (if (listp arg)
			 (let ((result
				(ushell-do-eval
				 (list 'ushell-commands arg) t)))
			   (assert (eq (car result) 'quote))
			   (cadr result))
		       arg)))
		(if (numberp val)
		    (setq val (number-to-string val)))
		(or val ""))))
	   args)
	  posns)))

(defun ushell-complete-commands-list ()
  "Generate list of applicable, visible commands."
  (let ((filename (pcomplete-arg)) glob-name)
    (if (file-name-directory filename)
	(pcomplete-executables)
      (if (and (> (length filename) 0)
	       (eq (aref filename 0) ushell-explicit-command-char))
	  (setq filename (substring filename 1)
		pcomplete-stub filename
		glob-name t))
      (let* ((paths (split-string (getenv "PATH") path-separator))
	     (cwd (file-name-as-directory
		   (expand-file-name default-directory)))
	     (path "") (comps-in-path ())
	     (file "") (filepath "") (completions ()))
	;; Go thru each path in the search path, finding completions.
	(while paths
	  (setq path (file-name-as-directory
		      (expand-file-name (or (car paths) ".")))
		comps-in-path
		(and (file-accessible-directory-p path)
		     (file-name-all-completions filename path)))
	  ;; Go thru each completion found, to see whether it should
	  ;; be used.
	  (while comps-in-path
	    (setq file (car comps-in-path)
		  filepath (concat path file))
	    (if (and (not (member file completions)) ;
		     (or (string-equal path cwd)
			 (not (file-directory-p filepath)))
		     (file-executable-p filepath))
		(setq completions (cons file completions)))
	    (setq comps-in-path (cdr comps-in-path)))
	  (setq paths (cdr paths)))
	;; Add aliases which are currently visible, and Lisp functions.
	(pcomplete-uniqify-list
	 (if glob-name
	     completions
	   (setq completions
		 (append (and (ushell-using-module 'ushell-alias)
			      (funcall (symbol-function 'ushell-alias-completions)
				       filename))
			 (ushell-winnow-list
			  (mapcar
			   (function
			    (lambda (name)
			      (substring name 7)))
			   (all-completions (concat "ushell/" filename)
					    obarray 'functionp))
			  nil '(ushell-find-alias-function))
			 completions))
	   (append (and (or ushell-show-lisp-completions
			    (and usell-show-lisp-alternatives
				 (null completions)))
			(all-completions filename obarray 'functionp))
		   completions)))))))

;;; Code:

;;; em-cmpl.el ends here
