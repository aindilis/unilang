;;; em-alias.el --- creation and management of command aliases

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

(provide 'em-alias)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-alias nil
  "Command aliases allow for easy definition of alternate commands."
  :tag "Command aliases"
  :link '(info-link "(ushell)Command aliases")
  :group 'ushell-module)

;;; Commentary:

;; Command aliases greatly simplify the definition of new commands.
;; They exist as an alternative to alias functions, which are
;; otherwise quite superior, being more flexible and natural to the
;; Emacs Lisp environment (if somewhat trickier to define; [Alias
;; functions]).
;;
;;;_* Creating aliases
;;
;; The user interface is simple: type 'alias' followed by the command
;; name followed by the definition.  Argument references are made
;; using '$1', '$2', etc., or '$*'.  For example:
;;
;;   alias ll 'ls -l $*'
;;
;; This will cause the command 'll NEWS' to be replaced by 'ls -l
;; NEWS'.  This is then passed back to the command parser for
;; reparsing.{Only the command text specified in the alias definition
;; will be reparsed.  Argument references (such as '$*') are handled
;; using variable values, which means that the expansion will not be
;; reparsed, but used directly.}
;;
;; To delete an alias, specify its name without a definition:
;;
;;   alias ll
;;
;; Aliases are written to disk immediately after being defined or
;; deleted.  The filename in which they are kept is defined by the
;; following variable:

(defcustom ushell-aliases-file (concat ushell-directory-name "alias")
  "*The file in which aliases are kept.
Whenever an alias is defined by the user, using the `alias' command,
it will be written to this file.  Thus, alias definitions (and
deletions) are always permanent.  This approach was chosen for the
sake of simplicity, since that's pretty much the only benefit to be
gained by using this module."
  :type 'file
  :group 'ushell-alias)

;;;
;; The format of this file is quite basic.  It specifies the alias
;; definitions in almost exactly the same way that the user entered
;; them, minus any argument quoting (since interpolation is not done
;; when the file is read).  Hence, it is possible to add new aliases
;; to the alias file directly, using a text editor rather than the
;; `alias' command.  Or, this method can be used for editing aliases
;; that have already defined.
;;
;; Here is an example of a few different aliases, and they would
;; appear in the aliases file:
;;
;;   alias clean rm -fr **/.#*~
;;   alias commit cvs commit -m changes $*
;;   alias ll ls -l $*
;;   alias info (info)
;;   alias reindex glimpseindex -o ~/Mail
;;   alias compact for i in ~/Mail/**/*~*.bz2(Lk+50) { bzip2 -9v $i }
;;
;;;_* Auto-correction of bad commands
;;
;; When a user enters the same unknown command many times during a
;; session, it is likely that they are experiencing a spelling
;; difficulty associated with a certain command.  To combat this,
;; Ushell will offer to automatically define an alias for that
;; mispelled command, once a given tolerance thrushold has been
;; reached.

(defcustom ushell-bad-command-tolerance 3
  "*The number of failed commands to ignore before creating an alias."
  :type 'integer
  :link '(custom-manual "(ushell)Auto-correction of bad commands")
  :group 'ushell-alias)

;;;
;; Whenever the same bad command name is encountered this many times,
;; the user will be prompted in the minibuffer to provide an alias
;; name.  An alias definition will then be created which will result
;; in an equal call to the correct name.  In this way, Ushell
;; gradually learns about the commands that the user mistypes
;; frequently, and will automatically correct them!
;;
;; Note that a '$*' is automatically appended at the end of the alias
;; definition, so that entering it is unnecessary when specifying the
;; corrected command name.

;;; Code:

(defcustom ushell-alias-load-hook '(ushell-alias-initialize)
  "*A hook that gets run when `ushell-alias' is loaded."
  :type 'hook
  :group 'ushell-alias)

(defvar ushell-command-aliases-list nil
  "A list of command aliases currently defined by the user.
Each element of this alias is a list of the form:

  (NAME DEFINITION)

Where NAME is the textual name of the alias, and DEFINITION is the
command string to replace that command with.

Note: this list should not be modified in your '.emacs' file.  Rather,
any desired alias definitions should be declared using the `alias'
command, which will automatically write them to the file named by
`ushell-aliases-file'.")

(put 'ushell-command-aliases-list 'risky-local-variable t)

(defvar ushell-failed-commands-alist nil
  "An alist of command name failures.")

(defun ushell-alias-initialize ()
  "Initialize the alias handling code."
  (make-local-variable 'ushell-failed-commands-alist)
  (make-local-hook 'ushell-alternate-command-hook)
  (add-hook 'ushell-alternate-command-hook 'ushell-fix-bad-commands t t)
  (ushell-read-aliases-list)
  (make-local-hook 'ushell-named-command-hook)
  (add-hook 'ushell-named-command-hook 'ushell-maybe-replace-by-alias t t)
  (make-local-variable 'ushell-complex-commands)
  (add-to-list 'ushell-complex-commands 'ushell-command-aliased-p))

(defun ushell-command-aliased-p (name)
  (assoc name ushell-command-aliases-list))

(defun ushell/alias (&optional alias &rest definition)
  "Define an ALIAS in the user's alias list using DEFINITION."
  (if (not alias)
      (ushell-for alias ushell-command-aliases-list
	(ushell-print (apply 'format "alias %s %s\n" alias)))
    (if (not definition)
	(setq ushell-command-aliases-list
	      (delq (assoc alias ushell-command-aliases-list)
		    ushell-command-aliases-list))
      (and (stringp definition)
	   (set-text-properties 0 (length definition) nil definition))
      (let ((def (assoc alias ushell-command-aliases-list))
	    (alias-def (list alias
			     (ushell-flatten-and-stringify definition))))
	(if def
	    (setq ushell-command-aliases-list
		  (delq def ushell-command-aliases-list)))
	(setq ushell-command-aliases-list
	      (cons alias-def ushell-command-aliases-list))))
    (ushell-write-aliases-list))
  nil)

(defun pcomplete/ushell-mode/alias ()
  "Completion function for Ushell's `alias' command."
  (pcomplete-here (ushell-alias-completions pcomplete-stub)))

(defun ushell-read-aliases-list ()
  "Read in an aliases list from `ushell-aliases-file'."
  (let ((file ushell-aliases-file))
    (when (file-readable-p file)
      (setq ushell-command-aliases-list
	    (with-temp-buffer
	      (let (ushell-command-aliases-list)
		(insert-file-contents file)
		(while (not (eobp))
		  (if (re-search-forward
		       "^alias\\s-+\\(\\S-+\\)\\s-+\\(.+\\)")
		      (setq ushell-command-aliases-list
			    (cons (list (match-string 1)
					(match-string 2))
				  ushell-command-aliases-list)))
		  (forward-line 1))
		ushell-command-aliases-list))))))

(defun ushell-write-aliases-list ()
  "Write out the current aliases into `ushell-aliases-file'."
  (if (file-writable-p (file-name-directory ushell-aliases-file))
      (let ((ushell-current-handles
	     (ushell-create-handles ushell-aliases-file 'overwrite)))
	(ushell/alias)
	(ushell-close-handles 0))))

(defsubst ushell-lookup-alias (name)
  "Check whether NAME is aliased.  Return the alias if there is one."
  (assoc name ushell-command-aliases-list))

(defvar ushell-prevent-alias-expansion nil)

(defun ushell-maybe-replace-by-alias (command args)
  "If COMMAND has an alias definition, call that instead using ARGS."
  (unless (and ushell-prevent-alias-expansion
	       (member command ushell-prevent-alias-expansion))
    (let ((alias (ushell-lookup-alias command)))
      (if alias
	  (throw 'ushell-replace-command
		 (list
		  'let
		  (list
		   (list 'ushell-command-name
			 (list 'quote ushell-last-command-name))
		   (list 'ushell-command-arguments
			 (list 'quote ushell-last-arguments))
		   (list 'ushell-prevent-alias-expansion
			 (list 'quote
			       (cons command
				     ushell-prevent-alias-expansion))))
		  (ushell-parse-command (nth 1 alias))))))))

(defun ushell-alias-completions (name)
  "Find all possible completions for NAME.
These are all the command aliases which begin with NAME."
  (let (completions)
    (ushell-for alias ushell-command-aliases-list
      (if (string-match (concat "^" name) (car alias))
	  (setq completions (cons (car alias) completions))))
    completions))

(defun ushell-fix-bad-commands (name)
  "If the user repeatedly a bad command NAME, make an alias for them."
  (ignore
   (unless (file-name-directory name)
     (let ((entry (assoc name ushell-failed-commands-alist)))
       (if (not entry)
	   (setq ushell-failed-commands-alist
		 (cons (cons name 1) ushell-failed-commands-alist))
	 (if (< (cdr entry) ushell-bad-command-tolerance)
	     (setcdr entry (1+ (cdr entry)))
	   (let ((alias (concat
			 (read-string
			  (format "Define alias for \"%s\": " name))
			 " $*")))
	     (ushell/alias name alias)
	     (throw 'ushell-replace-command
		    (list
		     'let
		     (list
		      (list 'ushell-command-name
			    (list 'quote name))
		      (list 'ushell-command-arguments
			    (list 'quote ushell-last-arguments))
		      (list 'ushell-prevent-alias-expansion
			    (list 'quote
				  (cons name
					ushell-prevent-alias-expansion))))
		     (ushell-parse-command alias))))))))))

;;; em-alias.el ends here
