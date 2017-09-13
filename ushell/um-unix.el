;;; em-unix.el --- UNIX command aliases

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

(provide 'em-unix)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-unix nil
  "This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Ushell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Ushell background processes
by name)."
  :tag "UNIX commands in Lisp"
  :group 'ushell-module)

;;; Commentary:

;; This file contains implementations of several UNIX command in Emacs
;; Lisp, for several reasons:
;;
;;   1) it makes them available on all platforms where the Lisp
;;      functions used are available
;;
;;   2) it makes their functionality accessible and modified by the
;;      Lisp programmer.
;;
;;   3) it allows Ushell to refrain from having to invoke external
;;      processes for common operations.

(defcustom ushell-unix-load-hook '(ushell-unix-initialize)
  "*A list of functions to run when `ushell-unix' is loaded."
  :type 'hook
  :group 'ushell-unix)

(defcustom ushell-plain-grep-behavior nil
  "*If non-nil, standalone \"grep\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-no-grep-available (not (ushell-search-path "grep"))
  "*If non-nil, no grep is available on the current machine."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-plain-diff-behavior nil
  "*If non-nil, standalone \"diff\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-plain-locate-behavior nil
  "*If non-nil, standalone \"locate\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-rm-removes-directories nil
  "*If non-nil, `rm' will remove directory entries.
Otherwise, `rmdir' is required."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-rm-interactive-query (= (user-uid) 0)
  "*If non-nil, `rm' will query before removing anything."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-mv-interactive-query (= (user-uid) 0)
  "*If non-nil, `mv' will query before overwriting anything."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-mv-overwrite-files t
  "*If non-nil, `mv' will overwrite files without warning."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-cp-interactive-query (= (user-uid) 0)
  "*If non-nil, `cp' will query before overwriting anything."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-cp-overwrite-files t
  "*If non-nil, `cp' will overwrite files without warning."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-ln-interactive-query (= (user-uid) 0)
  "*If non-nil, `ln' will query before overwriting anything."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-ln-overwrite-files nil
  "*If non-nil, `ln' will overwrite files without warning."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-default-target-is-dot nil
  "*If non-nil, the default destination for cp, mv or ln is `.'."
  :type 'boolean
  :group 'ushell-unix)

(defcustom ushell-du-prefer-over-ange nil
  "*Use Ushell's du in ange-ftp remote directories.
Otherwise, Emacs will attempt to use rsh to invoke du on the remote machine."
  :type 'boolean
  :group 'ushell-unix)

(require 'ush-opt)

;;; Functions:

(defun ushell-unix-initialize ()
  "Initialize the UNIX support/emulation code."
  (make-local-hook 'ushell-post-command-hook)
  (when (ushell-using-module 'ushell-cmpl)
    (make-local-hook 'pcomplete-try-first-hook)
    (add-hook 'pcomplete-try-first-hook
	      'ushell-complete-host-reference nil t))
  (make-local-variable 'ushell-complex-commands)
  (setq ushell-complex-commands
	(append '("grep" "egrep" "fgrep" "agrep" "glimpse" "locate"
		  "cat" "time" "cp" "mv" "make" "du" "diff")
		ushell-complex-commands)))

(defalias 'ushell/date     'current-time-string)
(defalias 'ushell/basename 'file-name-nondirectory)
(defalias 'ushell/dirname  'file-name-directory)

(eval-when-compile
  (defvar interactive)
  (defvar preview)
  (defvar recursive)
  (defvar verbose))

(defun ushell/man (&rest args)
  "Invoke man, flattening the arguments appropriately."
  (funcall 'man (apply 'ushell-flatten-and-stringify args)))

(put 'ushell/man 'ushell-no-numeric-conversions t)

(defun ushell-remove-entries (path files &optional top-level)
  "From PATH, remove all of the given FILES, perhaps interactively."
  (while files
    (if (string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if top-level
	    (ushell-error "rm: cannot remove `.' or `..'\n"))
      (if (and (file-directory-p (car files))
	       (not (file-symlink-p (car files))))
	  (let ((dir (file-name-as-directory (car files))))
	    (ushell-remove-entries dir
				   (mapcar
				    (function
				     (lambda (file)
				       (concat dir file)))
				    (directory-files dir)))
	    (if verbose
		(ushell-printn (format "rm: removing directory `%s'"
				       (car files))))
	    (unless
		(or preview
		    (and interactive
			 (not (y-or-n-p
			       (format "rm: remove directory `%s'? "
				       (car files))))))
	      (ushell-funcalln 'delete-directory (car files))))
	(if verbose
	    (ushell-printn (format "rm: removing file `%s'"
				   (car files))))
	(unless (or preview
		    (and interactive
			 (not (y-or-n-p
			       (format "rm: remove `%s'? "
				       (car files))))))
	  (ushell-funcalln 'delete-file (car files)))))
    (setq files (cdr files))))

(defun ushell/rm (&rest args)
  "Implementation of rm in Lisp.
This is implemented to call either `delete-file', `kill-buffer',
`kill-process', or `unintern', depending on the nature of the
argument."
  (setq args (ushell-flatten-list args))
  (ushell-eval-using-options
   "rm" args
   '((?h "help" nil nil "show this usage screen")
     (?f "force" nil force-removal "force removal")
     (?i "interactive" nil interactive "prompt before any removal")
     (?n "preview" nil preview "don't change anything on disk")
     (?r "recursive" nil recursive
	 "remove the contents of directories recursively")
     (?R nil nil recursive "(same)")
     (?v "verbose" nil verbose "explain what is being done")
     :preserve-args
     :external "rm"
     :show-usage
     :usage "[OPTION]... FILE...
Remove (unlink) the FILE(s).")
   (unless interactive
     (setq interactive ushell-rm-interactive-query))
   (if (and force-removal interactive)
       (setq interactive nil))
   (while args
     (let ((entry (if (stringp (car args))
		      (directory-file-name (car args))
		    (if (numberp (car args))
			(number-to-string (car args))
		      (car args)))))
       (cond
	((bufferp entry)
	 (if verbose
	     (ushell-printn (format "rm: removing buffer `%s'" entry)))
	 (unless (or preview
		     (and interactive
			  (not (y-or-n-p (format "rm: delete buffer `%s'? "
						 entry)))))
	   (ushell-funcalln 'kill-buffer entry)))
	((ushell-processp entry)
	 (if verbose
	     (ushell-printn (format "rm: killing process `%s'" entry)))
	 (unless (or preview
		     (and interactive
			  (not (y-or-n-p (format "rm: kill process `%s'? "
						 entry)))))
	   (ushell-funcalln 'kill-process entry)))
	((symbolp entry)
	 (if verbose
	     (ushell-printn (format "rm: uninterning symbol `%s'" entry)))
	 (unless
	     (or preview
		 (and interactive
		      (not (y-or-n-p (format "rm: unintern symbol `%s'? "
					     entry)))))
	   (ushell-funcalln 'unintern entry)))
	((stringp entry)
	 (if (and (file-directory-p entry)
		  (not (file-symlink-p entry)))
	     (if (or recursive
		     ushell-rm-removes-directories)
		 (if (or preview
			 (not interactive)
			 (y-or-n-p
			  (format "rm: descend into directory `%s'? "
				  entry)))
		     (ushell-remove-entries nil (list entry) t))
	       (ushell-error (format "rm: %s: is a directory\n" entry)))
	   (ushell-remove-entries nil (list entry) t)))))
     (setq args (cdr args)))
   nil))

(put 'ushell/rm 'ushell-no-numeric-conversions t)

(defun ushell/mkdir (&rest args)
  "Implementation of mkdir in Lisp."
  (ushell-eval-using-options
   "mkdir" args
   '((?h "help" nil nil "show this usage screen")
     :external "mkdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Create the DIRECTORY(ies), if they do not already exist.")
   (while args
     (ushell-funcalln 'make-directory (car args))
     (setq args (cdr args)))
   nil))

(put 'ushell/mkdir 'ushell-no-numeric-conversions t)

(defun ushell/rmdir (&rest args)
  "Implementation of rmdir in Lisp."
  (ushell-eval-using-options
   "rmdir" args
   '((?h "help" nil nil "show this usage screen")
     :external "rmdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Remove the DIRECTORY(ies), if they are empty.")
   (while args
     (ushell-funcalln 'delete-directory (car args))
     (setq args (cdr args)))
   nil))

(put 'ushell/rmdir 'ushell-no-numeric-conversions t)

(eval-when-compile
  (defvar no-dereference)
  (defvar preview)
  (defvar verbose))

(defvar ushell-warn-dot-directories t)

(defun ushell-shuffle-files (command action files target func deep &rest args)
  "Shuffle around some filesystem entries, using FUNC to do the work."
  (let ((attr-target (ushell-file-attributes target))
	(is-dir (or (file-directory-p target)
		    (and preview (not ushell-warn-dot-directories))))
	attr)
    (if (and (not preview) (not is-dir)
	     (> (length files) 1))
	(error "%s: when %s multiple files, last argument must be a directory"
	       command action))
    (while files
      (setcar files (directory-file-name (car files)))
      (cond
       ((string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if ushell-warn-dot-directories
	    (ushell-error (format "%s: %s: omitting directory\n"
				  command (car files)))))
       ((and attr-target
	     (or (not (ushell-under-windows-p))
		 (eq system-type 'ms-dos))
	     (setq attr (ushell-file-attributes (car files)))
	     (nth 10 attr-target) (nth 10 attr)
	     ;; Use equal, not -, since the inode and the device could
	     ;; cons cells.
	     (equal (nth 10 attr-target) (nth 10 attr))
	     (nth 11 attr-target) (nth 11 attr)
	     (equal (nth 11 attr-target) (nth 11 attr)))
	(ushell-error (format "%s: `%s' and `%s' are the same file\n"
			      command (car files) target)))
       (t
	(let ((source (car files))
	      (target (if is-dir
			  (expand-file-name
			   (file-name-nondirectory (car files)) target)
			target))
	      link)
	  (if (and (file-directory-p source)
		   (or (not no-dereference)
		       (not (file-symlink-p source)))
		   (not (memq func '(make-symbolic-link
				     add-name-to-file))))
	      (if (and (eq func 'copy-file)
		       (not recursive))
		  (ushell-error (format "%s: %s: omitting directory\n"
					command (car files)))
		(let (ushell-warn-dot-directories)
		  (if (and (not deep)
			   (eq func 'rename-file)
			   ;; Use equal, since the device might be a
			   ;; cons cell.
			   (equal (nth 11 (ushell-file-attributes
					   (file-name-directory
					    (directory-file-name
					     (expand-file-name source)))))
				  (nth 11 (ushell-file-attributes
					   (file-name-directory
					    (directory-file-name
					     (expand-file-name target)))))))
		      (apply 'ushell-funcalln func source target args)
		  (unless (file-directory-p target)
		    (if verbose
			(ushell-printn
			 (format "%s: making directory %s"
				 command target)))
		    (unless preview
		      (ushell-funcalln 'make-directory target)))
		  (apply 'ushell-shuffle-files
			 command action
			 (mapcar
			  (function
			   (lambda (file)
			     (concat source "/" file)))
			  (directory-files source))
			 target func t args)
		  (when (eq func 'rename-file)
		    (if verbose
			(ushell-printn
			 (format "%s: deleting directory %s"
				 command source)))
		    (unless preview
		      (ushell-funcalln 'delete-directory source))))))
	    (if verbose
		(ushell-printn (format "%s: %s -> %s" command
				       source target)))
	    (unless preview
	      (if (and no-dereference
		       (setq link (file-symlink-p source)))
		  (progn
		    (apply 'ushell-funcalln 'make-symbolic-link
			   link target args)
		    (if (eq func 'rename-file)
			(if (and (file-directory-p source)
				 (not (file-symlink-p source)))
			    (ushell-funcalln 'delete-directory source)
			  (ushell-funcalln 'delete-file source))))
		(apply 'ushell-funcalln func source target args)))))))
      (setq files (cdr files)))))

(defun ushell-shorthand-tar-command (command args)
  "Rewrite `cp -v dir a.tar.gz' to `tar cvzf a.tar.gz dir'."
  (let* ((archive (car (last args)))
	 (tar-args
	  (cond ((string-match "z2" archive) "If")
		((string-match "gz" archive) "zf")
		((string-match "\\(az\\|Z\\)" archive) "Zf")
		(t "f"))))
    (if (file-exists-p archive)
	(setq tar-args (concat "u" tar-args))
      (setq tar-args (concat "c" tar-args)))
    (if verbose
	(setq tar-args (concat "v" tar-args)))
    (if (equal command "mv")
	(setq tar-args (concat "--remove-files -" tar-args)))
    ;; truncate the archive name from the arguments
    (setcdr (last args 2) nil)
    (throw 'ushell-replace-command
	   (ushell-parse-command
	    (format "tar %s %s" tar-args archive) args))))

;; this is to avoid duplicating code...
(defmacro ushell-mvcpln-template (command action func query-var
					  force-var &optional preserve)
  `(let ((len (length args)))
     (if (or (= len 0)
	     (and (= len 1) (null ushell-default-target-is-dot)))
	 (error "%s: missing destination file or directory" ,command))
     (if (= len 1)
	 (nconc args '(".")))
     (setq args (ushell-stringify-list (ushell-flatten-list args)))
     (if (and ,(not (equal command "ln"))
	      (string-match ushell-tar-regexp (car (last args)))
	      (or (> (length args) 2)
		  (and (file-directory-p (car args))
		       (or (not no-dereference)
			   (not (file-symlink-p (car args)))))))
	 (ushell-shorthand-tar-command ,command args)
       (let ((target (car (last args)))
	     ange-cache)
	 (setcdr (last args 2) nil)
	 (ushell-shuffle-files
	  ,command ,action args target ,func nil
	  ,@(append
	     `((if (and (or interactive
			    ,query-var)
			(not force))
		   1 (or force ,force-var)))
	     (if preserve
		 (list preserve)))))
       nil)))

(defun ushell/mv (&rest args)
  "Implementation of mv in Lisp."
  (ushell-eval-using-options
   "mv" args
   '((?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil interactive
	 "request confirmation if target already exists")
     (?n "preview" nil preview
	 "don't change anything on disk")
     (?v "verbose" nil verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "mv"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or: mv [OPTION]... SOURCE... DIRECTORY
Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY.
\[OPTION] DIRECTORY...")
   (let ((no-dereference t))
     (ushell-mvcpln-template "mv" "moving" 'rename-file
			     ushell-mv-interactive-query
			     ushell-mv-overwrite-files))))

(put 'ushell/mv 'ushell-no-numeric-conversions t)

(defun ushell/cp (&rest args)
  "Implementation of cp in Lisp."
  (ushell-eval-using-options
   "cp" args
   '((?a "archive" nil archive
	 "same as -dpR")
     (?d "no-dereference" nil no-dereference
	 "preserve links")
     (?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil interactive
	 "request confirmation if target already exists")
     (?n "preview" nil preview
	 "don't change anything on disk")
     (?p "preserve" nil preserve
	 "preserve file attributes if possible")
     (?R "recursive" nil recursive
	 "copy directories recursively")
     (?v "verbose" nil verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "cp"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or:  cp [OPTION]... SOURCE... DIRECTORY
Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.")
   (if archive
       (setq preserve t no-dereference t recursive t))
   (ushell-mvcpln-template "cp" "copying" 'copy-file
			   ushell-cp-interactive-query
			   ushell-cp-overwrite-files preserve)))

(put 'ushell/cp 'ushell-no-numeric-conversions t)

(defun ushell/ln (&rest args)
  "Implementation of ln in Lisp."
  (ushell-eval-using-options
   "ln" args
   '((?h "help" nil nil "show this usage screen")
     (?s "symbolic" nil symbolic
	 "make symbolic links instead of hard links")
     (?i "interactive" nil interactive
	 "request confirmation if target already exists")
     (?f "force" nil force "remove existing destinations, never prompt")
     (?n "preview" nil preview
	 "don't change anything on disk")
     (?v "verbose" nil verbose "explain what is being done")
     :preserve-args
     :external "ln"
     :show-usage
     :usage "[OPTION]... TARGET [LINK_NAME]
   or:  ln [OPTION]... TARGET... DIRECTORY
Create a link to the specified TARGET with optional LINK_NAME.  If there is
more than one TARGET, the last argument must be a directory;  create links
in DIRECTORY to each TARGET.  Create hard links by default, symbolic links
with '--symbolic'.  When creating hard links, each TARGET must exist.")
   (let ((no-dereference t))
     (ushell-mvcpln-template "ln" "linking"
			     (if symbolic
				 'make-symbolic-link
			       'add-name-to-file)
			     ushell-ln-interactive-query
			     ushell-ln-overwrite-files))))

(put 'ushell/ln 'ushell-no-numeric-conversions t)

(defun ushell/cat (&rest args)
  "Implementation of cat in Lisp.
If in a pipeline, or the file is not a regular file, directory or
symlink, then revert to the system's definition of cat."
  (setq args (ushell-stringify-list (ushell-flatten-list args)))
  (if (or ushell-in-pipeline-p
	  (catch 'special
	    (ushell-for arg args
	      (unless (or (and (stringp arg)
			       (> (length arg) 0)
			       (eq (aref arg 0) ?-))
			  (let ((attrs (ushell-file-attributes arg)))
			    (and attrs (memq (aref (nth 8 attrs) 0)
					     '(?d ?l ?-)))))
		(throw 'special t)))))
      (let ((ext-cat (ushell-search-path "cat")))
	(if ext-cat
	    (throw 'ushell-replace-command
		   (ushell-parse-command ext-cat args))
	  (if ushell-in-pipeline-p
	      (error "Ushell's `cat' does not work in pipelines")
	    (error "Ushell's `cat' cannot display one of the files given"))))
    (ushell-init-print-buffer)
    (ushell-eval-using-options
     "cat" args
     '((?h "help" nil nil "show this usage screen")
       :external "cat"
       :show-usage
       :usage "[OPTION] FILE...
Concatenate FILE(s), or standard input, to standard output.")
     (ushell-for file args
       (if (string= file "-")
	   (throw 'ushell-external
		  (ushell-external-command "cat" args))))
     (let ((curbuf (current-buffer)))
       (ushell-for file args
	 (with-temp-buffer
	   (insert-file-contents file)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (let ((str (buffer-substring
			 (point) (min (1+ (line-end-position))
				      (point-max)))))
	       (with-current-buffer curbuf
		 (ushell-buffered-print str)))
	     (forward-line)))))
     (ushell-flush)
     ;; if the file does not end in a newline, do not emit one
     (setq ushell-ensure-newline-p nil))))

(put 'ushell/cat 'ushell-no-numeric-conversions t)

;; special front-end functions for compilation-mode buffers

(defun ushell/make (&rest args)
  "Use `compile' to do background makes."
  (if (and ushell-current-subjob-p
	   (ushell-interactive-output-p))
      (let ((compilation-process-setup-function
	     (list 'lambda nil
		   (list 'setq 'process-environment
			 (list 'quote (ushell-copy-environment))))))
	(compile (concat "make " (ushell-flatten-and-stringify args))))
    (throw 'ushell-replace-command
	   (ushell-parse-command "*make" (ushell-stringify-list
					  (ushell-flatten-list args))))))

(put 'ushell/make 'ushell-no-numeric-conversions t)

(defun ushell-occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun ushell-occur-mode-mouse-goto (event)
  "In Occur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (buffer pos)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq pos (occur-mode-find-occurrence))
	(setq buffer occur-buffer)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun ushell-poor-mans-grep (args)
  "A poor version of grep that opens every file and uses `occur'.
This eats up memory, since it leaves the buffers open (to speed future
searches), and it's very slow.  But, if your system has no grep
available..."
  (save-selected-window
    (let ((default-dir default-directory))
      (with-current-buffer (get-buffer-create "*grep*")
	(let ((inhibit-read-only t)
	      (default-directory default-dir))
	  (erase-buffer)
	  (occur-mode)
	  (let ((files (ushell-stringify-list
			(ushell-flatten-list (cdr args))))
		(inhibit-redisplay t)
		string)
	    (when (car args)
	      (if (get-buffer "*Occur*")
		  (kill-buffer (get-buffer "*Occur*")))
	      (setq string nil)
	      (while files
		(with-current-buffer (find-file-noselect (car files))
		  (save-excursion
		    (ignore-errors
		      (occur (car args))))
		  (if (get-buffer "*Occur*")
		      (with-current-buffer (get-buffer "*Occur*")
			(setq string (buffer-string))
			(kill-buffer (current-buffer)))))
		(if string (insert string))
		(setq string nil
		      files (cdr files)))))
	  (setq occur-buffer (current-buffer))
	  (local-set-key [mouse-2] 'ushell-occur-mode-mouse-goto)
	  (local-set-key [(control ?c) (control ?c)]
			 'ushell-occur-mode-goto-occurrence)
	  (local-set-key [(control ?m)]
			 'ushell-occur-mode-goto-occurrence)
	  (local-set-key [return] 'ushell-occur-mode-goto-occurrence)
	  (pop-to-buffer (current-buffer) t)
	  (goto-char (point-min))
	  (resize-temp-buffer-window))))))

(defun ushell-grep (command args &optional maybe-use-occur)
  "Generic service function for the various grep aliases.
It calls Emacs' grep utility if the command is not redirecting output,
and if it's not part of a command pipeline.  Otherwise, it calls the
external command."
  (if (and maybe-use-occur ushell-no-grep-available)
      (ushell-poor-mans-grep args)
    (if (or ushell-plain-grep-behavior
	    (not (and (ushell-interactive-output-p)
		      (not ushell-in-pipeline-p)
		      (not ushell-in-subcommand-p))))
	(throw 'ushell-replace-command
	       (ushell-parse-command (concat "*" command)
				     (ushell-stringify-list
				      (ushell-flatten-list args))))
      (let* ((compilation-process-setup-function
	      (list 'lambda nil
		    (list 'setq 'process-environment
			  (list 'quote (ushell-copy-environment)))))
	     (args (mapconcat 'identity
			      (mapcar 'shell-quote-argument
				      (ushell-stringify-list
				       (ushell-flatten-list args)))
			      " "))
	     (cmd (progn
		    (set-text-properties 0 (length args)
					 '(invisible t) args)
		    (format "%s -n %s" command args)))
	     compilation-scroll-output)
	(grep cmd)))))

(defun ushell/grep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (ushell-grep "grep" args t))

(defun ushell/egrep (&rest args)
  "Use Emacs grep facility instead of calling external egrep."
  (ushell-grep "egrep" args t))

(defun ushell/fgrep (&rest args)
  "Use Emacs grep facility instead of calling external fgrep."
  (ushell-grep "fgrep" args t))

(defun ushell/agrep (&rest args)
  "Use Emacs grep facility instead of calling external agrep."
  (ushell-grep "agrep" args))

(defun ushell/glimpse (&rest args)
  "Use Emacs grep facility instead of calling external glimpse."
  (let (null-device)
    (ushell-grep "glimpse" (append '("-z" "-y") args))))

;; completions rules for some common UNIX commands

(defsubst ushell-complete-hostname ()
  "Complete a command that wants a hostname for an argument."
  (pcomplete-here (ushell-read-host-names)))

(defun ushell-complete-host-reference ()
  "If there is a host reference, complete it."
  (let ((arg (pcomplete-actual-arg))
	index)
    (when (setq index (string-match "@[a-z.]*\\'" arg))
      (setq pcomplete-stub (substring arg (1+ index))
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions (ushell-read-host-names)))))

(defalias 'pcomplete/ftp    'ushell-complete-hostname)
(defalias 'pcomplete/ncftp  'ushell-complete-hostname)
(defalias 'pcomplete/ping   'ushell-complete-hostname)
(defalias 'pcomplete/rlogin 'ushell-complete-hostname)

(defun pcomplete/telnet ()
  (require 'pcmpl-unix)
  (pcomplete-opt "xl(pcmpl-unix-user-names)")
  (ushell-complete-hostname))

(defun pcomplete/rsh ()
  "Complete `rsh', which, after the user and hostname, is like xargs."
  (require 'pcmpl-unix)
  (pcomplete-opt "l(pcmpl-unix-user-names)")
  (ushell-complete-hostname)
  (pcomplete-here (funcall pcomplete-command-completion-function))
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

(defalias 'pcomplete/ssh 'pcomplete/rsh)

(eval-when-compile
  (defvar block-size)
  (defvar by-bytes)
  (defvar dereference-links)
  (defvar grand-total)
  (defvar human-readable)
  (defvar max-depth)
  (defvar only-one-filesystem)
  (defvar show-all))

(defsubst ushell-du-size-string (size)
  (let* ((str (ushell-printable-size size human-readable block-size t))
	 (len (length str)))
    (concat str (if (< len 8)
		    (make-string (- 8 len) ? )))))

(defun ushell-du-sum-directory (path depth)
  "Summarize PATH, and its member directories."
  (let ((entries (ushell-directory-files-and-attributes path))
	(size 0.0))
    (while entries
      (unless (string-match "\\`\\.\\.?\\'" (caar entries))
	(let* ((entry (concat path (char-to-string directory-sep-char)
			      (caar entries)))
	       (symlink (and (stringp (cadr (car entries)))
			     (cadr (car entries)))))
	  (unless (or (and symlink (not dereference-links))
		      (and only-one-filesystem
			   (/= only-one-filesystem
			       (nth 12 (car entries)))))
	    (if symlink
		(setq entry symlink))
	    (setq size
		  (+ size
		     (if (eq t (cadr (car entries)))
			 (ushell-du-sum-directory entry (1+ depth))
		       (let ((file-size (nth 8 (car entries))))
			 (prog1
			     file-size
			   (if show-all
			       (ushell-print
				(concat (ushell-du-size-string file-size)
					entry "\n")))))))))))
      (setq entries (cdr entries)))
    (if (or (not max-depth)
	    (= depth max-depth)
	    (= depth 0))
	(ushell-print (concat (ushell-du-size-string size)
			      (directory-file-name path) "\n")))
    size))

(defun ushell/du (&rest args)
  "Implementation of \"du\" in Lisp, passing ARGS."
  (setq args (if args
		 (ushell-stringify-list (ushell-flatten-list args))
	       '(".")))
  (let ((ext-du (ushell-search-path "du")))
    (if (and ext-du
	     (not (catch 'have-ange-path
		    (ushell-for arg args
		      (if (eq (find-file-name-handler (expand-file-name arg)
						      'directory-files)
			      'ange-ftp-hook-function)
			  (throw 'have-ange-path t))))))
	(throw 'ushell-replace-command
	       (ushell-parse-command ext-du args))
      (ushell-eval-using-options
       "du" args
       '((?a "all" nil show-all
	     "write counts for all files, not just directories")
	 (nil "block-size" t block-size
	      "use SIZE-byte blocks (i.e., --block-size SIZE)")
	 (?b "bytes" nil by-bytes
	     "print size in bytes")
	 (?c "total" nil grand-total
	     "produce a grand total")
	 (?d "max-depth" t max-depth
	     "display data only this many levels of data")
	 (?h "human-readable" 1024 human-readable
	     "print sizes in human readable format")
	 (?H "is" 1000 human-readable
	     "likewise, but use powers of 1000 not 1024")
	 (?k "kilobytes" 1024 block-size
	     "like --block-size 1024")
	 (?L "dereference" nil dereference-links
	     "dereference all symbolic links")
	 (?m "megabytes" 1048576 block-size
	     "like --block-size 1048576")
	 (?s "summarize" 0 max-depth
	     "display only a total for each argument")
	 (?x "one-file-system" nil only-one-filesystem
	     "skip directories on different filesystems")
	 (nil "help" nil nil
	      "show this usage screen")
	 :external "du"
	 :usage "[OPTION]... FILE...
Summarize disk usage of each FILE, recursively for directories.")
       (unless by-bytes
	 (setq block-size (or block-size 1024)))
       (if (and max-depth (stringp max-depth))
	   (setq max-depth (string-to-int max-depth)))
       ;; filesystem support means nothing under Windows
       (if (ushell-under-windows-p)
	   (setq only-one-filesystem nil))
       (let ((size 0.0) ange-cache)
	 (while args
	   (if only-one-filesystem
	       (setq only-one-filesystem
		     (nth 11 (ushell-file-attributes
			      (file-name-as-directory (car args))))))
	   (setq size (+ size (ushell-du-sum-directory
			       (directory-file-name (car args)) 0)))
	   (setq args (cdr args)))
	 (if grand-total
	     (ushell-print (concat (ushell-du-size-string size)
				   "total\n"))))))))

(defvar ushell-time-start nil)

(defun ushell-show-elapsed-time ()
  (let ((elapsed (format "%.3f secs\n"
			 (- (ushell-time-to-seconds (current-time))
			    ushell-time-start))))
    (set-text-properties 0 (length elapsed) '(face bold) elapsed)
    (ushell-interactive-print elapsed))
  (remove-hook 'ushell-post-command-hook 'ushell-show-elapsed-time t))

(defun ushell/time (&rest args)
  "Implementation of \"time\" in Lisp."
  (let ((time-args (copy-alist args))
	(continue t)
	last-arg)
    (while (and continue args)
      (if (not (string-match "^-" (car args)))
	  (progn
	    (if last-arg
		(setcdr last-arg nil)
	      (setq args '("")))
	    (setq continue nil))
	(setq last-arg args
	      args (cdr args))))
    (ushell-eval-using-options
     "time" args
     '((?h "help" nil nil "show this usage screen")
       :external "time"
       :show-usage
       :usage "COMMAND...
Show wall-clock time elapsed during execution of COMMAND.")
     (setq ushell-time-start (ushell-time-to-seconds (current-time)))
     (add-hook 'ushell-post-command-hook 'ushell-show-elapsed-time nil t)
     ;; after setting
     (throw 'ushell-replace-command
	    (ushell-parse-command (car time-args) (cdr time-args))))))

(defalias 'ushell/whoami 'user-login-name)

(defvar ushell-diff-window-config nil)

(defun ushell-diff-quit ()
  "Restore the window configuration previous to diff'ing."
  (interactive)
  (if ushell-diff-window-config
      (set-window-configuration ushell-diff-window-config)))

(defun ushell/diff (&rest args)
  "Alias \"diff\" to call Emacs `diff' function."
  (let ((orig-args (ushell-stringify-list (ushell-flatten-list args))))
    (if (or ushell-plain-diff-behavior
	    (not (and (ushell-interactive-output-p)
		      (not ushell-in-pipeline-p)
		      (not ushell-in-subcommand-p))))
	(throw 'ushell-replace-command
	       (ushell-parse-command "*diff" orig-args))
      (setq args (ushell-copy-list orig-args))
      (if (< (length args) 2)
	  (throw 'ushell-replace-command
		 (ushell-parse-command "*diff" orig-args)))
      (let ((old (car (last args 2)))
	    (new (car (last args)))
	    (config (current-window-configuration)))
	(if (= (length args) 2)
	    (setq args nil)
	  (setcdr (last args 3) nil))
	(with-current-buffer
	    (condition-case err
		(diff old new (ushell-flatten-and-stringify args))
	      (error
	       (throw 'ushell-replace-command
		      (ushell-parse-command "*diff" orig-args))))
	  (when (fboundp 'diff-mode)
	    (make-local-variable 'compilation-finish-functions)
	    (add-hook
	     'compilation-finish-functions
	     `(lambda (buff msg)
		(with-current-buffer buff
		  (diff-mode)
		  (set (make-local-variable 'ushell-diff-window-config)
		       ,config)
		  (local-set-key [?q] 'ushell-diff-quit)
		  (if (fboundp 'turn-on-font-lock-if-enabled)
		      (turn-on-font-lock-if-enabled))
		  (goto-char (point-min))))))
	  (pop-to-buffer (current-buffer))))))
  nil)

(put 'ushell/diff 'ushell-no-numeric-conversions t)

(defun ushell/locate (&rest args)
  "Alias \"locate\" to call Emacs `locate' function."
  (if (or ushell-plain-locate-behavior
	  (not (and (ushell-interactive-output-p)
		    (not ushell-in-pipeline-p)
		    (not ushell-in-subcommand-p)))
	  (and (stringp (car args))
	       (string-match "^-" (car args))))
      (throw 'ushell-replace-command
	     (ushell-parse-command "*locate" (ushell-stringify-list
					      (ushell-flatten-list args))))
    (save-selected-window
      (let ((locate-history-list (list (car args))))
	(locate-with-filter (car args) (cadr args))))))

(put 'ushell/locate 'ushell-no-numeric-conversions t)

(defun ushell/occur (&rest args)
  "Alias \"occur\" to call Emacs `occur' function."
  (let ((inhibit-read-only t))
    (if (> (length args) 2)
	(error "usage: occur: (REGEXP &optional NLINES)")
      (apply 'occur args))))

(put 'ushell/occur 'usell-no-numeric-conversions t)

;;; Code:

;;; em-unix.el ends here
