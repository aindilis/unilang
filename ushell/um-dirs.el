;;; em-dirs.el --- directory navigation commands

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

(provide 'em-dirs)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-dirs nil
  "Directory navigation involves changing directories, examining the
current directory, maintaining a directory stack, and also keeping
track of a history of the last directory locations the user was in.
Emacs does provide standard Lisp definitions of `pwd' and `cd', but
they lack somewhat in feel from the typical shell equivalents."
  :tag "Directory navigation"
  :group 'ushell-module)

;;; Commentary:

;; The only special feature that Ushell offers in the last-dir-ring.
;; To view the ring, enter:
;;
;;   cd =
;;
;; Changing to an index within the ring is done using:
;;
;;   cd -      ; same as cd -0
;;   cd -4
;;
;; Or, it is possible to change the first member in the ring which
;; matches a regexp:
;;
;;   cd =bcc   ; change to the last directory visited containing "bcc"
;;
;; This ring is maintained automatically, and is persisted across
;; Ushell sessions.  It is a separate mechanism from `pushd' and
;; `popd', and the two may be used at the same time.

(require 'ring)
(require 'ush-opt)

;;; User Variables:

(defcustom ushell-dirs-load-hook '(ushell-dirs-initialize)
  "*A hook that gets run when `ushell-dirs' is loaded."
  :type 'hook
  :group 'ushell-dirs)

(defcustom ushell-pwd-convert-function (if (ushell-under-windows-p)
					   'expand-file-name
					 'identity)
  "*The function used to normalize the value of Ushell's `pwd'.
The value returned by `pwd' is also used when recording the
last-visited directory in the last-dir-ring, so it will affect the
form of the list used by 'cd ='."
  :type '(radio (function-item file-truename)
		(function-item expand-file-name)
		(function-item identity)
		(function :tag "Other"))
  :group 'ushell-dirs)

(defcustom ushell-ask-to-save-last-dir 'always
  "*Determine if the last-dir-ring should be automatically saved.
The last-dir-ring is always preserved when exiting an Ushell buffer.
However, when Emacs is being shut down, this variable determines
whether to prompt the user, or just save the ring.
If set to nil, it means never ask whether to save the last-dir-ring.
If set to t, always ask if any Ushell buffers are open at exit time.
If set to `always', the list-dir-ring will always be saved, silently."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" t)
		 (const :tag "Always save" always))
  :group 'ushell-dirs)

(defcustom ushell-cd-shows-directory nil
  "*If non-nil, using `cd' will report the directory it changes to."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-cd-on-directory t
  "*If non-nil, do a cd if a directory is in command position."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-directory-change-hook nil
  "*A hook to run when the current directory changes."
  :type 'hook
  :group 'ushell-dirs)

(defcustom ushell-list-files-after-cd nil
  "*If non-nil, call \"ls\" with any remaining args after doing a cd.
This is provided for convenience, since the same effect is easily
achieved by adding a function to `ushell-directory-change-hook' that
calls \"ls\" and references `ushell-last-arguments'."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-pushd-tohome nil
  "*If non-nil, make pushd with no arg behave as 'pushd ~' (like `cd').
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-pushd-dextract nil
  "*If non-nil, make \"pushd +n\" pop the nth dir to the stack top.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-pushd-dunique nil
  "*If non-nil, make pushd only add unique directories to the stack.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-dirtrack-verbose t
  "*If non-nil, show the directory stack following directory change.
This is effective only if directory tracking is enabled."
  :type 'boolean
  :group 'ushell-dirs)

(defcustom ushell-last-dir-ring-file-name
  (concat ushell-directory-name "lastdir")
  "*If non-nil, name of the file to read/write the last-dir-ring.
See also `ushell-read-last-dir-ring' and `ushell-write-last-dir-ring'.
If it is nil, the last-dir-ring will not be written to disk."
  :type 'file
  :group 'ushell-dirs)

(defcustom ushell-last-dir-ring-size 32
  "*If non-nil, the size of the directory history ring.
This ring is added to every time `cd' or `pushd' is used.  It simply
stores the most recent directory locations Ushell has been in.  To
return to the most recent entry, use 'cd -' (equivalent to 'cd -0').
To return to an older entry, use 'cd -N', where N is an integer less
than `ushell-last-dir-ring-size'.  To return to the last directory
matching a particular regexp, use 'cd =REGEXP'.  To display the
directory history list, use 'cd ='.

This mechanism is very similar to that provided by `pushd', except
it's far more automatic.  `pushd' allows the user to decide which
directories gets pushed, and its size is unlimited.

`ushell-last-dir-ring' is meant for users who don't use `pushd'
explicity very much, but every once in a while would like to return to
a previously visited directory without having to type in the whole
thing again."
  :type 'integer
  :group 'ushell-dirs)

(defcustom ushell-last-dir-unique t
  "*If non-nil, `ushell-last-dir-ring' contains only unique entries."
  :type 'boolean
  :group 'ushell-dirs)

;;; Internal  Variables:

(defvar ushell-dirstack nil
  "List of directories saved by pushd in the Ushell buffer.
Thus, this does not include the current directory.")

(defvar ushell-last-dir-ring nil
  "The last directory that ushell was in.")

;;; Functions:

(defun ushell-dirs-initialize ()
  "Initialize the builtin functions for Ushell."
  (make-local-variable 'ushell-variable-aliases-list)
  (setq ushell-variable-aliases-list
	(append
	 ushell-variable-aliases-list
	 '(("-" (lambda (indices)
		  (if (not indices)
		      (unless (ring-empty-p ushell-last-dir-ring)
			(expand-file-name
			 (ring-ref ushell-last-dir-ring 0)))
		    (expand-file-name
		     (ushell-apply-indices ushell-last-dir-ring indices)))))
	   ("+" "PWD")
	   ("PWD" (lambda (indices)
		    (expand-file-name (ushell/pwd))) t)
	   ("OLDPWD" (lambda (indices)
		       (unless (ring-empty-p ushell-last-dir-ring)
			 (expand-file-name
			  (ring-ref ushell-last-dir-ring 0)))) t))))

  (when ushell-cd-on-directory
    (make-local-variable 'ushell-interpreter-alist)
    (setq ushell-interpreter-alist
	  (cons (cons 'ushell-lone-directory-p
		      'ushell-dirs-substitute-cd)
		ushell-interpreter-alist)))

  (make-local-hook 'ushell-parse-argument-hook)
  (add-hook 'ushell-parse-argument-hook
	    'ushell-parse-user-reference nil t)
  (if (ushell-under-windows-p)
      (add-hook 'ushell-parse-argument-hook
		'ushell-parse-drive-letter nil t))

  (when (ushell-using-module 'ushell-cmpl)
    (make-local-hook 'pcomplete-try-first-hook)
    (add-hook 'pcomplete-try-first-hook
	      'ushell-complete-user-reference nil t))

  (make-local-variable 'ushell-dirstack)
  (make-local-variable 'ushell-last-dir-ring)

  (if ushell-last-dir-ring-file-name
      (ushell-read-last-dir-ring))
  (unless ushell-last-dir-ring
    (setq ushell-last-dir-ring (make-ring ushell-last-dir-ring-size)))

  (make-local-hook 'ushell-exit-hook)
  (add-hook 'ushell-exit-hook 'ushell-write-last-dir-ring nil t)

  (add-hook 'kill-emacs-hook 'ushell-save-some-last-dir))

(defun ushell-save-some-last-dir ()
  "Save the list-dir-ring for any open Ushell buffers."
  (ushell-for buf (buffer-list)
    (if (buffer-live-p buf)
	(with-current-buffer buf
	  (if (and ushell-mode
		   ushell-ask-to-save-last-dir
		   (or (eq ushell-ask-to-save-last-dir 'always)
		       (y-or-n-p
			(format "Save last dir ring for Ushell buffer `%s'? "
				(buffer-name buf)))))
	      (ushell-write-last-dir-ring))))))

(defun ushell-lone-directory-p (file)
  "Test whether FILE is just a directory name, and not a command name."
  (and (file-directory-p file)
       (or (file-name-directory file)
	   (not (ushell-search-path file)))))

(defun ushell-dirs-substitute-cd (&rest args)
  "Substitute the given command for a call to `cd' on that name."
  (if (> (length args) 1)
      (error "%s: command not found" (car args))
    (throw 'ushell-replace-command
	   (ushell-parse-command "cd" (ushell-flatten-list args)))))

(defun ushell-parse-user-reference ()
  "An argument beginning with ~ is a filename to be expanded."
  (when (and (not ushell-current-argument)
	     (eq (char-after) ?~))
    (add-to-list 'ushell-current-modifiers 'expand-file-name)
    (forward-char)
    (char-to-string (char-before))))

(defun ushell-parse-drive-letter ()
  "An argument beginning X:[^/] is a drive letter reference."
  (when (and (not ushell-current-argument)
	     (looking-at "\\([A-Za-z]:\\)\\([^/\\\\]\\|\\'\\)"))
    (goto-char (match-end 1))
    (let* ((letter (match-string 1))
	   (regexp (concat "\\`" letter))
	   (path (ushell-find-previous-directory regexp)))
      (concat (or path letter)
	      (char-to-string directory-sep-char)))))

(defun ushell-complete-user-reference ()
  "If there is a user reference, complete it."
  (let ((arg (pcomplete-actual-arg)))
    (when (string-match "\\`~[a-z]*\\'" arg)
      (setq pcomplete-stub (substring arg 1)
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions
	     (progn
	       (ushell-read-user-names)
	       (pcomplete-uniqify-list
		(mapcar
		 (function
		  (lambda (user)
		    (file-name-as-directory (cdr user))))
		 ushell-user-names)))))))

(defun ushell/pwd (&rest args)
  "Change output from `pwd` to be cleaner."
  (let* ((path default-directory)
	 (len (length path)))
    (if (and (> len 1)
	     (eq (aref path (1- len)) directory-sep-char)
	     (not (and (ushell-under-windows-p)
		       (string-match "\\`[A-Za-z]:[\\\\/]\\'" path))))
	(setq path (substring path 0 (1- (length path)))))
    (if ushell-pwd-convert-function
	(funcall ushell-pwd-convert-function path)
      path)))

(defun ushell-expand-multiple-dots (path)
  "Convert '...' to '../..', '....' to '../../..', etc..

With the following piece of advice, you can make this functionality
available in most of Emacs, with the exception of filename completion
in the minibuffer:

  (defadvice expand-file-name
    (before translate-multiple-dots
	    (filename &optional directory) activate)
    (setq filename (ushell-expand-multiple-dots filename)))"
  (while (string-match "\\.\\.\\(\\.+\\)" path)
    (let* ((extra-dots (match-string 1 path))
	   (len (length extra-dots))
	   replace-text)
      (while (> len 0)
	(setq replace-text
	      (concat replace-text
		      (char-to-string directory-sep-char) "..")
	      len (1- len)))
      (setq path
	    (replace-match replace-text t t path 1))))
  path)

(defun ushell-find-previous-directory (regexp)
  "Find the most recent last-dir matching REGEXP."
  (let ((index 0)
	(len (ring-length ushell-last-dir-ring))
	oldpath)
    (if (> (length regexp) 0)
	(while (< index len)
	  (setq oldpath (ring-ref ushell-last-dir-ring index))
	  (if (string-match regexp oldpath)
	      (setq index len)
	    (setq oldpath nil
		  index (1+ index)))))
    oldpath))

(eval-when-compile
  (defvar dired-directory))

(defun ushell/cd (&rest args)           ; all but first ignored
  "Alias to extend the behavior of `cd'."
  (setq args (ushell-flatten-list args))
  (let ((path (car args))
	(subpath (car (cdr args)))
	(case-fold-search (ushell-under-windows-p))
	handled)
    (if (numberp path)
	(setq path (number-to-string path)))
    (if (numberp subpath)
	(setq subpath (number-to-string subpath)))
    (cond
     (subpath
      (let ((curdir (ushell/pwd)))
	(if (string-match path curdir)
	    (setq path (replace-match subpath nil nil curdir))
	  (error "Path substring '%s' not found" path))))
     ((and path (string-match "^-\\([0-9]*\\)$" path))
      (let ((index (match-string 1 path)))
	(setq path
	      (ring-remove ushell-last-dir-ring
			   (if index
			       (string-to-int index)
			     0)))))
     ((and path (string-match "^=\\(.*\\)$" path))
      (let ((oldpath (ushell-find-previous-directory
		      (match-string 1 path))))
	(if oldpath
	    (setq path oldpath)
	  (let ((len (ring-length ushell-last-dir-ring))
		(index 0))
	    (if (= len 0)
		(error "Directory ring empty"))
	    (ushell-init-print-buffer)
	    (while (< index len)
	      (ushell-buffered-print
	       (concat (number-to-string index) ": "
		       (ring-ref ushell-last-dir-ring index) "\n"))
	      (setq index (1+ index)))
	    (ushell-flush)
	    (setq handled t)))))
     (path
      (setq path (ushell-expand-multiple-dots path))))
    (unless handled
      (setq dired-directory (or path "~"))
      (let ((curdir (ushell/pwd)))
	(unless (equal curdir dired-directory)
	  (ushell-add-to-dir-ring curdir))
	(let ((result (cd dired-directory)))
	  (and ushell-cd-shows-directory
	       (ushell-printn result)))
	(run-hooks 'ushell-directory-change-hook)
	(if ushell-list-files-after-cd
	    (throw 'ushell-replace-command
		   (ushell-parse-command "ls" (cdr args))))
	nil))))

(put 'ushell/cd 'ushell-no-numeric-conversions t)

(defun ushell-add-to-dir-ring (path)
  "Add PATH to the last-dir-ring, if applicable."
  (unless (and (not (ring-empty-p ushell-last-dir-ring))
	       (equal path (ring-ref ushell-last-dir-ring 0)))
    (if ushell-last-dir-unique
	(let ((index 0)
	      (len (ring-length ushell-last-dir-ring)))
	  (while (< index len)
	    (if (equal (ring-ref ushell-last-dir-ring index) path)
		(ring-remove ushell-last-dir-ring index)
	      (setq index (1+ index))))))
    (ring-insert ushell-last-dir-ring path)))

;;; pushd [+n | dir]
(defun ushell/pushd (&rest args)        ; all but first ignored
  "Implementation of pushd in Lisp."
  (let ((path (car args)))
    (cond
     ((null path)
      ;; no arg -- swap pwd and car of stack unless ushell-pushd-tohome
      (cond (ushell-pushd-tohome
	     (ushell/pushd "~"))
	    (ushell-dirstack
	     (let ((old (ushell/pwd)))
	       (ushell/cd (car ushell-dirstack))
	       (setq ushell-dirstack (cons old (cdr ushell-dirstack)))
	       (ushell/dirs t)))
	    (t
	     (error "pushd: No other directory"))))
     ((string-match "^\\+\\([0-9]\\)" path)
      ;; pushd +n
      (setq path (string-to-number (match-string 1 path)))
      (cond ((> path (length ushell-dirstack))
	     (error "Directory stack not that deep"))
	    ((= path 0)
	     (error "Couldn't cd"))
	    (ushell-pushd-dextract
	     (let ((dir (nth (1- path) ushell-dirstack)))
	       (ushell/popd path)
	       (ushell/pushd (ushell/pwd))
	       (ushell/cd dir)
	       (ushell/dirs t)))
	    (t
	     (let* ((ds (cons (ushell/pwd) ushell-dirstack))
		    (dslen (length ds))
		    (front (nthcdr path ds))
		    (back (nreverse (nthcdr (- dslen path) (reverse ds))))
		    (new-ds (append front back)))
	       (ushell/cd (car new-ds))
	       (setq ushell-dirstack (cdr new-ds))
	       (ushell/dirs t)))))
     (t
      ;; pushd <dir>
      (let ((old-wd (ushell/pwd)))
	(ushell/cd path)
	(if (or (null ushell-pushd-dunique)
		(not (member old-wd ushell-dirstack)))
	    (setq ushell-dirstack (cons old-wd ushell-dirstack)))
	(ushell/dirs t)))))
  nil)

(put 'ushell/pushd 'ushell-no-numeric-conversions t)

;;; popd [+n]
(defun ushell/popd (&rest args)
  "Implementation of popd in Lisp."
  (let ((ref (or (car args) "+0")))
    (unless (and (stringp ref)
		 (string-match "\\`\\([+-][0-9]+\\)\\'" ref))
      (error "popd: bad arg `%s'" ref))
    (setq ref (string-to-number (match-string 1 ref)))
    (cond ((= ref 0)
	   (unless ushell-dirstack
	     (error "popd: Directory stack empty"))
	   (ushell/cd (car ushell-dirstack))
	   (setq ushell-dirstack (cdr ushell-dirstack))
	   (ushell/dirs t))
	  ((<= (abs ref) (length ushell-dirstack))
	   (let* ((ds (cons nil ushell-dirstack))
		  (cell (nthcdr (if (> ref 0)
				    (1- ref)
				  (+ (length ushell-dirstack) ref)) ds))
		  (dir (cadr cell)))
	     (ushell/cd dir)
	     (setcdr cell (cdr (cdr cell)))
	     (setq ushell-dirstack (cdr ds))
	     (ushell/dirs t)))
	  (t
	   (error "Couldn't popd"))))
  nil)

(put 'ushell/popd 'ushell-no-numeric-conversions t)

(defun ushell/dirs (&optional if-verbose)
  "Implementation of dirs in Lisp."
  (when (or (not if-verbose) ushell-dirtrack-verbose)
    (let* ((msg "")
	   (ds (cons (ushell/pwd) ushell-dirstack))
	   (home (expand-file-name "~/"))
	   (homelen (length home)))
      (while ds
	(let ((dir (car ds)))
	  (and (>= (length dir) homelen)
	       (string= home (substring dir 0 homelen))
	       (setq dir (concat "~/" (substring dir homelen))))
	  (setq msg (concat msg (directory-file-name dir) " "))
	  (setq ds (cdr ds))))
      msg)))

(defun ushell-read-last-dir-ring ()
  "Sets the buffer's `ushell-last-dir-ring' from a history file."
  (let ((file ushell-last-dir-ring-file-name))
    (cond
     ((or (null file)
	  (equal file "")
	  (not (file-readable-p file)))
      nil)
     (t
      (let* ((count 0)
	     (size ushell-last-dir-ring-size)
	     (ring (make-ring size)))
	(with-temp-buffer
	  (insert-file-contents file)
	  ;; Save restriction in case file is already visited...
	  ;; Watch for those date stamps in history files!
	  (goto-char (point-max))
	  (while (and (< count size)
		      (re-search-backward "^\\([^\n].*\\)$" nil t))
	    (ring-insert-at-beginning ring (match-string 1))
	    (setq count (1+ count)))
	  ;; never allow the top element to equal the current
	  ;; directory
	  (while (and (not (ring-empty-p ring))
		      (equal (ring-ref ring 0) (ushell/pwd)))
	    (ring-remove ring 0)))
	(setq ushell-last-dir-ring ring))))))

(defun ushell-write-last-dir-ring ()
  "Writes the buffer's `ushell-last-dir-ring' to a history file."
  (let ((file ushell-last-dir-ring-file-name))
    (cond
     ((or (null file)
	  (equal file "")
	  (null ushell-last-dir-ring)
	  (ring-empty-p ushell-last-dir-ring))
      nil)
     ((not (file-writable-p file))
      (message "Cannot write last-dir-ring file %s" file))
     (t
      (let* ((ring ushell-last-dir-ring)
	     (index (ring-length ring)))
	(with-temp-buffer
	  (while (> index 0)
	    (setq index (1- index))
	    (insert (ring-ref ring index) ?\n))
	  (insert (ushell/pwd) ?\n)
	  (usell-with-private-file-modes
	   (write-region (point-min) (point-max) file nil
			 'no-message))))))))

;;; Code:

;;; em-dirs.el ends here
