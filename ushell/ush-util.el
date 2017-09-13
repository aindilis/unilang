;;; ush-util.el --- general utilities

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

(provide 'ush-util)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-util nil
  "This is general utility code, meant for use by Ushell itself."
  :tag "General utilities"
  :group 'ushell)

;;; Commentary:

(require 'pp)

;;; User Variables:

(defcustom ushell-stringify-t t
  "*If non-nil, the string representation of t is 't'.
If nil, t will be represented only in the exit code of the function,
and not printed as a string.  This causes Lisp functions to behave
similarly to external commands, as far as successful result output."
  :type 'boolean
  :group 'ushell-util)

(defcustom ushell-group-file "/etc/group"
  "*If non-nil, the name of the group file on your system."
  :type '(choice (const :tag "No group file" nil) file)
  :group 'ushell-util)

(defcustom ushell-passwd-file "/etc/passwd"
  "*If non-nil, the name of the passwd file on your system."
  :type '(choice (const :tag "No passwd file" nil) file)
  :group 'ushell-util)

(defcustom ushell-hosts-file "/etc/hosts"
  "*The name of the /etc/hosts file."
  :type '(choice (const :tag "No hosts file" nil) file)
  :group 'ushell-util)

(defcustom ushell-handle-errors t
  "*If non-nil, Ushell will handle errors itself.
Setting this to nil is offered as an aid to debugging only."
  :type 'boolean
  :group 'ushell-util)

(defcustom ushell-private-file-modes 384 ; umask 177
  "*The file-modes value to use for creating \"private\" files."
  :type 'integer
  :group 'ushell-util)

(defcustom ushell-private-directory-modes 448 ; umask 077
  "*The file-modes value to use for creating \"private\" directories."
  :type 'integer
  :group 'ushell-util)

(defcustom ushell-tar-regexp
  "\\.t\\(ar\\(\\.\\(gz\\|bz2\\|Z\\)\\)?\\|gz\\|a[zZ]\\|z2\\)\\'"
  "*Regular expression used to match tar file names."
  :type 'regexp
  :group 'ushell-util)

(defcustom ushell-convert-numeric-arguments t
  "*If non-nil, converting arguments of numeric form to Lisp numbers.
Numeric form is tested using the regular expression
`ushell-number-regexp'.

NOTE: If you find that numeric conversions are intefering with the
specification of filenames (for example, in calling `find-file', or
some other Lisp function that deals with files, not numbers), add the
following in your .emacs file:

  (put 'find-file 'ushell-no-numeric-conversions t)

Any function with the property `ushell-no-numeric-conversions' set to
a non-nil value, will be passed strings, not numbers, even when an
argument matches `ushell-number-regexp'."
  :type 'boolean
  :group 'ushell-util)

(defcustom ushell-number-regexp "-?\\([0-9]*\\.\\)?[0-9]+\\(e[-0-9.]+\\)?"
  "*Regular expression used to match numeric arguments.
If `ushell-convert-numeric-arguments' is non-nil, and an argument
matches this regexp, it will be converted to a Lisp number, using the
function `string-to-number'."
  :type 'regexp
  :group 'ushell-util)

(defcustom ushell-ange-ls-uids nil
  "*List of user/host/id strings, used to determine remote ownership."
  :type '(repeat (cons :tag "Host for User/UID map"
		       (string :tag "Hostname")
		       (repeat (cons :tag "User/UID List"
				     (string :tag "Username")
				     (repeat :tag "UIDs" string)))))
  :group 'ushell-util)

;;; Internal Variables:

(defvar ushell-group-names nil
  "A cache to hold the names of groups.")

(defvar ushell-group-timestamp nil
  "A timestamp of when the group file was read.")

(defvar ushell-user-names nil
  "A cache to hold the names of users.")

(defvar ushell-user-timestamp nil
  "A timestamp of when the user file was read.")

(defvar ushell-host-names nil
  "A cache the names of frequently accessed hosts.")

(defvar ushell-host-timestamp nil
  "A timestamp of when the hosts file was read.")

;;; Functions:

(defsubst ushell-under-xemacs-p ()
  "Return non-nil if we are running under XEmacs."
  (boundp 'xemacs-logo))

(defsubst ushell-under-windows-p ()
  "Return non-nil if we are running under MS-DOS/Windows."
  (memq system-type '(ms-dos windows-nt)))

(defmacro ushell-condition-case (tag form &rest handlers)
  "Like `condition-case', but only if `ushell-pass-through-errors' is nil."
  (if ushell-handle-errors
      `(condition-case ,tag
	   ,form
	 ,@handlers)
    form))

(put 'ushell-condition-case 'lisp-indent-function 2)

(defmacro ushell-deftest (module name label &rest forms)
  (if (and (fboundp 'cl-compiling-file) (cl-compiling-file))
      nil
    (let ((fsym (intern (concat "ushell-test--" (symbol-name name)))))
      `(eval-when-compile
	 (ignore
	  (defun ,fsym () ,label
	    (ushell-run-test (quote ,module) (quote ,fsym) ,label
			     (quote (progn ,@forms)))))))))

(put 'ushell-deftest 'lisp-indent-function 2)

(defun ushell-find-delimiter
  (open close &optional bound reverse-p backslash-p)
  "From point, find the CLOSE delimiter corresponding to OPEN.
The matching is bounded by BOUND.
If REVERSE-P is non-nil, process the region backwards.
If BACKSLASH-P is non-nil, and OPEN and CLOSE are the same character,
then quoting is done by a backslash, rather than a doubled delimiter."
  (save-excursion
    (let ((depth 1)
	  (bound (or bound (point-max))))
      (if (if reverse-p
	      (eq (char-before) close)
	    (eq (char-after) open))
	  (forward-char (if reverse-p -1 1)))
      (while (and (> depth 0)
		  (funcall (if reverse-p '> '<) (point) bound))
	(let ((c (if reverse-p (char-before) (char-after))) nc)
	  (cond ((and (not reverse-p)
		      (or (not (eq open close))
			  backslash-p)
		      (eq c ?\\)
		      (setq nc (char-after (1+ (point))))
		      (or (eq nc open) (eq nc close)))
		 (forward-char 1))
		((and reverse-p
		      (or (not (eq open close))
			  backslash-p)
		      (or (eq c open) (eq c close))
		      (eq (char-before (1- (point)))
			  ?\\))
		 (forward-char -1))
		((eq open close)
		 (if (eq c open)
		     (if (and (not backslash-p)
			      (eq (if reverse-p
				      (char-before (1- (point)))
				    (char-after (1+ (point)))) open))
			 (forward-char (if reverse-p -1 1))
		       (setq depth (1- depth)))))
		((= c open)
		 (setq depth (+ depth (if reverse-p -1 1))))
		((= c close)
		 (setq depth (+ depth (if reverse-p 1 -1))))))
	(forward-char (if reverse-p -1 1)))
      (if (= depth 0)
	  (if reverse-p (point) (1- (point)))))))

(defun ushell-convert (string)
  "Convert STRING into a more native looking Lisp object."
  (if (not (stringp string))
      string
    (let ((len (length string)))
      (if (= len 0)
	  string
	(if (eq (aref string (1- len)) ?\n)
	    (setq string (substring string 0 (1- len))))
	(if (string-match "\n" string)
	    (split-string string "\n")
	  (if (and ushell-convert-numeric-arguments
		   (string-match
		    (concat "\\`\\s-*" ushell-number-regexp
			    "\\s-*\\'") string))
	      (string-to-number string)
	    string))))))

(defun ushell-sublist (l &optional n m)
  "Return from LIST the N to M elements.
If N or M is nil, it means the end of the list."
  (let* ((a (ushell-copy-list l))
	 result)
    (if (and m (consp (nthcdr m a)))
	(setcdr (nthcdr m a) nil))
    (if n
	(setq a (nthcdr n a))
      (setq n (1- (length a))
	    a (last a)))
    a))

(defun ushell-split-path (path)
  "Split a path into multiple subparts."
  (let ((len (length path))
	(i 0) (li 0)
	parts)
    (if (and (ushell-under-windows-p)
	     (> len 2)
	     (eq (aref path 0) directory-sep-char)
	     (eq (aref path 1) directory-sep-char))
	(setq i 2))
    (while (< i len)
      (if (and (eq (aref path i) directory-sep-char)
	       (not (get-text-property i 'escaped path)))
	  (setq parts (cons (if (= li i)
				(char-to-string directory-sep-char)
			      (substring path li (1+ i))) parts)
		li (1+ i)))
      (setq i (1+ i)))
    (if (< li i)
	(setq parts (cons (substring path li i) parts)))
    (if (and (ushell-under-windows-p)
	     (string-match "\\`[A-Za-z]:\\'" (car (last parts))))
	(setcar (last parts)
		(concat (car (last parts))
			(char-to-string directory-sep-char))))
    (nreverse parts)))

(defun ushell-to-flat-string (value)
  "Make value a string.  If separated by newlines change them to spaces."
  (let ((text (ushell-stringify value)))
    (if (string-match "\n+\\'" text)
	(setq text (replace-match "" t t text)))
    (while (string-match "\n+" text)
      (setq text (replace-match " " t t text)))
    text))

(defmacro ushell-for (for-var for-list &rest forms)
  "Iterate through a list"
  `(let ((list-iter ,for-list))
     (while list-iter
       (let ((,for-var (car list-iter)))
	 ,@forms)
       (setq list-iter (cdr list-iter)))))

(put 'ushell-for 'lisp-indent-function 2)

(defun ushell-flatten-list (args)
  "Flatten any lists within ARGS, so that there are no sublists."
  (let ((new-list (list t)))
    (ushell-for a args
      (if (and (listp a)
	       (listp (cdr a)))
	  (nconc new-list (ushell-flatten-list a))
	(nconc new-list (list a))))
    (cdr new-list)))

(defun ushell-uniqify-list (l)
  "Remove occurring multiples in L.  You probably want to sort first."
  (let ((m l))
    (while m
      (while (and (cdr m)
		  (string= (car m)
			   (cadr m)))
	(setcdr m (cddr m)))
      (setq m (cdr m))))
  l)

(defun ushell-stringify (object)
  "Convert OBJECT into a string value."
  (cond
   ((stringp object) object)
   ((and (listp object)
	 (not (eq object nil)))
    (let ((string (pp-to-string object)))
      (substring string 0 (1- (length string)))))
   ((numberp object)
    (number-to-string object))
   (t
    (unless (and (eq object t)
		 (not ushell-stringify-t))
      (pp-to-string object)))))

(defsubst ushell-stringify-list (args)
  "Convert each element of ARGS into a string value."
  (mapcar 'ushell-stringify args))

(defsubst ushell-flatten-and-stringify (&rest args)
  "Flatten and stringify all of the ARGS into a single string."
  (mapconcat 'ushell-stringify (ushell-flatten-list args) " "))

;; the next two are from GNUS, and really should be made part of Emacs
;; some day
(defsubst ushell-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defsubst ushell-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst ushell-directory-files (regexp &optional directory)
  "Return a list of files in the given DIRECTORY matching REGEXP."
  (directory-files (or directory default-directory)
		   directory regexp))

(defun ushell-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    (list (if (string-equal regexp "")
	      (setcar minibuffer-history-search-history
		      (nth 1 minibuffer-history-search-history))
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun ushell-printable-size (filesize &optional human-readable
				       block-size use-colors)
  "Return a printable FILESIZE."
  (let ((size (float (or filesize 0))))
    (if human-readable
	(if (< size human-readable)
	    (if (= (round size) 0)
		"0"
	      (if block-size
		  "1.0k"
		(format "%.0f" size)))
	  (setq size (/ size human-readable))
	  (if (< size human-readable)
	      (if (<= size 9.94)
		  (format "%.1fk" size)
		(format "%.0fk" size))
	    (setq size (/ size human-readable))
	    (if (< size human-readable)
		(let ((str (if (<= size 9.94)
			       (format "%.1fM" size)
			     (format "%.0fM" size))))
		  (if use-colors
		      (put-text-property 0 (length str)
					 'face 'bold str))
		  str)
	      (setq size (/ size human-readable))
	      (if (< size human-readable)
		  (let ((str (if (<= size 9.94)
				 (format "%.1fG" size)
			       (format "%.0fG" size))))
		    (if use-colors
			(put-text-property 0 (length str)
					   'face 'bold-italic str))
		    str)))))
      (if block-size
	  (setq size (/ size block-size)))
      (format "%.0f" size))))

(defun ushell-winnow-list (entries exclude &optional predicates)
  "Pare down the ENTRIES list using the EXCLUDE regexp, and PREDICATES.
The original list is not affected.  If the result is only one element
long, it will be returned itself, rather than returning a one-element
list."
  (let ((flist (list t))
	valid p listified)
    (unless (listp entries)
      (setq entries (list entries)
	    listified t))
    (ushell-for entry entries
      (unless (and exclude (string-match exclude entry))
	(setq p predicates valid (null p))
	(while p
	  (if (funcall (car p) entry)
	      (setq valid t)
	    (setq p nil valid nil))
	  (setq p (cdr p)))
	(when valid
	  (nconc flist (list entry)))))
    (if listified
	(cadr flist)
      (cdr flist))))

(defsubst ushell-redisplay ()
  "Allow Emacs to redisplay buffers."
  ;; for some strange reason, Emacs 21 is prone to trigger an
  ;; "args out of range" error in `sit-for', if this function
  ;; runs while point is in the minibuffer and the users attempt
  ;; to use completion.  Don't ask me.
  (ignore-errors (sit-for 0 0)))

(defun ushell-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE."
  (let (names)
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((fields
		  (split-string (buffer-substring
				 (point) (progn (end-of-line)
						(point))) ":")))
	    (if (and (and fields (nth 0 fields) (nth 2 fields))
		     (not (assq (string-to-int (nth 2 fields)) names)))
		(setq names (cons (cons (string-to-int (nth 2 fields))
					(nth 0 fields))
				  names))))
	  (forward-line))))
    names))

(defun ushell-read-passwd (file result-var timestamp-var)
  "Read the contents of /etc/passwd for user names."
  (if (or (not (symbol-value result-var))
	  (not (symbol-value timestamp-var))
	  (ushell-time-less-p
	   (symbol-value timestamp-var)
	   (nth 5 (file-attributes file))))
      (progn
	(set result-var (ushell-read-passwd-file file))
	(set timestamp-var (current-time))))
  (symbol-value result-var))

(defun ushell-read-group-names ()
  "Read the contents of /etc/group for group names."
  (if ushell-group-file
      (ushell-read-passwd ushell-group-file 'ushell-group-names
			  'ushell-group-timestamp)))

(defsubst ushell-group-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (ushell-read-group-names))))

(defsubst ushell-group-name (gid)
  "Return the group name for the given GID."
  (cdr (assoc gid (ushell-read-group-names))))

(defun ushell-read-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if ushell-passwd-file
      (ushell-read-passwd ushell-passwd-file 'ushell-user-names
			  'ushell-user-timestamp)))

(defsubst ushell-user-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (ushell-read-user-names))))

(defalias 'ushell-user-name 'user-login-name)

(defun ushell-read-hosts-file (filename)
  "Read in the hosts from the /etc/hosts file."
  (let (hosts)
    (with-temp-buffer
      (insert-file-contents ushell-hosts-file)
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\(\\s-*\\(\\S-+\\)\\)?" nil t)
	(if (match-string 1)
	    (add-to-list 'hosts (match-string 1)))
	(if (match-string 2)
	    (add-to-list 'hosts (match-string 2)))
	(if (match-string 4)
	    (add-to-list 'hosts (match-string 4)))))
    (sort hosts 'string-lessp)))

(defun ushell-read-hosts (file result-var timestamp-var)
  "Read the contents of /etc/passwd for user names."
  (if (or (not (symbol-value result-var))
	  (not (symbol-value timestamp-var))
	  (ushell-time-less-p
	   (symbol-value timestamp-var)
	   (nth 5 (file-attributes file))))
      (progn
	(set result-var (ushell-read-hosts-file file))
	(set timestamp-var (current-time))))
  (symbol-value result-var))

(defun ushell-read-host-names ()
  "Read the contents of /etc/hosts for host names."
  (if ushell-hosts-file
      (ushell-read-hosts ushell-hosts-file 'ushell-host-names
			 'ushell-host-timestamp)))

(unless (fboundp 'line-end-position)
  (defsubst line-end-position (&optional N)
    (save-excursion (end-of-line N) (point))))

(unless (fboundp 'line-beginning-position)
  (defsubst line-beginning-position (&optional N)
    (save-excursion (beginning-of-line N) (point))))

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
	(setq i (1- i))
	(if (eq (aref newstr i) fromchar)
	    (aset newstr i tochar)))
      newstr)))

(defsubst ushell-copy-environment ()
  "Return an unrelated copy of `process-environment'."
  (mapcar 'concat process-environment))

(defun ushell-subgroups (groupsym)
  "Return all of the subgroups of GROUPSYM."
  (let ((subgroups (get groupsym 'custom-group))
	(subg (list t)))
    (while subgroups
      (if (eq (cadr (car subgroups)) 'custom-group)
	  (nconc subg (list (caar subgroups))))
      (setq subgroups (cdr subgroups)))
    (cdr subg)))

(defmacro ushell-with-file-modes (modes &rest forms)
  "Evaluate, with file-modes set to MODES, the given FORMS."
  `(let ((modes (default-file-modes)))
     (set-default-file-modes ,modes)
     (unwind-protect
	 (progn ,@forms)
       (set-default-file-modes modes))))

(defmacro ushell-with-private-file-modes (&rest forms)
  "Evaluate FORMS with private file modes set."
  `(ushell-with-file-modes ,ushell-private-file-modes ,@forms))

(defsubst ushell-make-private-directory (dir &optional parents)
  "Make DIR with file-modes set to `ushell-private-directory-modes'."
  (ushell-with-file-modes ushell-private-directory-modes
			  (make-directory dir parents)))

(defsubst ushell-substring (string sublen)
  "Return the beginning of STRING, up to SUBLEN bytes."
  (if string
      (if (> (length string) sublen)
	  (substring string 0 sublen)
	string)))

(unless (fboundp 'directory-files-and-attributes)
  (defun directory-files-and-attributes (dir &optional full match nosort)
    (documentation 'directory-files)
    (let ((dir (expand-file-name dir)) ange-cache)
      (mapcar
       (function
	(lambda (file)
	  (cons file (ushell-file-attributes (expand-file-name file dir)))))
       (directory-files dir full match nosort)))))

(eval-when-compile
  (defvar ange-cache))

(defun ushell-directory-files-and-attributes (dir &optional full match nosort)
  "Make sure to use the handler for `directory-file-and-attributes'."
  (let* ((dir (expand-file-name dir))
	 (dfh (find-file-name-handler dir 'directory-files)))
    (if (not dfh)
	(directory-files-and-attributes dir full match nosort)
      (let ((files (funcall dfh 'directory-files dir full match nosort))
	    (fah (find-file-name-handler dir 'file-attributes)))
	(mapcar
	 (function
	  (lambda (file)
	    (cons file (if fah
			   (ushell-file-attributes
			    (expand-file-name file dir))
			 (file-attributes (expand-file-name file dir))))))
	 files)))))

(defun ushell-current-ange-uids ()
  (if (string-match "/\\([^@]+\\)@\\([^:]+\\):" default-directory)
      (let* ((host (match-string 2 default-directory))
	     (user (match-string 1 default-directory))
	     (host-users (assoc host ushell-ange-ls-uids)))
	(when host-users
	  (setq host-users (cdr host-users))
	  (cdr (assoc user host-users))))))

;; Add an autoload for parse-time-string
(if (and (not (fboundp 'parse-time-string))
	 (locate-library "parse-time"))
    (autoload 'parse-time-string "parse-time"))

(eval-when-compile
  (load "ange-ftp" t))

(defun ushell-parse-ange-ls (dir)
  (let (entry)
    (with-temp-buffer
      (insert (ange-ftp-ls dir "-la" nil))
      (goto-char (point-min))
      (if (looking-at "^total [0-9]+$")
	  (forward-line 1))
      ;; Some systems put in a blank line here.
      (if (eolp) (forward-line 1))
      (while (looking-at
	      `,(concat "\\([dlscb-][rwxst-]+\\)"
			"\\s-*" "\\([0-9]+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\([0-9]+\\)" "\\s-+" "\\(.*\\)"))
	(let* ((perms (match-string 1))
	       (links (string-to-number (match-string 2)))
	       (user (match-string 3))
	       (group (match-string 4))
	       (size (string-to-number (match-string 5)))
	       (mtime
		(if (fboundp 'parse-time-string)
		    (let ((moment (parse-time-string
				   (match-string 6))))
		      (if (nth 0 moment)
			  (setcar (nthcdr 5 moment)
				  (nth 5 (decode-time (current-time))))
			(setcar (nthcdr 0 moment) 0)
			(setcar (nthcdr 1 moment) 0)
			(setcar (nthcdr 2 moment) 0))
		      (apply 'encode-time moment))
		  (ange-ftp-file-modtime (expand-file-name name dir))))
	       (name (ange-ftp-parse-filename))
	       symlink)
	  (if (string-match "\\(.+\\) -> \\(.+\\)" name)
	      (setq symlink (match-string 2 name)
		    name (match-string 1 name)))
	  (setq entry
		(cons
		 (cons name
		       (list (if (eq (aref perms 0) ?d)
				 t
			       symlink)
			     links user group
			     nil mtime nil
			     size perms nil nil)) entry)))
	(forward-line)))
    entry))

(defun ushell-file-attributes (file)
  "Return the attributes of FILE, playing tricks if it's over ange-ftp."
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'file-attributes))
	 entry)
    (if (not handler)
	(file-attributes file)
      (if (eq (find-file-name-handler (file-name-directory file)
				      'directory-files)
	      'ange-ftp-hook-function)
	  (let ((base (file-name-nondirectory file))
		(dir (file-name-directory file)))
	    (if (boundp 'ange-cache)
		(setq entry (cdr (assoc base (cdr (assoc dir ange-cache))))))
	    (unless entry
	      (setq entry (ushell-parse-ange-ls dir))
	      (if (boundp 'ange-cache)
		  (setq ange-cache
			(cons (cons dir entry)
			      ange-cache)))
	      (if entry
		  (let ((fentry (assoc base (cdr entry))))
		    (if fentry
			(setq entry (cdr fentry))
		      (setq entry nil)))))))
      (or entry (funcall handler 'file-attributes file)))))

(defun ushell-copy-list (list)
  "Return a copy of a list, which may be a dotted list.
The elements of the list are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))

(defun ushell-copy-tree (tree &optional vecp)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
Contrast to copy-sequence, which copies only along the cdrs.  With second
argument VECP, this copies vectors as well as conses."
  (if (consp tree)
      (let ((p (setq tree (ushell-copy-list tree))))
	(while (consp p)
	  (if (or (consp (car p)) (and vecp (vectorp (car p))))
	      (setcar p (ushell-copy-tree (car p) vecp)))
	  (or (listp (cdr p)) (setcdr p (ushell-copy-tree (cdr p) vecp)))
	  (cl-pop p)))
    (if (and vecp (vectorp tree))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (ushell-copy-tree (aref tree i) vecp))))))
  tree)

(defsubst ushell-processp (proc)
  "If the `processp' function does not exist, PROC is not a process."
  (and (fboundp 'processp) (processp proc)))

; (defun ushell-copy-file
;   (file newname &optional ok-if-already-exists keep-date)
;   "Copy FILE to NEWNAME.  See docs for `copy-file'."
;   (let (copied)
;     (if (string-match "\\`\\([^:]+\\):\\(.*\\)" file)
;	(let ((front (match-string 1 file))
;	      (back (match-string 2 file))
;	      buffer)
;	  (if (and front (string-match ushell-tar-regexp front)
;		     (setq buffer (find-file-noselect front)))
;	    (with-current-buffer buffer
;	      (goto-char (point-min))
;	      (if (re-search-forward (concat " " (regexp-quote back)
;					     "$") nil t)
;		  (progn
;		    (tar-copy (if (file-directory-p newname)
;				  (expand-file-name
;				   (file-name-nondirectory back) newname)
;				newname))
;		    (setq copied t))
;		(error "%s not found in tar file %s" back front))))))
;     (unless copied
;       (copy-file file newname ok-if-already-exists keep-date))))

; (defun ushell-file-attributes (filename)
;   "Return a list of attributes of file FILENAME.
; See the documentation for `file-attributes'."
;   (let (result)
;     (when (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" filename)
;       (let ((front (match-string 1 filename))
;	    (back (match-string 2 filename))
;	    buffer)
;	(when (and front (string-match ushell-tar-regexp front)
;		   (setq buffer (find-file-noselect front)))
;	  (with-current-buffer buffer
;	    (goto-char (point-min))
;	    (when (re-search-forward (concat " " (regexp-quote back)
;					     "\\s-*$") nil t)
;	      (let* ((descrip (tar-current-descriptor))
;		     (tokens (tar-desc-tokens descrip)))
;		(setq result
;		      (list
;		       (cond
;			((eq (tar-header-link-type tokens) 5)
;			 t)
;			((eq (tar-header-link-type tokens) t)
;			 (tar-header-link-name tokens)))
;		       1
;		       (tar-header-uid tokens)
;		       (tar-header-gid tokens)
;		       (tar-header-date tokens)
;		       (tar-header-date tokens)
;		       (tar-header-date tokens)
;		       (tar-header-size tokens)
;		       (concat
;			(cond
;			 ((eq (tar-header-link-type tokens) 5) "d")
;			 ((eq (tar-header-link-type tokens) t) "l")
;			 (t "-"))
;			(tar-grind-file-mode (tar-header-mode tokens)
;					     (make-string 9 ? ) 0))
;		       nil nil nil))))))))
;     (or result
;	(file-attributes filename))))

;;; Code:

;;; us-util.el ends here
