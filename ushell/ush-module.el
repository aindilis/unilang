;;; ush-module.el --- Ushell modules

;; Copyright (C) 1999, 2000 Free Software Foundation

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes

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

(provide 'ush-module)

(eval-when-compile
  (require 'ush-maint)
  (require 'cl))

(defgroup ushell-module nil
  "The `ushell-module' group is for Ushell extension modules, which
provide optional behavior which the user can enable or disable by
customizing the variable `ushell-modules-list'."
  :tag "Extension modules"
  :group 'ushell)

;;; Commentary:

(require 'ush-util)

(defun ushell-load-defgroups (&optional directory)
  "Load `defgroup' statements from Ushell's module files."
  (with-current-buffer
      (find-file-noselect (expand-file-name "ush-groups.el" directory))
    (erase-buffer)
    (insert ";;; do not modify this file; it is auto-generated\n\n")
    (let ((files (directory-files (or directory
				      (car command-line-args-left))
				  nil "\\`em-.*\\.el\\'")))
      (while files
	(message "Loading defgroup from `%s'" (car files))
	(let (defgroup)
	  (catch 'handled
	    (with-current-buffer (find-file-noselect (car files))
	      (goto-char (point-min))
	      (while t
		(forward-sexp)
		(if (eobp) (throw 'handled t))
		(backward-sexp)
		(let ((begin (point))
		      (defg (looking-at "(defgroup")))
		  (forward-sexp)
		  (if defg
		      (setq defgroup (buffer-substring begin (point))))))))
	  (if defgroup
	      (insert defgroup "\n\n")))
	(setq files (cdr files))))
    (save-buffer)))

;; load the defgroup's for the standard extension modules, so that
;; documentation can be provided when the user customize's
;; `ushell-modules-list'.
(eval-when-compile
  (when (and (boundp 'byte-compile-current-file)
	     byte-compile-current-file
	     (or
	      (equal (file-name-nondirectory byte-compile-current-file)
		     "ush-module.el")
	      ;; When ushell file names are expanded from a wildcard
	      ;; or by reading the Ushell directory, e.g. when they
	      ;; say "make recompile" in the lisp directory, Emacs on
	      ;; MS-DOS sees a truncated name "ush-modu.el" instead of
	      ;; "ush-module.el".
	      (and (fboundp 'msdos-long-file-names)
		   (null (msdos-long-file-names))
		   (equal (file-name-nondirectory byte-compile-current-file)
			  "ush-modu.el"))))
    (let* ((directory (file-name-directory byte-compile-current-file))
	   (elc-file (expand-file-name "ush-groups.elc" directory)))
      (ushell-load-defgroups directory)
      (if (file-exists-p elc-file) (delete-file elc-file)))))

(load "ush-groups" t t)

;;; User Variables:

(defcustom ushell-module-unload-hook
  '(ushell-unload-extension-modules)
  "*A hook run when `ushell-module' is unloaded."
  :type 'hook
  :group 'ushell-module)

(defcustom ushell-modules-list
  '(ushell-alias
    ushell-banner
    ushell-basic
    ushell-cmpl
    ushell-dirs
    ushell-glob
    ushell-hist
    ushell-ls
    ushell-pred
    ushell-prompt
    ushell-script
    ushell-term
    ushell-unix)
  "*A list of optional add-on modules to be loaded by Ushell.
Changes will only take effect in future Ushell buffers."
  :type (append
	 (list 'set ':tag "Supported modules")
	 (mapcar
	  (function
	   (lambda (modname)
	     (let ((modsym (intern modname)))
	       (list 'const
		     ':tag (format "%s -- %s" modname
				   (get modsym 'custom-tag))
		     ':link (caar (get modsym 'custom-links))
		     ':doc (concat "\n" (get modsym 'group-documentation)
				   "\n ")
		     modsym))))
	  (sort (mapcar 'symbol-name
			(ushell-subgroups 'ushell-module))
		'string-lessp))
	 '((repeat :inline t :tag "Other modules" symbol)))
  :group 'ushell-module)

;;; Code:

(defsubst ushell-using-module (module)
  "Return non-nil if a certain Ushell MODULE is in use.
The MODULE should be a symbol corresponding to that module's
customization group.  Example: `ushell-cmpl' for that module."
  (memq module ushell-modules-list))

(defun ushell-unload-extension-modules ()
  "Unload any memory resident extension modules."
  (ushell-for module (ushell-subgroups 'ushell-module)
    (if (featurep module)
	(ignore-errors
	  (message "Unloading %s..." (symbol-name module))
	  (unload-feature module)
	  (message "Unloading %s...done" (symbol-name module))))))

;;; us-module.el ends here
