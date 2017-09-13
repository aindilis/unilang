;;; ush-maint.el --- init code for building ushell

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

;;; Commentary:

;;; Code:

(provide 'ush-maint)

(and (fboundp 'font-lock-add-keywords)
     (font-lock-add-keywords
      'emacs-lisp-mode
      '(("(ushell-for\\>"            . font-lock-keyword-face)
	("(ushell-deftest\\>"        . font-lock-keyword-face)
	("(ushell-condition-case\\>" . font-lock-keyword-face))))

(if (file-directory-p "../pcomplete")
    (add-to-list 'load-path "../pcomplete"))

(if (locate-library "pcomplete")
    (require 'pcomplete))

(eval-when-compile
  (require 'cl)
  (setq cl-optimize-speed 9))

;; (defun ushell-generate-autoloads ()
;;   (interactive)
;;   (require 'autoload)
;;   (setq generated-autoload-file
;;	(expand-file-name (car command-line-args-left)))
;;   (setq command-line-args-left (cdr command-line-args-left))
;;   (batch-update-autoloads))

(require 'ushell)
(require 'ush-mode)    ; brings in ushell-util
(require 'ush-opt)
(require 'ush-test)

;; (defun ushell-generate-main-menu ()
;;   "Create the main menu for the ushell documentation."
;;   (insert "@menu
;; * The Emacs shell::                 ushell.

;; Core Functionality\n")
;;   (ushell-for module
;;       (sort (ushell-subgroups 'ushell)
;;	    (function
;;	     (lambda (a b)
;;	       (string-lessp (symbol-name a)
;;			     (symbol-name b)))))
;;     (insert (format "* %-34s"
;;		    (concat (get module 'custom-tag) "::"))
;;	    (symbol-name module) ".\n"))
;;   (insert "\nOptional Functionality\n")
;;   (ushell-for module
;;       (sort (ushell-subgroups 'ushell-module)
;;	    (function
;;	     (lambda (a b)
;;	       (string-lessp (symbol-name a)
;;			     (symbol-name b)))))
;;     (insert (format "* %-34s"
;;		    (concat (get module 'custom-tag) "::"))
;;	    (symbol-name module) ".\n"))
;;   (insert "@end menu\n"))

;; (defun ushell-make-texi ()
;;   "Make the ushell.texi file."
;;   (interactive)
;;   (require 'ushell-auto)
;;   (require 'texidoc)
;;   (require 'pcomplete)
;;   (apply 'texidoc-files 'ushell-generate-main-menu "ushell.doci"
;;	 (append
;;	  (list "ushell.el")
;;	  (sort (mapcar
;;		 (function
;;		  (lambda (sym)
;;		    (let ((name (symbol-name sym)))
;;		      (if (string-match "\\`ushell-\\(.*\\)" name)
;;			  (setq name (concat "ush-" (match-string 1 name))))
;;		      (concat name ".el"))))
;;		 (ushell-subgroups 'ushell))
;;		'string-lessp)
;;	  (sort (mapcar
;;		 (function
;;		  (lambda (sym)
;;		    (let ((name (symbol-name sym)))
;;		      (if (string-match "\\`ushell-\\(.*\\)" name)
;;			  (setq name (concat "em-" (match-string 1 name))))
;;		      (concat name ".el"))))
;;		 (ushell-subgroups 'ushell-module))
;;		'string-lessp)
;;	  (list "ushell.texi"))))

;; (defun ushell-make-readme ()
;;   "Make the README file from ushell.el."
;;   (interactive)
;;   (require 'ushell-auto)
;;   (require 'texidoc)
;;   (require 'pcomplete)
;;   (texidoc-files nil "ushell.doci" "ushell.el" "README.texi")
;;   (set-buffer (get-buffer "README.texi"))
;;   (goto-char (point-min))
;;   (search-forward "@chapter")
;;   (beginning-of-line)
;;   (forward-line -1)
;;   (kill-line 2)
;;   (re-search-forward "^@section User Options")
;;   (beginning-of-line)
;;   (delete-region (point) (point-max))
;;   (insert "@bye\n")
;;   (save-buffer)
;;   (with-temp-buffer
;;     (call-process "makeinfo" nil t nil "--no-headers" "README.texi")
;;     (goto-char (point-min))
;;     (search-forward "The Emacs Shell")
;;     (beginning-of-line)
;;     (delete-region (point-min) (point))
;;     (write-file "README"))
;;   (delete-file "README.texi")
;;   (kill-buffer "README.texi"))

;;; ush-maint.el ends here
