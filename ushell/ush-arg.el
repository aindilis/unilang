;;; ush-arg.el --- argument processing

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

(provide 'ush-arg)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-arg nil
  "Argument parsing involves transforming the arguments passed on the
command line into equivalent Lisp forms that, when evaluated, will
yield the values intended."
  :tag "Argument parsing"
  :group 'ushell)

;;; Commentary:

;; Parsing of arguments can be extended by adding functions to the
;; hook `ushell-parse-argument-hook'.  For a good example of this, see
;; `ushell-parse-drive-letter', defined in ushell-dirs.el.

(defcustom ushell-parse-argument-hook
  (list
   ;; a term such as #<buffer NAME>, or #<process NAME> is a buffer
   ;; or process reference
   'ushell-parse-special-reference

   ;; numbers convert to numbers if they stand alone
   (function
    (lambda ()
      (when (and (not ushell-current-argument)
		 (not ushell-current-quoted)
		 (looking-at ushell-number-regexp)
		 (ushell-arg-delimiter (match-end 0)))
	(goto-char (match-end 0))
	(let ((str (match-string 0)))
	  (if (> (length str) 0)
	      (add-text-properties 0 1 '(number t) str))
	  str))))

   ;; parse any non-special characters, based on the current context
   (function
    (lambda ()
      (unless ushell-inside-quote-regexp
	(setq ushell-inside-quote-regexp
	      (format "[^%s]+"
		      (apply 'string ushell-special-chars-inside-quoting))))
      (unless ushell-outside-quote-regexp
	(setq ushell-outside-quote-regexp
	      (format "[^%s]+"
		      (apply 'string ushell-special-chars-outside-quoting))))
      (when (looking-at (if ushell-current-quoted
			    ushell-inside-quote-regexp
			  ushell-outside-quote-regexp))
	(goto-char (match-end 0))
	(let ((str (match-string 0)))
	  (if str
	      (set-text-properties 0 (length str) nil str))
	  str))))

   ;; whitespace or a comment is an argument delimiter
   (function
    (lambda ()
      (let (comment-p)
	(when (or (looking-at "[ \t]+")
		  (and (not ushell-current-argument)
		       (looking-at "#\\([^<'].*\\|$\\)")
		       (setq comment-p t)))
	  (if comment-p
	      (add-text-properties (match-beginning 0) (match-end 0)
				   '(comment t)))
	  (goto-char (match-end 0))
	  (ushell-finish-arg)))))

   ;; backslash before a special character means escape it
   'ushell-parse-backslash

   ;; text beginning with ' is a literally quoted
   'ushell-parse-literal-quote

   ;; text beginning with " is interpolably quoted
   'ushell-parse-double-quote

   ;; argument delimiter
   'ushell-parse-delimiter)
  "*Define how to process Ushell command line arguments.
When each function on this hook is called, point will be at the
current position within the argument list.  The function should either
return nil, meaning that it did no argument parsing, or it should
return the result of the parse as a sexp.  It is also responsible for
moving the point forward to reflect the amount of input text that was
parsed.

If no function handles the current character at point, it will be
treated as a literal character."
  :type 'hook
  :group 'ushell-arg)

;;; Code:

;;; User Variables:

(defcustom ushell-arg-load-hook '(ushell-arg-initialize)
  "*A hook that gets run when `ushell-arg' is loaded."
  :type 'hook
  :group 'ushell-arg)

(defcustom ushell-delimiter-argument-list '(?\; ?& ?\| ?\> ?  ?\t ?\n)
  "List of characters to recognize as argument separators."
  :type '(repeat character)
  :group 'ushell-arg)

(defcustom ushell-special-chars-inside-quoting '(?\\ ?\")
  "*Characters which are still special inside double quotes."
  :type '(repeat character)
  :group 'ushell-arg)

(defcustom ushell-special-chars-outside-quoting
  (append ushell-delimiter-argument-list '(?# ?! ?\\ ?\" ?\'))
  "*Characters that require escaping outside of double quotes.
Without escaping them, they will introduce a change in the argument."
  :type '(repeat character)
  :group 'ushell-arg)

;;; Internal Variables:

(defvar ushell-current-argument nil)
(defvar ushell-current-modifiers nil)
(defvar ushell-arg-listified nil)
(defvar ushell-nested-argument nil)
(defvar ushell-current-quoted nil)
(defvar ushell-inside-quote-regexp nil)
(defvar ushell-outside-quote-regexp nil)

;;; Functions:

(defun ushell-arg-initialize ()
  "Initialize the argument parsing code."
  (define-key ushell-command-map [(meta ?b)] 'ushell-insert-buffer-name)
  (set (make-local-variable 'ushell-inside-quote-regexp) nil)
  (set (make-local-variable 'ushell-outside-quote-regexp) nil))

(defun ushell-insert-buffer-name (buffer-name)
  "Insert BUFFER-NAME into the current buffer at point."
  (interactive "BName of buffer: ")
  (insert-and-inherit "#<buffer " buffer-name ">"))

(defsubst ushell-escape-arg (string)
  "Return STRING with the `escaped' property on it."
  (if (stringp string)
      (add-text-properties 0 (length string) '(escaped t) string))
  string)

(defun ushell-resolve-current-argument ()
  "If there are pending modifications to be made, make them now."
  (when ushell-current-argument
    (when ushell-arg-listified
      (let ((parts ushell-current-argument))
	(while parts
	  (unless (stringp (car parts))
	    (setcar parts
		    (list 'ushell-to-flat-string (car parts))))
	  (setq parts (cdr parts)))
	(setq ushell-current-argument
	      (list 'ushell-convert
		    (append (list 'concat) ushell-current-argument))))
      (setq ushell-arg-listified nil))
    (while ushell-current-modifiers
      (setq ushell-current-argument
	    (list (car ushell-current-modifiers) ushell-current-argument)
	    ushell-current-modifiers (cdr ushell-current-modifiers))))
  (setq ushell-current-modifiers nil))

(defun ushell-finish-arg (&optional argument)
  "Finish the current argument being processed."
  (if argument
      (setq ushell-current-argument argument))
  (throw 'ushell-arg-done t))

(defsubst ushell-arg-delimiter (&optional pos)
  "Return non-nil if POS is an argument delimiter.
If POS is nil, the location of point is checked."
  (let ((pos (or pos (point))))
    (or (= pos (point-max))
	(memq (char-after pos) ushell-delimiter-argument-list))))

;; Argument parsing

(defun ushell-parse-arguments (beg end)
  "Parse all of the arguments at point from BEG to END.
Returns the list of arguments in their raw form.
Point is left at the end of the arguments."
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let ((inhibit-point-motion-hooks t)
	    (args (list t))
	    after-change-functions
	    delim)
	(remove-text-properties (point-min) (point-max)
				'(arg-begin nil arg-end nil))
	(if (setq
	     delim
	     (catch 'ushell-incomplete
	       (while (not (eobp))
		 (let* ((here (point))
			(arg (ushell-parse-argument)))
		   (if (= (point) here)
		       (error "Failed to parse argument '%s'"
			      (buffer-substring here (point-max))))
		   (and arg (nconc args (list arg)))))))
	    (if (listp delim)
		(throw 'ushell-incomplete delim)
	      (throw 'ushell-incomplete
		     (list delim (point) (cdr args)))))
	(cdr args)))))

(defun ushell-parse-argument ()
  "Get the next argument.  Leave point after it."
  (let* ((outer (null ushell-nested-argument))
	 (arg-begin (and outer (point)))
	 (ushell-nested-argument t)
	 ushell-current-argument
	 ushell-current-modifiers
	 ushell-arg-listified)
    (catch 'ushell-arg-done
      (while (not (eobp))
	(let ((result
	       (or (run-hook-with-args-until-success
		    'ushell-parse-argument-hook)
		   (prog1
		       (char-to-string (char-after))
		     (forward-char)))))
	  (if (not ushell-current-argument)
	      (setq ushell-current-argument result)
	    (unless ushell-arg-listified
	      (setq ushell-current-argument
		    (list ushell-current-argument)
		    ushell-arg-listified t))
	    (nconc ushell-current-argument (list result))))))
    (when (and outer ushell-current-argument)
      (add-text-properties arg-begin (1+ arg-begin)
			   '(arg-begin t rear-nonsticky
				       (arg-begin arg-end)))
      (add-text-properties (1- (point)) (point)
			   '(arg-end t rear-nonsticky
				     (arg-end arg-begin))))
    (ushell-resolve-current-argument)
    ushell-current-argument))

(defsubst ushell-operator (&rest args)
  "A stub function that generates an error if a floating operator is found."
  (error "Unhandled operator in input text"))

(defsubst ushell-looking-at-backslash-return (pos)
  "Test whether a backslash-return sequence occurs at POS."
  (and (eq (char-after pos) ?\\)
       (or (= (1+ pos) (point-max))
	   (and (eq (char-after (1+ pos)) ?\n)
		(= (+ pos 2) (point-max))))))

(defun ushell-quote-backslash (string &optional index)
  "Intelligently backslash the character occuring in STRING at INDEX.
If the character is itself a backslash, it needs no escaping."
  (let ((char (aref string index)))
    (if (eq char ?\\)
	(char-to-string char)
      (if (memq char ushell-special-chars-outside-quoting)
	  (string ?\\ char)))))

(defun ushell-parse-backslash ()
  "Parse a single backslash (\) character, which might mean escape.
It only means escape if the character immediately following is a
special character that is not itself a backslash."
  (when (eq (char-after) ?\\)
    (if (ushell-looking-at-backslash-return (point))
	(throw 'ushell-incomplete ?\\)
      (if (and (not (eq (char-after (1+ (point))) ?\\))
	       (if ushell-current-quoted
		   (memq (char-after (1+ (point)))
			 ushell-special-chars-inside-quoting)
		 (memq (char-after (1+ (point)))
		       ushell-special-chars-outside-quoting)))
	  (progn
	    (forward-char 2)
	    (list 'ushell-escape-arg
		  (char-to-string (char-before))))
	;; allow \\<RET> to mean a literal "\" character followed by a
	;; normal return, rather than a backslash followed by a line
	;; continuator (i.e., "\\ + \n" rather than "\ + \\n").  This
	;; is necessary because backslashes in Ushell are not special
	;; unless they either precede something special, or precede a
	;; backslash that precedes something special.  (Mainly this is
	;; done to make using backslash on Windows systems more
	;; natural-feeling).
	(if (ushell-looking-at-backslash-return (1+ (point)))
	    (forward-char))
	(forward-char)
	"\\"))))

(defun ushell-parse-literal-quote ()
  "Parse a literally quoted string.  Nothing has special meaning!"
  (if (eq (char-after) ?\')
      (let ((end (ushell-find-delimiter ?\' ?\')))
	(if (not end)
	    (throw 'ushell-incomplete ?\')
	  (let ((string (buffer-substring-no-properties (1+ (point)) end)))
	    (goto-char (1+ end))
	    (while (string-match "''" string)
	      (setq string (replace-match "'" t t string)))
	    (list 'ushell-escape-arg string))))))

(defun ushell-parse-double-quote ()
  "Parse a double quoted string, which allows for variable interpolation."
  (when (eq (char-after) ?\")
    (let* ((end (ushell-find-delimiter ?\" ?\" nil nil t))
	   (ushell-current-quoted t))
      (if (not end)
	  (throw 'ushell-incomplete ?\")
	(prog1
	    (save-restriction
	      (forward-char)
	      (narrow-to-region (point) end)
	      (list 'ushell-escape-arg
		    (ushell-parse-argument)))
	  (goto-char (1+ end)))))))

(defun ushell-parse-special-reference ()
  "Parse a special syntax reference, of the form '#<type arg>'."
  (if (and (not ushell-current-argument)
	   (not ushell-current-quoted)
	   (looking-at "#<\\(buffer\\|process\\)\\s-"))
      (let ((here (point)))
	(goto-char (match-end 0))
	(let* ((buffer-p (string= (match-string 1) "buffer"))
	       (end (ushell-find-delimiter ?\< ?\>)))
	  (if (not end)
	      (throw 'ushell-incomplete ?\<)
	    (if (ushell-arg-delimiter (1+ end))
		(prog1
		    (list (if buffer-p 'get-buffer-create 'get-process)
			  (buffer-substring-no-properties (point) end))
		  (goto-char (1+ end)))
	      (ignore (goto-char here))))))))

(defun ushell-parse-delimiter ()
  "Parse an argument delimiter, which is essentially a command operator."
  ;; this `ushell-operator' keyword gets parsed out by
  ;; `ushell-separate-commands'.  Right now the only possibility for
  ;; error is an incorrect output redirection specifier.
  (when (looking-at "[&|;\n]\\s-*")
    (let ((end (match-end 0)))
    (if ushell-current-argument
	(ushell-finish-arg)
      (ushell-finish-arg
       (prog1
	   (list 'ushell-operator
		 (cond
		  ((eq (char-after end) ?\&)
		   (setq end (1+ end)) "&&")
		  ((eq (char-after end) ?\|)
		   (setq end (1+ end)) "||")
		  ((eq (char-after) ?\n) ";")
		  (t
		   (char-to-string (char-after)))))
	 (goto-char end)))))))

;;; us-arg.el ends here
