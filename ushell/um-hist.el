;;; em-hist.el --- history list management

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

(provide 'em-hist)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-hist nil
  "This module provides command history management."
  :tag "History list management"
  :group 'ushell-module)

;;; Commentary:

;; Ushell's history facility imitates the syntax used by bash
;; ([(bash)History Interaction]).  Thus:
;;
;;   !ls           ; repeat the last command beginning with 'ls'
;;   !?ls          ; repeat the last command containing ls
;;   echo !ls:2    ; echo the second arg of the last 'ls' command
;;   !ls<tab>      ; complete against all possible words in this
;;                 ; position, by looking at the history list
;;   !ls<C-c SPC>  ; expand any matching history input at point
;;
;; Also, most of `comint-mode's keybindings are accepted:
;;
;;   M-r     ; search backward for a previous command by regexp
;;   M-s     ; search forward for a previous command by regexp
;;   M-p     ; access the last command entered, repeatable
;;   M-n     ; access the first command entered, repeatable
;;
;;   C-c M-r ; using current input, find a matching command thus, with
;;           ; 'ls' as the current input, it will go back to the same
;;           ; command that '!ls' would have selected
;;   C-c M-s ; same, but in reverse order
;;
;; Note that some of these keybindings are only available if the
;; `ushell-rebind' is not in use, in which case M-p does what C-c M-r
;; normally would do, and C-p is used instead of M-p.  It may seem
;; confusing, but the intention is to make the most useful
;; functionality the most easily accessible.  If `ushell-rebind' is
;; not being used, history navigation will use comint's keybindings;
;; if it is, history navigation tries to use similar keybindings to
;; bash.  This is all configurable, of course.

;;; Code:

(require 'ring)
(require 'ush-opt)
(require 'em-pred)

;;; User Variables:

(defcustom ushell-hist-load-hook '(ushell-hist-initialize)
  "*A list of functions to call when loading `ushell-hist'."
  :type 'hook
  :group 'ushell-hist)

(defcustom ushell-hist-unload-hook
  (list
   (function
    (lambda ()
      (remove-hook 'kill-emacs-hook 'ushell-save-some-history))))
  "*A hook that gets run when `ushell-hist' is unloaded."
  :type 'hook
  :group 'ushell-hist)

(defcustom ushell-history-file-name
  (concat ushell-directory-name "history")
  "*If non-nil, name of the file to read/write input history.
See also `ushell-read-history' and `ushell-write-history'.
If it is nil, Ushell will use the value of HISTFILE."
  :type 'file
  :group 'ushell-hist)

(defcustom ushell-history-size 128
  "*Size of the input history ring.  If nil, use envvar HISTSIZE."
  :type 'integer
  :group 'ushell-hist)

(defcustom ushell-hist-ignoredups nil
  "*If non-nil, don't add input matching the last on the input ring.
This mirrors the optional behavior of bash."
  :type 'boolean
  :group 'ushell-hist)

(defcustom ushell-ask-to-save-history t
  "*Determine if history should be automatically saved.
History is always preserved after sanely exiting an Ushell buffer.
However, when Emacs is being shut down, this variable determines
whether to prompt the user.
If set to nil, it means never ask whether history should be saved.
If set to t, always ask if any Ushell buffers are open at exit time.
If set to `always', history will always be saved, silently."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" t)
		 (const :tag "Always save" always))
  :group 'ushell-hist)

(defcustom ushell-input-filter
  (function
   (lambda (str)
     (not (string-match "\\`\\s-*\\'" str))))
  "*Predicate for filtering additions to input history.
Takes one argument, the input.  If non-nil, the input may be saved on
the input history list.  Default is to save anything that isn't all
whitespace."
  :type 'function
  :group 'ushell-hist)

(put 'ushell-input-filter 'risky-local-variable t)

(defcustom ushell-hist-match-partial t
  "*If non-nil, movement through history is constrained by current input.
Otherwise, typing <M-p> and <M-n> will always go to the next history
element, regardless of any text on the command line.  In that case,
<C-c M-r> and <C-c M-s> still offer that functionality."
  :type 'boolean
  :group 'ushell-hist)

(defcustom ushell-hist-move-to-end t
  "*If non-nil, move to the end of the buffer before cycling history."
  :type 'boolean
  :group 'ushell-hist)

(defcustom ushell-hist-event-designator
  "^!\\(!\\|-?[0-9]+\\|\\??[^:^$%*?]+\\??\\|#\\)"
  "*The regexp used to identifier history event designators."
  :type 'regexp
  :group 'ushell-hist)

(defcustom ushell-hist-word-designator
  "^:?\\([0-9]+\\|[$^%*]\\)?\\(\\*\\|-[0-9]*\\|[$^%*]\\)?"
  "*The regexp used to identify history word designators."
  :type 'regexp
  :group 'ushell-hist)

(defcustom ushell-hist-modifier
  "^\\(:\\([hretpqx&g]\\|s/\\([^/]*\\)/\\([^/]*\\)/\\)\\)*"
  "*The regexp used to identity history modifiers."
  :type 'regexp
  :group 'ushell-hist)

(defcustom ushell-hist-rebind-keys-alist
  '(([(control ?p)]   . ushell-previous-input)
    ([(control ?n)]   . ushell-next-input)
    ([(control up)]   . ushell-previous-input)
    ([(control down)] . ushell-next-input)
    ([(control ?r)]   . ushell-isearch-backward)
    ([(control ?s)]   . ushell-isearch-forward)
    ([(meta ?r)]      . ushell-previous-matching-input)
    ([(meta ?s)]      . ushell-next-matching-input)
    ([(meta ?p)]      . ushell-previous-matching-input-from-input)
    ([(meta ?n)]      . ushell-next-matching-input-from-input)
    ([up]             . ushell-previous-matching-input-from-input)
    ([down]           . ushell-next-matching-input-from-input))
  "*History keys to bind differently if point is in input text."
  :type '(repeat (cons (vector :tag "Keys to bind"
			       (repeat :inline t sexp))
		       (function :tag "Command")))
  :group 'ushell-hist)

;;; Internal Variables:

(defvar ushell-history-ring nil)
(defvar ushell-history-index nil)
(defvar ushell-matching-input-from-input-string "")
(defvar ushell-save-history-index nil)

(defvar ushell-isearch-map nil)

(unless ushell-isearch-map
  (setq ushell-isearch-map (copy-keymap isearch-mode-map))
  (define-key ushell-isearch-map [(control ?m)] 'ushell-isearch-return)
  (define-key ushell-isearch-map [return] 'ushell-isearch-return)
  (define-key ushell-isearch-map [(control ?r)] 'ushell-isearch-repeat-backward)
  (define-key ushell-isearch-map [(control ?s)] 'ushell-isearch-repeat-forward)
  (define-key ushell-isearch-map [(control ?g)] 'ushell-isearch-abort)
  (define-key ushell-isearch-map [backspace] 'ushell-isearch-delete-char)
  (define-key ushell-isearch-map [delete] 'ushell-isearch-delete-char)
  (defvar ushell-isearch-cancel-map)
  (define-prefix-command 'ushell-isearch-cancel-map)
  (define-key ushell-isearch-map [(control ?c)] 'ushell-isearch-cancel-map)
  (define-key ushell-isearch-cancel-map [(control ?c)] 'ushell-isearch-cancel))

;;; Functions:

(defun ushell-hist-initialize ()
  "Initialize the history management code for one Ushell buffer."
  (make-local-hook 'ushell-expand-input-functions)
  (add-hook 'ushell-expand-input-functions
	    'ushell-expand-history-references nil t)

  (when (ushell-using-module 'ushell-cmpl)
    (make-local-hook 'pcomplete-try-first-hook)
    (add-hook 'pcomplete-try-first-hook
	      'ushell-complete-history-reference nil t))

  (if (ushell-using-module 'ushell-rebind)
      (let ((rebind-alist (symbol-value 'ushell-rebind-keys-alist)))
	(make-local-variable 'ushell-rebind-keys-alist)
	(set 'ushell-rebind-keys-alist
	     (append rebind-alist ushell-hist-rebind-keys-alist))
	(set (make-local-variable 'search-invisible) t)
	(set (make-local-variable 'search-exit-option) t)
	(make-local-hook 'isearch-mode-hook)
	(add-hook 'isearch-mode-hook
		  (function
		   (lambda ()
		     (if (>= (point) ushell-last-output-end)
			 (setq overriding-terminal-local-map
			       ushell-isearch-map)))) nil t)
	(make-local-hook 'isearch-mode-end-hook)
	(add-hook 'isearch-mode-end-hook
		  (function
		   (lambda ()
		     (setq overriding-terminal-local-map nil))) nil t))
    (define-key ushell-mode-map [up] 'ushell-previous-matching-input-from-input)
    (define-key ushell-mode-map [down] 'ushell-next-matching-input-from-input)
    (define-key ushell-mode-map [(control up)] 'ushell-previous-input)
    (define-key ushell-mode-map [(control down)] 'ushell-next-input)
    (define-key ushell-mode-map [(meta ?r)] 'ushell-previous-matching-input)
    (define-key ushell-mode-map [(meta ?s)] 'ushell-next-matching-input)
    (define-key ushell-command-map [(meta ?r)]
      'ushell-previous-matching-input-from-input)
    (define-key ushell-command-map [(meta ?s)]
      'ushell-next-matching-input-from-input)
    (if ushell-hist-match-partial
	(progn
	  (define-key ushell-mode-map [(meta ?p)]
	    'ushell-previous-matching-input-from-input)
	  (define-key ushell-mode-map [(meta ?n)]
	    'ushell-next-matching-input-from-input)
	  (define-key ushell-command-map [(meta ?p)] 'ushell-previous-input)
	  (define-key ushell-command-map [(meta ?n)] 'ushell-next-input))
      (define-key ushell-mode-map [(meta ?p)] 'ushell-previous-input)
      (define-key ushell-mode-map [(meta ?n)] 'ushell-next-input)
      (define-key ushell-command-map [(meta ?p)]
	'ushell-previous-matching-input-from-input)
      (define-key ushell-command-map [(meta ?n)]
	'ushell-next-matching-input-from-input)))

  (make-local-variable 'ushell-history-size)
  (or ushell-history-size
      (setq ushell-history-size (getenv "HISTSIZE")))

  (make-local-variable 'ushell-history-file-name)
  (or ushell-history-file-name
      (setq ushell-history-file-name (getenv "HISTFILE")))

  (make-local-variable 'ushell-history-index)
  (make-local-variable 'ushell-save-history-index)
  (make-local-variable 'ushell-history-ring)
  (if ushell-history-file-name
      (ushell-read-history nil t))
  (unless ushell-history-ring
    (setq ushell-history-ring (make-ring ushell-history-size)))

  (make-local-hook 'ushell-exit-hook)
  (add-hook 'ushell-exit-hook 'ushell-write-history nil t)

  (add-hook 'kill-emacs-hook 'ushell-save-some-history)

  (make-local-variable 'ushell-input-filter-functions)
  (add-hook 'ushell-input-filter-functions 'ushell-add-to-history nil t)

  (define-key ushell-command-map [(control ?l)] 'ushell-list-history)
  (define-key ushell-command-map [(control ?x)] 'ushell-get-next-from-history))

(defun ushell-save-some-history ()
  "Save the history for any open Ushell buffers."
  (ushell-for buf (buffer-list)
    (if (buffer-live-p buf)
	(with-current-buffer buf
	  (if (and ushell-mode
		   ushell-history-file-name
		   ushell-ask-to-save-history
		   (or (eq ushell-ask-to-save-history 'always)
		       (y-or-n-p
			(format "Save input history for Ushell buffer `%s'? "
				(buffer-name buf)))))
	      (ushell-write-history))))))

(defun ushell/history (&rest args)
  "List in help buffer the buffer's input history."
  (ushell-init-print-buffer)
  (ushell-eval-using-options
   "history" args
   '((?r "read" nil read-history
	 "read from history file to current history list")
     (?w "write" nil write-history
	 "write current history list to history file")
     (?a "append" nil append-history
	 "append current history list to history file")
     (?h "help" nil nil "display this usage message")
     :usage "[n] [-rwa [filename]]"
     :post-usage
"When Ushell is started, history is read from `ushell-history-file-name'.
This is also the location where history info will be saved by this command,
unless a different file is specified on the command line.")
   (and (or (not (ring-p ushell-history-ring))
	   (ring-empty-p ushell-history-ring))
	(error "No history"))
   (let (length command file)
     (when (and args (string-match "^[0-9]+$" (car args)))
       (setq length (min (ushell-convert (car args))
			 (ring-length ushell-history-ring))
	     args (cdr args)))
     (and length
	  (or read-history write-history append-history)
	  (error "history: extra arguments"))
     (when (and args (stringp (car args)))
       (setq file (car args)
	     args (cdr args)))
     (cond
      (read-history (ushell-read-history file))
      (write-history (ushell-write-history file))
      (append-history (ushell-write-history file t))
      (t
       (let* ((history nil)
	      (index (1- (or length (ring-length ushell-history-ring))))
	      (ref (- (ring-length ushell-history-ring) index)))
	 ;; We have to build up a list ourselves from the ring vector.
	 (while (>= index 0)
	   (ushell-buffered-print
	    (format "%5d  %s\n" ref (ushell-get-history index)))
	   (setq index (1- index)
		 ref (1+ ref)))))))
   (ushell-flush)
   nil))

(defun ushell-put-history (input &optional ring at-beginning)
  "Put a new input line into the history ring."
  (unless ring (setq ring ushell-history-ring))
  (if at-beginning
      (ring-insert-at-beginning ring input)
    (ring-insert ring input)))

(defun ushell-get-history (index &optional ring)
  "Get an input line from the history ring."
  (ring-ref (or ring ushell-history-ring) index))

(defun ushell-add-to-history ()
  "Add INPUT to the history ring.
The input is entered into the input history ring, if the value of
variable `ushell-input-filter' returns non-nil when called on the
input."
  (when (> (1- ushell-last-input-end) ushell-last-input-start)
    (let ((input (buffer-substring ushell-last-input-start
				   (1- ushell-last-input-end))))
      (if (and (funcall ushell-input-filter input)
	       (or (null ushell-hist-ignoredups)
		   (not (ring-p ushell-history-ring))
		   (ring-empty-p ushell-history-ring)
		   (not (string-equal (ushell-get-history 0) input))))
	  (ushell-put-history input))
      (setq ushell-save-history-index ushell-history-index)
      (setq ushell-history-index nil))))

(defun ushell-read-history (&optional filename silent)
  "Sets the buffer's `ushell-history-ring' from a history file.
The name of the file is given by the variable
`ushell-history-file-name'.  The history ring is of size
`ushell-history-size', regardless of file size.  If
`ushell-history-file-name' is nil this function does nothing.

If the optional argument SILENT is non-nil, we say nothing about a
failure to read the history file.

This function is useful for major mode commands and mode hooks.

The structure of the history file should be one input command per
line, with the most recent command last.  See also
`ushell-hist-ignoredups' and `ushell-write-history'."
  (let ((file (or filename ushell-history-file-name)))
    (cond
     ((or (null file)
	  (equal file ""))
      nil)
     ((not (file-readable-p file))
      (or silent
	  (message "Cannot read history file %s" file)))
     (t
      (let* ((count 0)
	     (size ushell-history-size)
	     (ring (make-ring size))
	     (ignore-dups ushell-hist-ignoredups))
	(with-temp-buffer
	  (insert-file-contents file)
	  ;; Save restriction in case file is already visited...
	  ;; Watch for those date stamps in history files!
	  (goto-char (point-max))
	  (while (and (< count size)
		      (re-search-backward "^[ \t]*\\([^#\n].*\\)[ \t]*$"
					  nil t))
	    (let ((history (match-string 1)))
	      (if (or (null ignore-dups)
		      (ring-empty-p ring)
		      (not (string-equal (ring-ref ring 0) history)))
		  (ring-insert-at-beginning
		   ring (subst-char-in-string ?\177 ?\n history))))
	    (setq count (1+ count))))
	(setq ushell-history-ring ring
	      ushell-history-index nil))))))

(defun ushell-write-history (&optional filename append)
  "Writes the buffer's `ushell-history-ring' to a history file.
The name of the file is given by the variable
`ushell-history-file-name'.  The original contents of the file are
lost if `ushell-history-ring' is not empty.  If
`ushell-history-file-name' is nil this function does nothing.

Useful within process sentinels.

See also `ushell-read-history'."
  (let ((file (or filename ushell-history-file-name)))
    (cond
     ((or (null file)
	  (equal file "")
	  (null ushell-history-ring)
	  (ring-empty-p ushell-history-ring))
      nil)
     ((not (file-writable-p file))
      (message "Cannot write history file %s" file))
     (t
      (let* ((ring ushell-history-ring)
	     (index (ring-length ring)))
	;; Write it all out into a buffer first.  Much faster, but
	;; messier, than writing it one line at a time.
	(with-temp-buffer
	  (while (> index 0)
	    (setq index (1- index))
	    (let ((start (point)))
	      (insert (ring-ref ring index) ?\n)
	      (subst-char-in-region start (1- (point)) ?\n ?\177)))
	  (ushell-with-private-file-modes
	   (write-region (point-min) (point-max) file append
			 'no-message))))))))

(defun ushell-list-history ()
  "List in help buffer the buffer's input history."
  (interactive)
  (let (prefix prelen)
    (save-excursion
      (if (re-search-backward "!\\(.+\\)" (line-beginning-position) t)
	  (setq prefix (match-string 1)
		prelen (length prefix))))
    (if (or (not (ring-p ushell-history-ring))
	    (ring-empty-p ushell-history-ring))
	(message "No history")
      (let ((history nil)
	    (history-buffer " *Input History*")
	    (index (1- (ring-length ushell-history-ring)))
	    (conf (current-window-configuration)))
	;; We have to build up a list ourselves from the ring vector.
	(while (>= index 0)
	  (let ((hist (ushell-get-history index)))
	    (if (or (not prefix)
		    (and (>= (length hist) prelen)
			 (string= (substring hist 0 prelen) prefix)))
		(setq history (cons hist history))))
	  (setq index (1- index)))
	;; Change "completion" to "history reference"
	;; to make the display accurate.
	(with-output-to-temp-buffer history-buffer
	  (display-completion-list history)
	  (set-buffer history-buffer)
	  (forward-line 3)
	  (while (search-backward "completion" nil 'move)
	    (replace-match "history reference")))
	(ushell-redisplay)
	(message "Hit space to flush")
	(let ((ch (read-event)))
	  (if (eq ch ?\ )
	      (set-window-configuration conf)
	    (setq unread-command-events (list ch))))))))

(defun ushell-hist-word-reference (ref)
  "Return the word designator index referred to by REF."
  (cond
   ((string-match "^[0-9]+$" ref)
    (string-to-number ref))
   ((string= "^" ref) 1)
   ((string= "$" ref) nil)
   ((string= "%" ref)
    (error "`%' history word designator not yet implemented"))))

(defun ushell-hist-parse-arguments (&optional silent b e)
  "Parse current command arguments in a history-code-friendly way."
  (let ((end (or e (point)))
	(begin (or b (save-excursion (ushell-bol) (point))))
	(posb (list t))
	(pose (list t))
	(textargs (list t))
	hist args)
    (unless (catch 'ushell-incomplete
	      (ignore
	       (setq args (ushell-parse-arguments begin end))))
      (save-excursion
	(goto-char begin)
	(while (< (point) end)
	  (if (get-text-property (point) 'arg-begin)
	      (nconc posb (list (point))))
	  (if (get-text-property (point) 'arg-end)
	      (nconc pose
		     (list (if (= (1+ (point)) end)
			       (1+ (point))
			     (point)))))
	  (forward-char))
	(setq posb (cdr posb)
	      pose (cdr pose))
	(assert (= (length posb) (length args)))
	(assert (<= (length posb) (length pose))))
      (setq hist (buffer-substring-no-properties begin end))
      (let ((b posb) (e pose))
	(while b
	  (nconc textargs
		 (list (substring hist (- (car b) begin)
				  (- (car e) begin))))
	  (setq b (cdr b)
		e (cdr e))))
      (setq textargs (cdr textargs))
      (assert (= (length textargs) (length args)))
      (list textargs posb pose))))

(defun ushell-expand-history-references (beg end)
  "Parse and expand any history references in current input."
  (let ((result (ushell-hist-parse-arguments t beg end)))
    (when result
      (let ((textargs (nreverse (nth 0 result)))
	    (posb (nreverse (nth 1 result)))
	    (pose (nreverse (nth 2 result))))
	(save-excursion
	  (while textargs
	    (let ((str (ushell-history-reference (car textargs))))
	      (unless (eq str (car textargs))
		(goto-char (car posb))
		(insert-and-inherit str)
		(delete-char (- (car pose) (car posb)))))
	    (setq textargs (cdr textargs)
		  posb (cdr posb)
		  pose (cdr pose))))))))

(defun ushell-complete-history-reference ()
  "Complete a history reference, by completing the event designator."
  (let ((arg (pcomplete-actual-arg)))
    (when (string-match "\\`![^:^$*%]*\\'" arg)
      (setq pcomplete-stub (substring arg 1)
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions
	     (let ((history nil)
		   (index (1- (ring-length ushell-history-ring)))
		   (stublen (length pcomplete-stub)))
	       ;; We have to build up a list ourselves from the ring
	       ;; vector.
	       (while (>= index 0)
		 (let ((hist (ushell-get-history index)))
		   (if (and (>= (length hist) stublen)
			    (string= (substring hist 0 stublen)
				     pcomplete-stub)
			    (string-match "^\\([^:^$*% \t\n]+\\)" hist))
		       (setq history (cons (match-string 1 hist)
					   history))))
		 (setq index (1- index)))
	       (let ((fhist (list t)))
		 ;; uniqify the list, but preserve the order
		 (while history
		   (unless (member (car history) fhist)
		     (nconc fhist (list (car history))))
		   (setq history (cdr history)))
		 (cdr fhist)))))))

(defun ushell-history-reference (reference)
  "Expand directory stack REFERENCE.
The syntax used here was taken from the Bash info manual.
Returns the resultant reference, or the same string REFERENCE if none
matched."
  ;; `^string1^string2^'
  ;;      Quick Substitution.  Repeat the last command, replacing
  ;;      STRING1 with STRING2.  Equivalent to `!!:s/string1/string2/'
  (if (and (ushell-using-module 'ushell-pred)
	   (string-match "\\^\\([^^]+\\)\\^\\([^^]+\\)\\^?\\s-*$"
			 reference))
      (setq reference (format "!!:s/%s/%s/"
			      (match-string 1 reference)
			      (match-string 2 reference))))
  ;; `!'
  ;;      Start a history substitution, except when followed by a
  ;;      space, tab, the end of the line, = or (.
  (if (not (string-match "^![^ \t\n=\(]" reference))
      reference
    (setq ushell-history-index nil)
    (let ((event (ushell-hist-parse-event-designator reference)))
      (unless event
	(error "Could not find history event `%s'" reference))
      (setq ushell-history-index (car event)
	    reference (substring reference (cdr event))
	    event (ushell-get-history ushell-history-index))
      (if (not (string-match "^[:^$*%]" reference))
	  event
	(let ((word (ushell-hist-parse-word-designator
		     event reference)))
	  (unless word
	    (error "Unable to honor word designator `%s'" reference))
	  (unless (string-match "^[:^$*%][[$^*%0-9-]" reference)
	    (setcdr word 0))
	  (setq event (car word)
		reference (substring reference (cdr word)))
	  (if (not (and (ushell-using-module 'ushell-pred)
			(string-match "^:" reference)))
	      event
	    (ushell-hist-parse-modifier event reference)))))))

(defun ushell-hist-parse-event-designator (reference)
  "Parse a history event designator beginning in REFERENCE."
  (let* ((index (string-match ushell-hist-event-designator reference))
	 (end (and index (match-end 0))))
    (unless index
      (error "Invalid history event designator `%s'" reference))
    (let* ((event (match-string 1 reference))
	   (pos
	    (cond
	     ((string= event "!") (ring-length ushell-history-ring))
	     ((string= event "#") (error "!# not yet implemented"))
	     ((string-match "^-?[0-9]+$" event)
	      (let ((num (string-to-number event)))
		(if (>= num 0)
		    (- (ring-length ushell-history-ring) num)
		  (1- (abs num)))))
	     ((string-match "^\\(\\??\\)\\([^?]+\\)\\??$" event)
	      (let ((pref (if (> (length (match-string 1 event)) 0)
			      "" "^"))
		    (str (match-string 2 event)))
		(save-match-data
		  (ushell-previous-matching-input-string-position
		   (concat pref (regexp-quote str)) 1))))
	     (t
	      (error "Failed to parse event designator `%s'" event)))))
      (and pos (cons pos end)))))

(defun ushell-hist-parse-word-designator (hist reference)
  "Parse a history word designator beginning for HIST in REFERENCE."
  (let* ((index (string-match ushell-hist-word-designator reference))
	 (end (and index (match-end 0))))
    (unless (memq (aref reference 0) '(?: ?^ ?$ ?* ?%))
      (error "Invalid history word designator `%s'" reference))
    (let ((nth (match-string 1 reference))
	  (mth (match-string 2 reference))
	  (here (point))
	  textargs)
      (insert hist)
      (setq textargs (car (ushell-hist-parse-arguments nil here (point))))
      (delete-region here (point))
      (if (string= nth "*")
	  (if mth
	      (error "Invalid history word designator `%s'"
		     reference)
	    (setq nth 1 mth "-$")))
      (if (not mth)
	  (if nth
	      (setq mth nth)
	    (setq nth 0 mth "$"))
	(if (string= mth "-")
	    (setq mth (- (length textargs) 2))
	  (if (string= mth "*")
	      (setq mth "$")
	    (if (not (and (> (length mth) 1)
			  (eq (aref mth 0) ?-)))
		(error "Invalid history word designator `%s'"
		       reference)
	      (setq mth (substring mth 1))))))
      (unless (numberp nth)
	(setq nth (ushell-hist-word-reference nth)))
      (unless (numberp mth)
	(setq mth (ushell-hist-word-reference mth)))
      (cons (mapconcat 'identity (ushell-sublist textargs nth mth) "")
	    end))))

(defun ushell-hist-parse-modifier (hist reference)
  "Parse a history modifier beginning for HIST in REFERENCE."
  (let ((here (point)))
    (insert reference)
    (prog1
	(save-restriction
	  (narrow-to-region here (point))
	  (goto-char (point-min))
	  (let ((modifiers (cdr (ushell-parse-modifiers))))
	    (ushell-for mod modifiers
	      (setq hist (funcall mod hist)))
	    hist))
      (delete-region here (point)))))

(defun ushell-get-next-from-history ()
  "After fetching a line from input history, this fetches the next.
In other words, this recalls the input line after the line you
recalled last.  You can use this to repeat a sequence of input lines."
  (interactive)
  (if ushell-save-history-index
      (progn
	(setq ushell-history-index (1+ ushell-save-history-index))
	(ushell-next-input 1))
    (message "No previous history command")))

(defun ushell-search-arg (arg)
  ;; First make sure there is a ring and that we are after the process
  ;; mark
  (if (and ushell-hist-move-to-end
	   (< (point) ushell-last-output-end))
      (goto-char ushell-last-output-end))
  (cond ((or (null ushell-history-ring)
	     (ring-empty-p ushell-history-ring))
	 (error "Empty input ring"))
	((zerop arg)
	 ;; arg of zero resets search from beginning, and uses arg of
	 ;; 1
	 (setq ushell-history-index nil)
	 1)
	(t
	 arg)))

(defun ushell-search-start (arg)
  "Index to start a directional search, starting at `ushell-history-index'."
  (if ushell-history-index
      ;; If a search is running, offset by 1 in direction of arg
      (mod (+ ushell-history-index (if (> arg 0) 1 -1))
	   (ring-length ushell-history-ring))
    ;; For a new search, start from beginning or end, as appropriate
    (if (>= arg 0)
	0                               ; First elt for forward search
      ;; Last elt for backward search
      (1- (ring-length ushell-history-ring)))))

(defun ushell-previous-input-string (arg)
  "Return the string ARG places along the input ring.
Moves relative to `ushell-history-index'."
  (ushell-get-history (if ushell-history-index
			  (mod (+ arg ushell-history-index)
			       (ring-length ushell-history-ring))
			arg)))

(defun ushell-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (ushell-previous-matching-input "." arg))

(defun ushell-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (ushell-previous-input (- arg)))

(defun ushell-previous-matching-input-string (regexp arg)
  "Return the string matching REGEXP ARG places along the input ring.
Moves relative to `ushell-history-index'."
  (let* ((pos (ushell-previous-matching-input-string-position regexp arg)))
    (if pos (ushell-get-history pos))))

(defun ushell-previous-matching-input-string-position
  (regexp arg &optional start)
  "Return the index matching REGEXP ARG places along the input ring.
Moves relative to START, or `ushell-history-index'."
  (if (or (not (ring-p ushell-history-ring))
	  (ring-empty-p ushell-history-ring))
      (error "No history"))
  (let* ((len (ring-length ushell-history-ring))
	 (motion (if (> arg 0) 1 -1))
	 (n (mod (- (or start (ushell-search-start arg)) motion) len))
	 (tried-each-ring-item nil)
	 (case-fold-search (ushell-under-windows-p))
	 (prev nil))
    ;; Do the whole search as many times as the argument says.
    (while (and (/= arg 0) (not tried-each-ring-item))
      ;; Step once.
      (setq prev n
	    n (mod (+ n motion) len))
      ;; If we haven't reached a match, step some more.
      (while (and (< n len) (not tried-each-ring-item)
		  (not (string-match regexp (ushell-get-history n))))
	(setq n (mod (+ n motion) len)
	      ;; If we have gone all the way around in this search.
	      tried-each-ring-item (= n prev)))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use, if we found it,
    ;; return that.
    (if (string-match regexp (ushell-get-history n))
	n)))

(defun ushell-previous-matching-input (regexp arg)
  "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (ushell-regexp-arg "Previous input matching (regexp): "))
  (setq arg (ushell-search-arg arg))
  (let ((pos (ushell-previous-matching-input-string-position regexp arg)))
    ;; Has a match been found?
    (if (null pos)
	(error "Not found")
      (setq ushell-history-index pos)
      (unless (minibuffer-window-active-p (selected-window))
	(message "History item: %d" (- (ring-length ushell-history-ring) pos)))
       ;; Can't use kill-region as it sets this-command
      (delete-region (save-excursion (ushell-bol) (point)) (point))
      (insert-and-inherit (ushell-get-history pos)))))

(defun ushell-next-matching-input (regexp arg)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (ushell-regexp-arg "Next input matching (regexp): "))
  (ushell-previous-matching-input regexp (- arg)))

(defun ushell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(ushell-previous-matching-input-from-input
				ushell-next-matching-input-from-input)))
      ;; Starting a new search
      (setq ushell-matching-input-from-input-string
	    (buffer-substring (save-excursion (ushell-bol) (point))
			      (point))
	    ushell-history-index nil))
  (ushell-previous-matching-input
   (concat "^" (regexp-quote ushell-matching-input-from-input-string))
   arg))

(defun ushell-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match."
  (interactive "p")
  (ushell-previous-matching-input-from-input (- arg)))

(defun ushell-test-imatch ()
  "If isearch match good, put point at the beginning and return non-nil."
  (if (get-text-property (point) 'history)
      (progn (beginning-of-line) t)
    (let ((before (point)))
      (ushell-bol)
      (if (and (not (bolp))
	       (<= (point) before))
	  t
	(if isearch-forward
	    (progn
	      (end-of-line)
	      (forward-char))
	  (beginning-of-line)
	  (backward-char))))))

(defun ushell-return-to-prompt ()
  "Once a search string matches, insert it at the end and go there."
  (setq isearch-other-end nil)
  (let ((found (ushell-test-imatch)) before)
    (while (and (not found)
		(setq before
		      (funcall (if isearch-forward
				   're-search-forward
				 're-search-backward)
			       isearch-string nil t)))
      (setq found (ushell-test-imatch)))
    (if (not found)
	(progn
	  (goto-char ushell-last-output-end)
	  (delete-region (point) (point-max)))
      (setq before (point))
      (let ((text (buffer-substring-no-properties
		   (point) (line-end-position)))
	    (orig (marker-position ushell-last-output-end)))
	(goto-char ushell-last-output-end)
	(delete-region (point) (point-max))
	(when (and text (> (length text) 0))
	  (insert text)
	  (put-text-property (1- (point)) (point)
			     'last-search-pos before)
	  (set-marker ushell-last-output-end orig)
	  (goto-char ushell-last-output-end))))))

(defun ushell-prepare-for-search ()
  "Make sure the old history file is at the beginning of the buffer."
  (unless (get-text-property (point-min) 'history)
    (save-excursion
      (goto-char (point-min))
      (let ((end (copy-marker (point) t)))
	(insert-file-contents ushell-history-file-name)
	(set-text-properties (point-min) end
			     '(history t invisible t))))))

(defun ushell-isearch-backward (&optional invert)
  "Do incremental regexp search backward through past commands."
  (interactive)
  (let ((inhibit-read-only t) end)
    (ushell-prepare-for-search)
    (goto-char (point-max))
    (set-marker ushell-last-output-end (point))
    (delete-region (point) (point-max)))
  (isearch-mode invert t 'ushell-return-to-prompt))

(defun ushell-isearch-repeat-backward (&optional invert)
  "Do incremental regexp search backward through past commands."
  (interactive)
  (let ((old-pos (get-text-property (1- (point-max))
				    'last-search-pos)))
    (when old-pos
      (goto-char old-pos)
      (if invert
	  (end-of-line)
	(backward-char)))
    (setq isearch-forward invert)
    (isearch-search-and-update)))

(defun ushell-isearch-forward ()
  "Do incremental regexp search backward through past commands."
  (interactive)
  (ushell-isearch-backward t))

(defun ushell-isearch-repeat-forward ()
  "Do incremental regexp search backward through past commands."
  (interactive)
  (ushell-isearch-repeat-backward t))

(defun ushell-isearch-cancel ()
  (interactive)
  (goto-char ushell-last-output-end)
  (delete-region (point) (point-max))
  (call-interactively 'isearch-cancel))

(defun ushell-isearch-abort ()
  (interactive)
  (goto-char ushell-last-output-end)
  (delete-region (point) (point-max))
  (call-interactively 'isearch-abort))

(defun ushell-isearch-delete-char ()
  (interactive)
  (save-excursion
  (isearch-delete-char)))

(defun ushell-isearch-return ()
  (interactive)
  (isearch-done)
  (usell-send-input))

;;; em-hist.el ends here
