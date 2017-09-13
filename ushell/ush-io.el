;;; ush-io.el --- I/O management

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

(provide 'ush-io)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-io nil
  "Ushell's I/O management code provides a scheme for treating many
different kinds of objects -- symbols, files, buffers, etc. -- as
though they were files."
  :tag "I/O management"
  :group 'ushell)

;;; Commentary:

;; At the moment, only output redirection is supported in Ushell.  To
;; use input redirection, the following syntax will work, assuming
;; that the command after the pipe is always an external command:
;;
;;   cat <file> | <command>
;;
;; Otherwise, output redirection and piping are provided in a manner
;; consistent with most shells.  Therefore, only unique features are
;; mentioned here.
;;
;;;_* Insertion
;;
;; To insert at the location of point in a buffer, use '>>>':
;;
;;   echo alpha >>> #<buffer *scratch*>;
;;
;;;_* Pseudo-devices
;;
;; A few pseudo-devices are provided, since Emacs cannot write
;; directly to a UNIX device file:
;;
;;   echo alpha > /dev/null   ; the bit bucket
;;   echo alpha > /dev/kill   ; set the kill ring
;;   echo alpha >> /dev/clip  ; append to the clipboard
;;
;;;_* Multiple output targets
;;
;; Ushell can write to multiple output targets, including pipes.
;; Example:
;;
;;   (+ 1 2) > a > b > c   ; prints number to all three files
;;   (+ 1 2) > a | wc      ; prints to 'a', and pipes to 'wc'

;;; User Variables:

(defcustom ushell-io-load-hook '(ushell-io-initialize)
  "*A hook that gets run when `ushell-io' is loaded."
  :type 'hook
  :group 'ushell-io)

(defcustom ushell-number-of-handles 3
  "*The number of file handles that ushell supports.
Currently this is standard input, output and error.  But even all of
these Emacs does not currently support with asynchronous processes
\(which is what ushell uses so that you can continue doing work in
other buffers) ."
  :type 'integer
  :group 'ushell-io)

(defcustom ushell-output-handle 1
  "*The index of the standard output handle."
  :type 'integer
  :group 'ushell-io)

(defcustom ushell-error-handle 2
  "*The index of the standard error handle."
  :type 'integer
  :group 'ushell-io)

(defcustom ushell-buffer-shorthand nil
  "*If non-nil, a symbol name can be used for a buffer in redirection.
If nil, redirecting to a buffer requires buffer name syntax.  If this
variable is set, redirection directly to Lisp symbols will be
impossible.

Example:

  echo hello > '*scratch*  ; works if `ushell-buffer-shorthand' is t
  echo hello > #<buffer *scratch*>  ; always works"
  :type 'boolean
  :group 'ushell-io)

(defcustom ushell-print-queue-size 5
  "*The size of the print queue, for doing buffered printing.
This is basically a speed enhancement, to avoid blocking the Lisp code
from executing while Emacs is redisplaying."
  :type 'integer
  :group 'ushell-io)

(defcustom ushell-virtual-targets
  '(("/dev/ushell" ushell-interactive-print nil)
    ("/dev/kill" (lambda (mode)
		   (if (eq mode 'overwrite)
		       (kill-new ""))
		   'ushell-kill-append) t)
    ("/dev/clip" (lambda (mode)
		   (if (eq mode 'overwrite)
		       (let ((x-select-enable-clipboard t))
			 (kill-new "")))
		   'ushell-clipboard-append) t))
  "*Map virtual devices name to Emacs Lisp functions.
If the user specifies any of the filenames above as a redirection
target, the function in the second element will be called.

If the third element is non-nil, the redirection mode is passed as an
argument (which is the symbol `overwrite', `append' or `insert'), and
the function is expected to return another function -- which is the
output function.  Otherwise, the second element itself is the output
function.

The output function is then called repeatedly with single strings,
which represents successive pieces of the output of the command, until nil
is passed, meaning EOF.

NOTE: /dev/null is handled specially as a virtual target, and should
not be added to this variable."
  :type '(repeat
	  (list (string :tag "Target")
		function
		(choice (const :tag "Func returns output-func" t)
			(const :tag "Func is output-func" nil))))
  :group 'ushell-io)

(put 'ushell-virtual-targets 'risky-local-variable t)

;;; Internal Variables:

(defvar ushell-current-handles nil)

(defvar ushell-last-command-status 0
  "The exit code from the last command.  0 if successful.")

(defvar ushell-last-command-result nil
  "The result of the last command.  Not related to success.")

(defvar ushell-output-file-buffer nil
  "If non-nil, the current buffer is a file output buffer.")

(defvar ushell-print-count)
(defvar ushell-current-redirections)

;;; Functions:

(defun ushell-io-initialize ()
  "Initialize the I/O subsystem code."
  (make-local-hook 'ushell-parse-argument-hook)
  (add-hook 'ushell-parse-argument-hook
	    'ushell-parse-redirection nil t)
  (make-local-variable 'ushell-current-redirections)
  (make-local-hook 'ushell-pre-rewrite-command-hook)
  (add-hook 'ushell-pre-rewrite-command-hook
	    'ushell-strip-redirections nil t)
  (make-local-hook 'ushell-post-rewrite-command-hook)
  (add-hook 'ushell-post-rewrite-command-hook
	    'ushell-apply-redirections nil t))

(defun ushell-parse-redirection ()
  "Parse an output redirection, such as '2>'."
  (if (and (not ushell-current-quoted)
	   (looking-at "\\([0-9]\\)?\\(<\\|>+\\)&?\\([0-9]\\)?\\s-*"))
      (if ushell-current-argument
	  (ushell-finish-arg)
	(let ((sh (match-string 1))
	      (oper (match-string 2))
;	      (th (match-string 3))
	      )
	  (if (string= oper "<")
	      (error "Ushell does not support input redirection"))
	  (ushell-finish-arg
	   (prog1
	       (list 'ushell-set-output-handle
		     (or (and sh (string-to-int sh)) 1)
		     (list 'quote
			   (aref [overwrite append insert]
				 (1- (length oper)))))
	     (goto-char (match-end 0))))))))

(defun ushell-strip-redirections (terms)
  "Rewrite any output redirections in TERMS."
  (setq ushell-current-redirections (list t))
  (let ((tl terms)
	(tt (cdr terms)))
    (while tt
      (if (not (and (consp (car tt))
		    (eq (caar tt) 'ushell-set-output-handle)))
	  (setq tt (cdr tt)
		tl (cdr tl))
	(unless (cdr tt)
	  (error "Missing redirection target"))
	(nconc ushell-current-redirections
	       (list (list 'ignore
			   (append (car tt) (list (cadr tt))))))
	(setcdr tl (cddr tt))
	(setq tt (cddr tt))))
    (setq ushell-current-redirections
	  (cdr ushell-current-redirections))))

(defun ushell-apply-redirections (cmdsym)
  "Apply any redirection which were specified for COMMAND."
  (if ushell-current-redirections
      (set cmdsym
	   (append (list 'progn)
		   ushell-current-redirections
		   (list (symbol-value cmdsym))))))

(defun ushell-create-handles
  (standard-output output-mode &optional standard-error error-mode)
  "Create a new set of file handles for a command.
The default location for standard output and standard error will go to
STANDARD-OUTPUT and STANDARD-ERROR, respectively.
OUTPUT-MODE and ERROR-MODE are either `overwrite', `append' or `insert';
a nil value of mode defaults to `insert'."
  (let ((handles (make-vector ushell-number-of-handles nil))
	(output-target (ushell-get-target standard-output output-mode))
	(error-target (ushell-get-target standard-error error-mode)))
    (aset handles ushell-output-handle (cons output-target 1))
    (if standard-error
	(aset handles ushell-error-handle (cons error-target 1))
      (aset handles ushell-error-handle (cons output-target 1)))
    handles))

(defun ushell-protect-handles (handles)
  "Protect the handles in HANDLES from a being closed."
  (let ((idx 0))
    (while (< idx ushell-number-of-handles)
      (if (aref handles idx)
	  (setcdr (aref handles idx)
		  (1+ (cdr (aref handles idx)))))
      (setq idx (1+ idx))))
  handles)

(defun ushell-close-target (target status)
  "Close an output TARGET, passing STATUS as the result.
STATUS should be non-nil on successful termination of the output."
  (cond
   ((symbolp target) nil)

   ;; If we were redirecting to a file, save the file and close the
   ;; buffer.
   ((markerp target)
    (let ((buf (marker-buffer target)))
      (when buf                         ; somebody's already killed it!
	(save-current-buffer
	  (set-buffer buf)
	  (when ushell-output-file-buffer
	    (save-buffer)
	    (when (eq ushell-output-file-buffer t)
	      (or status (set-buffer-modified-p nil))
	      (kill-buffer buf)))))))

   ;; If we're redirecting to a process (via a pipe, or process
   ;; redirection), send it EOF so that it knows we're finished.
   ((ushell-processp target)
    (if (eq (process-status target) 'run)
	(process-send-eof target)))

   ;; A plain function redirection needs no additional arguments
   ;; passed.
   ((functionp target)
    (funcall target status))

   ;; But a more complicated function redirection (which can only
   ;; happen with aliases at the moment) has arguments that need to be
   ;; passed along with it.
   ((consp target)
    (apply (car target) status (cdr target)))))

(defun ushell-close-handles (exit-code &optional result handles)
  "Close all of the current handles, taking refcounts into account.
EXIT-CODE is the process exit code; mainly, it is zero, if the command
completed successfully.  RESULT is the quoted value of the last
command.  If nil, then the meta variables for keeping track of the
last execution result should not be changed."
  (let ((idx 0))
    (assert (or (not result) (eq (car result) 'quote)))
    (setq ushell-last-command-status exit-code
	  ushell-last-command-result (cadr result))
    (while (< idx ushell-number-of-handles)
      (let ((handles (or handles ushell-current-handles)))
	(when (aref handles idx)
	  (setcdr (aref handles idx)
		  (1- (cdr (aref handles idx))))
	  (when (= (cdr (aref handles idx)) 0)
	    (let ((target (car (aref handles idx))))
	      (if (not (listp target))
		  (ushell-close-target target (= exit-code 0))
		(while target
		  (ushell-close-target (car target) (= exit-code 0))
		  (setq target (cdr target)))))
	    (setcar (aref handles idx) nil))))
      (setq idx (1+ idx)))
    nil))

(defun ushell-kill-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (kill-append string nil)))

(defun ushell-clipboard-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (let ((x-select-enable-clipboard t))
	(kill-append string nil))))

(defun ushell-get-target (target &optional mode)
  "Convert TARGET, which is a raw argument, into a valid output target.
MODE is either `overwrite', `append' or `insert'; if it is omitted or nil,
it defaults to `insert'."
  (setq mode (or mode 'insert))
  (cond
   ((stringp target)
    (let ((redir (assoc target ushell-virtual-targets)))
     (if redir
	 (if (nth 2 redir)
	     (funcall (nth 1 redir) mode)
	   (nth 1 redir))
       (let* ((exists (get-file-buffer target))
	      (buf (find-file-noselect target t)))
	 (with-current-buffer buf
	   (if buffer-read-only
	       (error "Cannot write to read-only file `%s'" target))
	   (set (make-local-variable 'ushell-output-file-buffer)
		(if (eq exists buf) 0 t))
	   (cond ((eq mode 'overwrite)
		  (erase-buffer))
		 ((eq mode 'append)
		  (goto-char (point-max))))
	   (point-marker))))))
   ((or (bufferp target)
	(and (boundp 'ushell-buffer-shorthand)
	     (symbol-value 'ushell-buffer-shorthand)
	     (symbolp target)))
    (let ((buf (if (bufferp target)
		   target
		 (get-buffer-create
		  (symbol-name target)))))
      (with-current-buffer buf
	(cond ((eq mode 'overwrite)
	       (erase-buffer))
	      ((eq mode 'append)
	       (goto-char (point-max))))
	(point-marker))))
   ((functionp target)
    nil)
   ((symbolp target)
    (if (eq mode 'overwrite)
	(set target nil))
    target)
   ((or (ushell-processp target)
	(markerp target))
    target)
   (t
    (error "Illegal redirection target: %s"
	   (ushell-stringify target)))))

(eval-when-compile
  (defvar grep-null-device))

(defun ushell-set-output-handle (index mode &optional target)
  "Set handle INDEX, using MODE, to point to TARGET."
  (when target
    (if (and (stringp target)
	     (or (cond
		  ((boundp 'null-device)
		   (string= target null-device))
		  ((boundp 'grep-null-device)
		   (string= target grep-null-device))
		  (t nil))
		 (string= target "/dev/null")))
	(aset ushell-current-handles index nil)
      (let ((where (ushell-get-target target mode))
	    (current (car (aref ushell-current-handles index))))
	(if (and (listp current)
		 (not (member where current)))
	    (setq current (append current (list where)))
	  (setq current where))
	(if (not (aref ushell-current-handles index))
	    (aset ushell-current-handles index (cons nil 1)))
	(setcar (aref ushell-current-handles index) current)))))

(defun ushell-interactive-output-p ()
  "Return non-nil if current handles are bound for interactive display."
  (and (eq (car (aref ushell-current-handles
		      ushell-output-handle)) t)
       (eq (car (aref ushell-current-handles
		      ushell-error-handle)) t)))

(defvar ushell-print-queue nil)
(defvar ushell-print-queue-count -1)

(defun ushell-flush (&optional reset-p)
  "Flush out any lines that have been queued for printing.
Must be called before printing begins with -1 as its argument, and
after all printing is over with no argument."
  (ignore
   (if reset-p
       (setq ushell-print-queue nil
	     ushell-print-queue-count reset-p)
     (if ushell-print-queue
	 (ushell-print ushell-print-queue))
     (ushell-flush 0))))

(defun ushell-init-print-buffer ()
  "Initialize the buffered printing queue."
  (ushell-flush -1))

(defun ushell-buffered-print (&rest strings)
  "A buffered print -- *for strings only*."
  (if (< ushell-print-queue-count 0)
      (progn
	(ushell-print (apply 'concat strings))
	(setq ushell-print-queue-count 0))
    (if (= ushell-print-queue-count ushell-print-queue-size)
	(ushell-flush))
    (setq ushell-print-queue
	  (concat ushell-print-queue (apply 'concat strings))
	  ushell-print-queue-count (1+ ushell-print-queue-count))))

(defsubst ushell-print (object)
  "Output OBJECT to the standard output handle."
  (ushell-output-object object ushell-output-handle))

(defsubst ushell-error (object)
  "Output OBJECT to the standard error handle."
  (ushell-output-object object ushell-error-handle))

(defsubst ushell-errorn (object)
  "Output OBJECT followed by a newline to the standard error handle."
  (ushell-error object)
  (ushell-error "\n"))

(defsubst ushell-printn (object)
  "Output OBJECT followed by a newline to the standard output handle."
  (ushell-print object)
  (ushell-print "\n"))

(defun ushell-output-object-to-target (object target)
  "Insert OBJECT into TARGET.
Returns what was actually sent, or nil if nothing was sent."
  (cond
   ((functionp target)
    (funcall target object))

   ((symbolp target)
    (if (eq target t)                   ; means "print to display"
	(ushell-output-filter nil (ushell-stringify object))
      (if (not (symbol-value target))
	  (set target object)
	(setq object (ushell-stringify object))
	(if (not (stringp (symbol-value target)))
	    (set target (ushell-stringify
			 (symbol-value target))))
	(set target (concat (symbol-value target) object)))))

   ((markerp target)
    (if (buffer-live-p (marker-buffer target))
	(with-current-buffer (marker-buffer target)
	  (let ((moving (= (point) target)))
	    (save-excursion
	      (goto-char target)
	      (setq object (ushell-stringify object))
	      (insert-and-inherit object)
	      (set-marker target (point-marker)))
	    (if moving
		(goto-char target))))))

   ((ushell-processp target)
    (when (eq (process-status target) 'run)
      (setq object (ushell-stringify object))
      (process-send-string target object)))

   ((consp target)
    (apply (car target) object (cdr target))))
  object)

(defun ushell-output-object (object &optional handle-index handles)
  "Insert OBJECT, using HANDLE-INDEX specifically)."
  (let ((target (car (aref (or handles ushell-current-handles)
			   (or handle-index ushell-output-handle)))))
    (if (and target (not (listp target)))
	(ushell-output-object-to-target object target)
      (while target
	(ushell-output-object-to-target object (car target))
	(setq target (cdr target))))))

;;; Code:

;;; us-io.el ends here
