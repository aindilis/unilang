;;; em-term.el --- running visual commands

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

(provide 'em-term)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-term nil
  "This module causes visual commands (e.g., 'vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `ushell-visual-commands' defines
which commands are considered visual in nature."
  :tag "Running visual commands"
  :group 'ushell-module)

;;; Commentary:

;; At the moment, ushell is stream-based in its interactive input and
;; output.  This means that full-screen commands, such as "vi" or
;; "lynx", will not display correctly.  These are therefore thought of
;; as "visual" programs.  In order to run these progrem under Emacs,
;; Ushell uses the term.el package, and invokes them in a separate
;; buffer, giving the illusion that Ushell itself is allowing these
;; visual processes to execute.

(require 'term)

;;; User Variables:

(defcustom ushell-term-load-hook '(ushell-term-initialize)
  "*A list of functions to call when loading `ushell-term'."
  :type 'hook
  :group 'ushell-term)

(defcustom ushell-visual-commands
  '("vi"                                ; what is going on??
    "screen" "top"                      ; ok, a valid program...
    "less" "more"                       ; M-x view-file
    "lynx" "ncftp"                      ; w3.el, ange-ftp
    "pine" "tin" "trn" "elm")           ; GNUS!!
  "*A list of commands that present their output in a visual fashion."
  :type '(repeat string)
  :group 'ushell-term)

(defcustom ushell-term-name "eterm"
  "*Name to use for the TERM variable when running visual commands.
See `term-term-name' in term.el for more information on how this is
used."
  :type 'string
  :group 'ushell-term)

(defcustom ushell-escape-control-x t
  "*If non-nil, allow <C-x> to be handled by Emacs key in visual buffers.
See the variable `ushell-visual-commands'.  If this variable is set to
nil, <C-x> will send that control character to the invoked process."
  :type 'boolean
  :group 'ushell-term)

;;; Internal Variables:

(defvar ushell-parent-buffer)

;;; Functions:

(defun ushell-term-initialize ()
  "Initialize the `term' interface code."
  (make-local-variable 'ushell-interpreter-alist)
  (setq ushell-interpreter-alist
	(cons (cons (function
		     (lambda (command)
		       (member (file-name-nondirectory command)
			       ushell-visual-commands)))
		    'ushell-exec-visual)
	      ushell-interpreter-alist)))

(defun ushell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* (ushell-interpreter-alist
	 (interp (ushell-find-interpreter (car args)))
	 (program (car interp))
	 (args (ushell-flatten-list
		(ushell-stringify-list (append (cdr interp)
					       (cdr args)))))
	 (term-buf
	  (generate-new-buffer
	   (concat "*" (file-name-nondirectory program) "*")))
	 (ushell-buf (current-buffer)))
    (save-current-buffer
      (switch-to-buffer term-buf)
      (term-mode)
      (set (make-local-variable 'term-term-name) ushell-term-name)
      (make-local-variable 'ushell-parent-buffer)
      (setq ushell-parent-buffer ushell-buf)
      (term-exec term-buf program program nil args)
      (let ((proc (get-buffer-process term-buf)))
	(if (and proc (eq 'run (process-status proc)))
	    (set-process-sentinel proc 'ushell-term-sentinel)
	  (error "Failed to invoke visual command")))
      (term-char-mode)
      (if ushell-escape-control-x
	  (term-set-escape-char ?\C-x))))
  nil)

(defun ushell-term-sentinel (proc string)
  "Destroy the buffer visiting PROC."
  (let ((proc-buf (process-buffer proc)))
    (when (and proc-buf (buffer-live-p proc-buf)
	       (not (eq 'run (process-status proc)))
	       (= (process-exit-status proc) 0))
      (if (eq (current-buffer) proc-buf)
	  (let ((buf (and (boundp 'ushell-parent-buffer)
			  ushell-parent-buffer
			  (buffer-live-p ushell-parent-buffer)
			  ushell-parent-buffer)))
	    (if buf
		(switch-to-buffer buf))))
      (kill-buffer proc-buf))))

;; jww (1999-09-17): The code below will allow Ushell to send input
;; characters directly to the currently running interactive process.
;; However, since this would introduce other problems that would need
;; solutions, I'm going to let it wait until after 2.1.

; (defvar ushell-term-raw-map nil
;   "Keyboard map for sending characters directly to the inferior process.")
; (defvar ushell-term-escape-char nil
;   "Escape character for char-sub-mode of term mode.
; Do not change it directly;  use term-set-escape-char instead.")
; (defvar ushell-term-raw-escape-map nil)

; (defun ushell-term-send-raw-string (chars)
;   (goto-char ushell-last-output-end)
;   (process-send-string (ushell-interactive-process) chars))

; (defun ushell-term-send-raw ()
;   "Send the last character typed through the terminal-emulator
; without any interpretation."
;   (interactive)
;   ;; Convert `return' to C-m, etc.
;   (if (and (symbolp last-input-char)
;	   (get last-input-char 'ascii-character))
;       (setq last-input-char (get last-input-char 'ascii-character)))
;   (ushell-term-send-raw-string (make-string 1 last-input-char)))

; (defun ushell-term-send-raw-meta ()
;   (interactive)
;   (if (symbolp last-input-char)
;       ;; Convert `return' to C-m, etc.
;       (let ((tmp (get last-input-char 'event-symbol-elements)))
;	(if tmp
;	    (setq last-input-char (car tmp)))
;	(if (symbolp last-input-char)
;	    (progn
;	      (setq tmp (get last-input-char 'ascii-character))
;	      (if tmp (setq last-input-char tmp))))))
;   (ushell-term-send-raw-string (if (and (numberp last-input-char)
;					(> last-input-char 127)
;					(< last-input-char 256))
;				   (make-string 1 last-input-char)
;				 (format "\e%c" last-input-char))))

; (defun ushell-term-mouse-paste (click arg)
;   "Insert the last stretch of killed text at the position clicked on."
;   (interactive "e\nP")
;   (if (boundp 'xemacs-logo)
;       (ushell-term-send-raw-string
;        (or (condition-case () (x-get-selection) (error ()))
;	   (x-get-cutbuffer)
;	   (error "No selection or cut buffer available")))
;     ;; Give temporary modes such as isearch a chance to turn off.
;     (run-hooks 'mouse-leave-buffer-hook)
;     (setq this-command 'yank)
;     (ushell-term-send-raw-string
;      (current-kill (cond ((listp arg) 0)
;			 ((eq arg '-) -1)
;			 (t (1- arg)))))))

; ;; Which would be better:  "\e[A" or "\eOA"? readline accepts either.
; ;; For my configuration it's definitely better \eOA but YMMV. -mm
; ;; For example: vi works with \eOA while elm wants \e[A ...
; (defun ushell-term-send-up    () (interactive) (ushell-term-send-raw-string "\eOA"))
; (defun ushell-term-send-down  () (interactive) (ushell-term-send-raw-string "\eOB"))
; (defun ushell-term-send-right () (interactive) (ushell-term-send-raw-string "\eOC"))
; (defun ushell-term-send-left  () (interactive) (ushell-term-send-raw-string "\eOD"))
; (defun ushell-term-send-home  () (interactive) (ushell-term-send-raw-string "\e[1~"))
; (defun ushell-term-send-end   () (interactive) (ushell-term-send-raw-string "\e[4~"))
; (defun ushell-term-send-prior () (interactive) (ushell-term-send-raw-string "\e[5~"))
; (defun ushell-term-send-next  () (interactive) (ushell-term-send-raw-string "\e[6~"))
; (defun ushell-term-send-del   () (interactive) (ushell-term-send-raw-string "\C-?"))
; (defun ushell-term-send-backspace  () (interactive) (ushell-term-send-raw-string "\C-H"))

; (defun ushell-term-set-escape-char (c)
;   "Change term-escape-char and keymaps that depend on it."
;   (if ushell-term-escape-char
;       (define-key ushell-term-raw-map ushell-term-escape-char 'ushell-term-send-raw))
;   (setq c (make-string 1 c))
;   (define-key ushell-term-raw-map c ushell-term-raw-escape-map)
;   ;; Define standard bindings in ushell-term-raw-escape-map
;   (define-key ushell-term-raw-escape-map "\C-x"
;     (lookup-key (current-global-map) "\C-x"))
;   (define-key ushell-term-raw-escape-map "\C-v"
;     (lookup-key (current-global-map) "\C-v"))
;   (define-key ushell-term-raw-escape-map "\C-u"
;     (lookup-key (current-global-map) "\C-u"))
;   (define-key ushell-term-raw-escape-map c 'ushell-term-send-raw))

; (defun ushell-term-char-mode ()
;   "Switch to char (\"raw\") sub-mode of term mode.
; Each character you type is sent directly to the inferior without
; intervention from Emacs, except for the escape character (usually C-c)."
;   (interactive)
;   (if (not ushell-term-raw-map)
;       (let* ((map (make-keymap))
;	     (esc-map (make-keymap))
;	     (i 0))
;	(while (< i 128)
;	  (define-key map (make-string 1 i) 'ushell-term-send-raw)
;	  (define-key esc-map (make-string 1 i) 'ushell-term-send-raw-meta)
;	  (setq i (1+ i)))
;	(define-key map "\e" esc-map)
;	(setq ushell-term-raw-map map)
;	(setq ushell-term-raw-escape-map
;	      (copy-keymap (lookup-key (current-global-map) "\C-x")))
;	(if (boundp 'xemacs-logo)
;	    (define-key ushell-term-raw-map [button2] 'ushell-term-mouse-paste)
;	  (define-key ushell-term-raw-map [mouse-2] 'ushell-term-mouse-paste))
;	(define-key ushell-term-raw-map [up] 'ushell-term-send-up)
;	(define-key ushell-term-raw-map [down] 'ushell-term-send-down)
;	(define-key ushell-term-raw-map [right] 'ushell-term-send-right)
;	(define-key ushell-term-raw-map [left] 'ushell-term-send-left)
;	(define-key ushell-term-raw-map [delete] 'ushell-term-send-del)
;	(define-key ushell-term-raw-map [backspace] 'ushell-term-send-backspace)
;	(define-key ushell-term-raw-map [home] 'ushell-term-send-home)
;	(define-key ushell-term-raw-map [end] 'ushell-term-send-end)
;	(define-key ushell-term-raw-map [prior] 'ushell-term-send-prior)
;	(define-key ushell-term-raw-map [next] 'ushell-term-send-next)
;	(ushell-term-set-escape-char ?\C-c))))

; (defun ushell-term-line-mode  ()
;   "Switch to line (\"cooked\") sub-mode of usell-term mode."
;  (use-local-map term-old-mode-map))

;;; Code:

;;; em-term.el ends here
