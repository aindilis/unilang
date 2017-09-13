;;; ush-proc.el --- process management

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

(provide 'ush-proc)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-proc nil
  "When Ushell invokes external commands, it always does so
asynchronously, so that Emacs isn't tied up waiting for the process to
finish."
  :tag "Process management"
  :group 'ushell)

;;; Commentary:

;;; User Variables:

(defcustom ushell-proc-load-hook '(ushell-proc-initialize)
  "*A hook that gets run when `ushell-proc' is loaded."
  :type 'hook
  :group 'ushell-proc)

(defcustom ushell-process-wait-seconds 0
  "*The number of seconds to delay waiting for a synchronous process."
  :type 'integer
  :group 'ushell-proc)

(defcustom ushell-process-wait-milliseconds 50
  "*The number of milliseconds to delay waiting for a synchronous process."
  :type 'integer
  :group 'ushell-proc)

(defcustom ushell-done-messages-in-minibuffer t
  "*If non-nil, subjob \"Done\" messages will display in minibuffer."
  :type 'boolean
  :group 'ushell-proc)

(defcustom ushell-delete-exited-processes t
  "*If nil, process entries will stick around until `jobs' is run.
This variable sets the buffer-local value of `delete-exited-processes'
in Ushell buffers.

This variable causes Ushell to mimic the behavior of bash when set to
nil.  It allows the user to view the exit status of a completed subjob
\(process) at their leisure, because the process entry remains in
memory until the user examines it using \\[list-processes].

Otherwise, if `ushell-done-messages-in-minibuffer' is nil, and this
variable is set to t, the only indication the user will have that a
subjob is done is that it will no longer appear in the
\\[list-processes\\] display.

Note that Ushell will have to be restarted for a change in this
variable's value to take effect."
  :type 'boolean
  :group 'ushell-proc)

(defcustom ushell-reset-signals
  "^\\(interrupt\\|killed\\|quit\\|stopped\\)"
  "*If a termination signal matches this regexp, the terminal will be reset."
  :type 'regexp
  :group 'ushell-proc)

(defcustom ushell-exec-hook nil
  "*Called each time a process is exec'd by `ushell-gather-process-output'.
It is passed one argument, which is the process that was just started.
It is useful for things that must be done each time a process is
executed in a ushell mode buffer (e.g., `process-kill-without-query').
In contrast, `ushell-mode-hook' is only executed once when the buffer
is created."
  :type 'hook
  :group 'ushell-proc)

(defcustom ushell-kill-hook '(ushell-reset-after-proc)
  "*Called when a process run by `ushell-gather-process-output' has ended.
It is passed two arguments: the process that was just ended, and the
termination status (as a string).  Note that the first argument may be
nil, in which case the user attempted to send a signal, but there was
no relevant process.  This can be used for displaying help
information, for example."
  :type 'hook
  :group 'ushell-proc)

;;; Internal Variables:

(defvar ushell-current-subjob-p nil)

(defvar ushell-process-list nil
  "A list of the current status of subprocesses.")

;;; Functions:

(defun ushell-proc-initialize ()
  "Initialize the process handling code."
  (make-local-variable 'ushell-process-list)
  (define-key ushell-command-map [(meta ?i)] 'ushell-insert-process)
  (define-key ushell-command-map [(control ?c)]  'ushell-interrupt-process)
  (define-key ushell-command-map [(control ?k)]  'ushell-kill-process)
  (define-key ushell-command-map [(control ?d)]  'ushell-send-eof-to-process)
; (define-key ushell-command-map [(control ?q)]  'ushell-continue-process)
  (define-key ushell-command-map [(control ?s)]  'list-processes)
; (define-key ushell-command-map [(control ?z)]  'ushell-stop-process)
  (define-key ushell-command-map [(control ?\\)] 'ushell-quit-process))

(defun ushell-reset-after-proc (proc status)
  "Reset the command input location after a process terminates.
The signals which will cause this to happen are matched by
`ushell-reset-signals'."
  (if (and (stringp status)
	   (string-match ushell-reset-signals status))
      (ushell-reset)))

(defun ushell-wait-for-process (&rest procs)
  "Wait until PROC has successfully completed."
  (while procs
    (let ((proc (car procs)))
      (when (ushell-processp proc)
	;; NYI: If the process gets stopped here, that's bad.
	(while (assq proc ushell-process-list)
	  (if (input-pending-p)
	      (discard-input))
	  (sit-for ushell-process-wait-seconds
		   ushell-process-wait-milliseconds))))
    (setq procs (cdr procs))))

(defalias 'ushell/wait 'ushell-wait-for-process)

(defun ushell/jobs (&rest args)
  "List processes, if there are any."
  (and (fboundp 'process-list)
       (process-list)
       (list-processes)))

(defun ushell/kill (&rest args)
  "Kill processes, buffers, symbol or files."
  (let ((ptr args)
	(signum 'SIGINT))
    (while ptr
      (if (or (ushell-processp (car ptr))
	      (and (stringp (car ptr))
		   (string-match "^[A-Za-z/][A-Za-z0-9<>/]+$"
				 (car ptr))))
	  ;; What about when $lisp-variable is possible here?
	  ;; It could very well name a process.
	  (setcar ptr (get-process (car ptr))))
      (setq ptr (cdr ptr)))
    (while args
      (let ((id (if (ushell-processp (car args))
		    (process-id (car args))
		  (car args))))
	(when id
	  (cond
	   ((null id)
	    (error "kill: bad signal spec"))
	   ((and (numberp id) (= id 0))
	    (error "kill: bad signal spec `%d'" id))
	   ((and (stringp id)
		 (string-match "^-?[0-9]+$" id))
	    (setq signum (abs (string-to-number id))))
	   ((stringp id)
	    (let (case-fold-search)
	      (if (string-match "^-\\([A-Z]+\\)$" id)
		  (setq signum
			(intern (concat "SIG" (match-string 1 id))))
		(error "kill: bad signal spec `%s'" id))))
	   ((< id 0)
	    (setq signum (abs id)))
	   (t
	    (signal-process id signum)))))
      (setq args (cdr args)))
    nil))

(defun ushell-read-process-name (prompt)
  "Read the name of a process from the minibuffer, using completion.
The prompt will be set to PROMPT."
  (completing-read prompt
		   (mapcar
		    (function
		     (lambda (proc)
		       (cons (process-name proc) t)))
		    (process-list)) nil t))

(defun ushell-insert-process (process)
  "Insert the name of PROCESS into the current buffer at point."
  (interactive
   (list (get-process
	  (ushell-read-process-name "Name of process: "))))
  (insert-and-inherit "#<process " (process-name process) ">"))

(defsubst ushell-record-process-object (object)
  "Record OBJECT as now running."
  (if (and (ushell-processp object)
	   ushell-current-subjob-p)
      (ushell-interactive-print
       (format "[%s] %d\n" (process-name object) (process-id object))))
  (setq ushell-process-list
	(cons (list object ushell-current-handles
		    ushell-current-subjob-p nil nil)
	      ushell-process-list)))

(defun ushell-remove-process-entry (entry)
  "Record the process ENTRY as fully completed."
  (if (and (ushell-processp (car entry))
	   (nth 2 entry)
	   ushell-done-messages-in-minibuffer)
      (message (format "[%s]+ Done %s" (process-name (car entry))
		       (process-command (car entry)))))
  (setq ushell-process-list
	(delq entry ushell-process-list)))

(defvar ushell-scratch-buffer " *ushell-scratch*"
  "Scratch buffer for holding Ushell's input/output.")
(defvar ushell-last-sync-output-start nil
  "A marker that tracks the beginning of output of the last subprocess.
Used only on systems which do not support async subprocesses.")

(defun ushell-gather-process-output (command args)
  "Gather the output from COMMAND + ARGS."
  (unless (and (file-executable-p command)
	       (file-regular-p command))
    (error "%s: not an executable file" command))
  (let* ((delete-exited-processes
	  (if ushell-current-subjob-p
	      ushell-delete-exited-processes
	    delete-exited-processes))
	 (process-environment (ushell-environment-variables))
	 proc decoding encoding changed)
    (cond
     ((fboundp 'start-process)
      (setq proc
	    (apply 'start-process
		   (file-name-nondirectory command) nil
		   ;; `start-process' can't deal with relative
		   ;; filenames
		   (append (list (expand-file-name command)) args)))
      (ushell-record-process-object proc)
      (set-process-buffer proc (current-buffer))
      (if (ushell-interactive-output-p)
	  (set-process-filter proc 'ushell-output-filter)
	(set-process-filter proc 'ushell-insertion-filter))
      (set-process-sentinel proc 'ushell-sentinel)
      (run-hook-with-args 'ushell-exec-hook proc)
      (when (fboundp 'process-coding-system)
	(let ((coding-systems (process-coding-system proc)))
	  (setq decoding (car coding-systems)
		encoding (cdr coding-systems)))
	;; If start-process decided to use some coding system for
	;; decoding data sent from the process and the coding system
	;; doesn't specify EOL conversion, we had better convert CRLF
	;; to LF.
	(if (vectorp (coding-system-eol-type decoding))
	    (setq decoding (coding-system-change-eol-conversion decoding 'dos)
		  changed t))
	;; Even if start-process left the coding system for encoding
	;; data sent from the process undecided, we had better use the
	;; same one as what we use for decoding.  But, we should
	;; suppress EOL conversion.
	(if (and decoding (not encoding))
	    (setq encoding (coding-system-change-eol-conversion decoding 'unix)
		  changed t))
	(if changed
	    (set-process-coding-system proc decoding encoding))))
     (t
      ;; No async subprocesses...
      (let ((oldbuf (current-buffer))
	    (interact-p (ushell-interactive-output-p))
	    lbeg lend line proc-buf exit-status)
	(and (not (markerp ushell-last-sync-output-start))
	     (setq ushell-last-sync-output-start (point-marker)))
	(setq proc-buf
	      (set-buffer (get-buffer-create ushell-scratch-buffer)))
	(erase-buffer)
	(set-buffer oldbuf)
	(run-hook-with-args 'ushell-exec-hook command)
	(setq exit-status
	      (apply 'call-process-region
		     (append (list ushell-last-sync-output-start (point)
				   command t
				   ushell-scratch-buffer nil)
			     args)))
	;; When in a pipeline, record the place where the output of
	;; this process will begin.
	(and ushell-in-pipeline-p
	     (set-marker ushell-last-sync-output-start (point)))
	;; Simulate the effect of the process filter.
	(when (numberp exit-status)
	  (set-buffer proc-buf)
	  (goto-char (point-min))
	  (setq lbeg (point))
	  (while (eq 0 (forward-line 1))
	    (setq lend (point)
		  line (buffer-substring-no-properties lbeg lend))
	    (set-buffer oldbuf)
	    (if interact-p
		(ushell-output-filter nil line)
	      (ushell-output-object line))
	    (setq lbeg lend)
	    (set-buffer proc-buf))
	  (set-buffer oldbuf))
	(ushell-update-markers ushell-last-output-end)
	;; Simulate the effect of ushell-sentinel.
	(ushell-close-handles (if (numberp exit-status) exit-status -1))
	(run-hook-with-args 'ushell-kill-hook command exit-status)
	(or ushell-in-pipeline-p
	    (setq ushell-last-sync-output-start nil))
	(if (not (numberp exit-status))
	  (error "%s: external command failed: %s" command exit-status))
	(setq proc t))))
    proc))

(defun ushell-insertion-filter (proc string)
  "Insert a string into the ushell buffer, or a process/file/buffer.
PROC is the process for which we're inserting output.  STRING is the
output."
  (when (buffer-live-p (process-buffer proc))
    (set-buffer (process-buffer proc))
    (let ((entry (assq proc ushell-process-list)))
      (when entry
	(setcar (nthcdr 3 entry)
		(concat (nth 3 entry) string))
	(unless (nth 4 entry)           ; already being handled?
	  (while (nth 3 entry)
	    (let ((data (nth 3 entry)))
	      (setcar (nthcdr 3 entry) nil)
	      (setcar (nthcdr 4 entry) t)
	      (ushell-output-object data nil (cadr entry))
	      (setcar (nthcdr 4 entry) nil))))))))

(defun ushell-sentinel (proc string)
  "Generic sentinel for command processes.  Reports only signals.
PROC is the process that's exiting.  STRING is the exit message."
  (when (buffer-live-p (process-buffer proc))
    (set-buffer (process-buffer proc))
    (unwind-protect
	(let* ((entry (assq proc ushell-process-list)))
;	  (if (not entry)
;	      (error "Sentinel called for unowned process `%s'"
;		     (process-name proc))
	  (when entry
	    (unwind-protect
		(progn
		  (unless (string= string "run")
		    (unless (string-match "^\\(finished\\|exited\\)" string)
		      (ushell-insertion-filter proc string))
		    (ushell-close-handles (process-exit-status proc) 'nil
					  (cadr entry))))
	      (ushell-remove-process-entry entry))))
      (run-hook-with-args 'ushell-kill-hook proc string))))

(defun ushell-process-interact (func &optional all query)
  "Interact with a process, using PROMPT if more than one, via FUNC.
If ALL is non-nil, background processes will be interacted with as well.
If QUERY is non-nil, query the user with QUERY before calling FUNC."
  (let (defunct result)
    (ushell-for entry ushell-process-list
      (if (and (memq (process-status (car entry))
		    '(run stop open closed))
	       (or all
		   (not (nth 2 entry)))
	       (or (not query)
		   (y-or-n-p (format query (process-name (car entry))))))
	  (setq result (funcall func (car entry))))
      (unless (memq (process-status (car entry))
		    '(run stop open closed))
	(setq defunct (cons entry defunct))))
    ;; clean up the process list; this can get dirty if an error
    ;; occurred that brought the user into the debugger, and then they
    ;; quit, so that the sentinel was never called.
    (ushell-for d defunct
      (ushell-remove-process-entry d))
    result))

(defcustom ushell-kill-process-wait-time 5
  "*Seconds to wait between sending termination signals to a subprocess."
  :type 'integer
  :group 'ushell-proc)

(defcustom ushell-kill-process-signals '(SIGINT SIGQUIT SIGKILL)
  "*Signals used to kill processes when an Ushell buffer exits.
Ushell calls each of these signals in order when an Ushell buffer is
killed; if the process is still alive afterwards, Ushell waits a
number of seconds defined by `ushell-kill-process-wait-time', and
tries the next signal in the list."
  :type '(repeat symbol)
  :group 'ushell-proc)

(defcustom ushell-kill-processes-on-exit nil
  "*If non-nil, kill active processes when exiting an Ushell buffer.
Emacs will only kill processes owned by that Ushell buffer.

If nil, ownership of background and foreground processes reverts to
Emacs itself, and will die only if the user exits Emacs, calls
`kill-process', or terminates the processes externally.

If `ask', Emacs prompts the user before killing any processes.

If `every', it prompts once for every process.

If t, it kills all buffer-owned processes without asking.

Processes are first sent SIGHUP, then SIGINT, then SIGQUIT, then
SIGKILL.  The variable `ushell-kill-process-wait-time' specifies how
long to delay between signals."
  :type '(choice (const :tag "Kill all, don't ask" t)
		 (const :tag "Ask before killing" ask)
		 (const :tag "Ask for each process" every)
		 (const :tag "Don't kill subprocesses" nil))
  :group 'ushell-proc)

(defun ushell-round-robin-kill (&optional query)
  "Kill current process by trying various signals in sequence.
See the variable `ushell-kill-processes-on-exit'."
  (let ((sigs ushell-kill-process-signals))
    (while sigs
      (ushell-process-interact
       (function
	(lambda (proc)
	  (signal-process (process-id proc) (car sigs)))) t query)
      (setq query nil)
      (if (not ushell-process-list)
	  (setq sigs nil)
	(sleep-for ushell-kill-process-wait-time)
	(setq sigs (cdr sigs))))))

(defun ushell-query-kill-processes ()
  "Kill processes belonging to the current Ushell buffer, possibly w/ query."
  (when (and ushell-kill-processes-on-exit
	     ushell-process-list)
    (save-window-excursion
      (list-processes)
      (if (or (not (eq ushell-kill-processes-on-exit 'ask))
	      (y-or-n-p (format "Kill processes owned by `%s'? "
				(buffer-name))))
	  (ushell-round-robin-kill
	   (if (eq ushell-kill-processes-on-exit 'every)
	       "Kill Ushell child process `%s'? ")))
      (let ((buf (get-buffer "*Process List*")))
	(if (and buf (buffer-live-p buf))
	    (kill-buffer buf)))
      (message nil))))

(custom-add-option 'ushell-exit-hook 'ushell-query-kill-processes)

(defun ushell-interrupt-process ()
  "Interrupt a process."
  (interactive)
  (unless (ushell-process-interact 'interrupt-process)
    (run-hook-with-args 'ushell-kill-hook nil "interrupt")))

(defun ushell-kill-process ()
  "Kill a process."
  (interactive)
  (unless (ushell-process-interact 'kill-process)
    (run-hook-with-args 'ushell-kill-hook nil "killed")))

(defun ushell-quit-process ()
  "Send quit signal to process."
  (interactive)
  (unless (ushell-process-interact 'quit-process)
    (run-hook-with-args 'ushell-kill-hook nil "quit")))

;(defun ushell-stop-process ()
;  "Send STOP signal to process."
;  (interactive)
;  (unless (ushell-process-interact 'stop-process)
;    (run-hook-with-args 'ushell-kill-hook nil "stopped")))

;(defun ushell-continue-process ()
;  "Send CONTINUE signal to process."
;  (interactive)
;  (unless (ushell-process-interact 'continue-process)
;    ;; jww (1999-09-17): this signal is not dealt with yet.  For
;    ;; example, `ushell-reset' will be called, and so will
;    ;; `ushell-resume-eval'.
;    (run-hook-with-args 'ushell-kill-hook nil "continue")))

(defun ushell-send-eof-to-process ()
  "Send EOF to process."
  (interactive)
  (ushell-send-input nil nil t)
  (ushell-process-interact 'process-send-eof))

;;; Code:

;;; us-proc.el ends here
