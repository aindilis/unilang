;;; ush-test.el --- Ushell test suite

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

(provide 'ush-test)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-test nil
  "This module is meant to ensure that Ushell is working correctly."
  :tag "Ushell test suite"
  :group 'ushell)

;;; Commentary:

;; The purpose of this module is to verify that Ushell works as
;; expected.  To run it on your system, use the command
;; \\[ushell-test].

;;; Code:

(require 'ush-mode)

;;; User Variables:

(defface ushell-test-ok-face
  '((((class color) (background light)) (:foreground "Green" :bold t))
    (((class color) (background dark)) (:foreground "Green" :bold t)))
  "*The face used to highlight OK result strings."
  :group 'ushell-test)

(defface ushell-test-failed-face
  '((((class color) (background light)) (:foreground "OrangeRed" :bold t))
    (((class color) (background dark)) (:foreground "OrangeRed" :bold t))
    (t (:bold t)))
  "*The face used to highlight FAILED result strings."
  :group 'ushell-test)

(defcustom ushell-show-usage-metrics nil
  "*If non-nil, display different usage metrics for each Ushell command."
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'ushell-mode-hook 'ushell-show-usage-metrics)
	   (remove-hook 'ushell-mode-hook 'ushell-show-usage-metrics))
	 (set symbol value))
  :type '(choice (const :tag "No metrics" nil)
		 (const :tag "Cons cells consumed" t)
		 (const :tag "Time elapsed" 0))
  :group 'ushell-test)

;;; Code:

(eval-when-compile
  (defvar test-buffer))

(defun ushell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (goto-char ushell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'ushell-send-input)))

(defun ushell-match-result (regexp)
  "Insert a command at the end of the buffer."
  (goto-char ushell-last-input-end)
  (looking-at regexp))

(defun ushell-command-result-p (text regexp &optional func)
  "Insert a command at the end of the buffer."
  (ushell-insert-command text func)
  (ushell-match-result regexp))

(defvar ushell-test-failures nil)

(defun ushell-run-test (module funcsym label command)
  "Test whether FORM evaluates to a non-nil value."
  (when (let ((sym (intern-soft (concat "ushell-" (symbol-name module)))))
	  (or (memq sym (ushell-subgroups 'ushell))
	      (ushell-using-module sym)))
    (with-current-buffer test-buffer
      (insert-before-markers
       (format "%-70s " (substring label 0 (min 70 (length label)))))
      (insert-before-markers "  ....")
      (ushell-redisplay))
    (let ((truth (eval command)))
      (with-current-buffer test-buffer
	(delete-backward-char 6)
	(insert-before-markers
	 "[" (let (str)
	       (if truth
		   (progn
		     (setq str "  OK  ")
		     (put-text-property 0 6 'face
					'ushell-test-ok-face str))
		 (setq str "FAILED")
		 (setq ushell-test-failures (1+ ushell-test-failures))
		 (put-text-property 0 6 'face
				    'ushell-test-failed-face str))
	       str) "]")
	(add-text-properties (line-beginning-position) (point)
			     (list 'test-func funcsym))
	(ushell-redisplay)))))

(defun ushell-test-goto-func ()
  "Jump to the function that defines a particular test."
  (interactive)
  (let ((fsym (get-text-property (point) 'test-func)))
    (when fsym
      (let* ((def (symbol-function fsym))
	     (library (locate-library (symbol-file fsym)))
	     (name (substring (symbol-name fsym)
			      (length "ushell-test--")))
	     (inhibit-redisplay t))
	(find-file library)
	(goto-char (point-min))
	(re-search-forward (concat "^(ushell-deftest\\s-+\\w+\\s-+"
				   name))
	(beginning-of-line)))))

(defun ushell-run-one-test (&optional arg)
  "Jump to the function that defines a particular test."
  (interactive "P")
  (let ((fsym (get-text-property (point) 'test-func)))
    (when fsym
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (let ((test-buffer (current-buffer)))
	(set-buffer (let ((inhibit-redisplay t))
		      (save-window-excursion (ushell t))))
	(funcall fsym)
	(unless arg
	  (kill-buffer (current-buffer)))))))

;;;###autoload
(defun ushell-test (&optional arg)
  "Test Ushell to verify that it works as expected."
  (interactive "P")
  (let* ((begin (ushell-time-to-seconds (current-time)))
	 (test-buffer (get-buffer-create "*ushell test*")))
    (set-buffer (let ((inhibit-redisplay t))
		  (save-window-excursion (ushell t))))
    (with-current-buffer test-buffer
      (erase-buffer)
      (setq major-mode 'ushell-test-mode)
      (setq mode-name "Ushell Test")
      (set (make-local-variable 'ushell-test-failures) 0)
      (local-set-key [(control ?c) (control ?c)] 'ushell-test-goto-func)
      (local-set-key [(control ?c) (control ?r)] 'ushell-run-one-test)
      (local-set-key [(control ?m)] 'ushell-test-goto-func)
      (local-set-key [return] 'ushell-test-goto-func)

      (insert "Testing Ushell under "
	      (format "GNU Emacs %s (%s%s)"
		      emacs-version
		      system-configuration
		      (cond ((featurep 'motif) ", Motif")
			    ((featurep 'x-toolkit) ", X toolkit")
			    (t ""))))
      (switch-to-buffer test-buffer)
      (delete-other-windows))
    (ushell-for funcname (sort (all-completions "ushell-test--"
						obarray 'functionp)
			       'string-lessp)
      (with-current-buffer test-buffer
	(insert "\n"))
      (funcall (intern-soft funcname)))
    (with-current-buffer test-buffer
      (insert (format "\n\n--- %s --- (completed in %d seconds)\n"
		      (current-time-string)
		      (- (ushell-time-to-seconds (current-time))
			 begin)))
      (message "Ushell test suite completed: %s failure%s"
	       (if (> ushell-test-failures 0)
		   (number-to-string ushell-test-failures)
		 "No")
	       (if (= ushell-test-failures 1) "" "s"))))
  (goto-char ushell-last-output-end)
  (unless arg
    (kill-buffer (current-buffer))))


(defvar ushell-metric-before-command 0)
(defvar ushell-metric-after-command 0)

(defun ushell-show-usage-metrics ()
  "If run at Ushell mode startup, metrics are shown after each command."
  (set (make-local-variable 'ushell-metric-before-command)
       (if (eq ushell-show-usage-metrics t)
	   0
	 (current-time)))
  (set (make-local-variable 'ushell-metric-after-command)
       (if (eq ushell-show-usage-metrics t)
	   0
	 (current-time)))

  (make-local-hook 'ushell-pre-command-hook)
  (add-hook 'ushell-pre-command-hook
	    (function
	     (lambda ()
	       (setq ushell-metric-before-command
		     (if (eq ushell-show-usage-metrics t)
			 (car (memory-use-counts))
		       (current-time))))) nil t)

  (make-local-hook 'ushell-post-command-hook)
  (add-hook 'ushell-post-command-hook
	    (function
	     (lambda ()
	       (setq ushell-metric-after-command
		     (if (eq ushell-show-usage-metrics t)
			 (car (memory-use-counts))
		       (current-time)))
	       (ushell-interactive-print
		(concat
		 (int-to-string
		  (if (eq ushell-show-usage-metrics t)
		      (- ushell-metric-after-command
			 ushell-metric-before-command 7)
		    (- (ushell-time-to-seconds
			ushell-metric-after-command)
		       (ushell-time-to-seconds
			ushell-metric-before-command))))
		 "\n"))))
	    nil t))

;;; us-test.el ends here
