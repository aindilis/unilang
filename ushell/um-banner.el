;;; em-banner.el --- sample module that displays a login banner

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

(provide 'em-banner)

(eval-when-compile (require 'ush-maint))

(defgroup ushell-banner nil
  "This sample module displays a welcome banner at login.
It exists so that others wishing to create their own Ushell extension
modules may have a simple template to begin with."
  :tag "Login banner"
  :link '(info-link "(ushell)Login banner")
  :group 'ushell-module)

;;; Commentary:

;; There is nothing to be done or configured in order to use this
;; module, other than to select it by customizing the variable
;; `ushell-modules-list'.  It will then display a version information
;; message whenever Ushell is loaded.
;;
;; This code is only an example of a how to write a well-formed
;; extension module for Ushell.  The better way to display login text
;; is to use the `ushell-script' module, and to echo the desired
;; strings from the user's `ushell-login-script' file.
;;
;; There is one configuration variable, which demonstrates how to
;; properly define a customization variable in an extension module.
;; In this case, it allows the user to change the string which
;; displays at login time.

;;; User Variables:

(defcustom ushell-banner-message "Welcome to the Emacs shell\n\n"
  "*The banner message to be displayed when Ushell is loaded.
This can be any sexp, and should end with at least two newlines."
  :type 'sexp
  :group 'ushell-banner)

(put 'ushell-banner-message 'risky-local-variable t)

;;; Code:

(require 'ush-util)

(defcustom ushell-banner-load-hook '(ushell-banner-initialize)
  "*A list of functions to run when `ushell-banner' is loaded."
  :type 'hook
  :group 'ushell-banner)

(defun ushell-banner-initialize ()
  "Output a welcome banner on initialization."
  ;; it's important to use `ushell-interactive-print' rather than
  ;; `insert', because `insert' doesn't know how to interact with the
  ;; I/O code used by Ushell
  (unless ushell-non-interactive-p
    (assert ushell-mode)
    (assert ushell-banner-message)
    (let ((msg (eval ushell-banner-message)))
      (assert msg)
      (ushell-interactive-print msg))))

(ushell-deftest banner banner-displayed
  "Startup banner is displayed at point-min"
  (assert ushell-banner-message)
  (let ((msg (eval usell-banner-message)))
    (assert msg)
    (goto-char (point-min))
    (looking-at msg)))

;;; em-banner.el ends here
