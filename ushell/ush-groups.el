;;; do not modify this file; it is auto-generated

(defgroup ushell-alias nil
  "Command aliases allow for easy definition of alternate commands."
  :tag "Command aliases"
  :link '(info-link "(ushell)Command aliases")
  :group 'ushell-module)

(defgroup ushell-banner nil
  "This sample module displays a welcome banner at login.
It exists so that others wishing to create their own Ushell extension
modules may have a simple template to begin with."
  :tag "Login banner"
  :link '(info-link "(ushell)Login banner")
  :group 'ushell-module)

(defgroup ushell-basic nil
  "The \"basic\" code provides a set of convenience functions which
are traditionally considered shell builtins.  Since all of the
functionality provided by them is accessible through Lisp, they are
not really builtins at all, but offer a command-oriented way to do the
same thing."
  :tag "Basic shell commands"
  :group 'ushell-module)

(defgroup ushell-cmpl nil
  "This module provides a programmable completion function bound to
the TAB key, which allows for completing command names, file names,
variable names, arguments, etc."
  :tag "Argument completion"
  :group 'ushell-module)

(defgroup ushell-dirs nil
  "Directory navigation involves changing directories, examining the
current directory, maintaining a directory stack, and also keeping
track of a history of the last directory locations the user was in.
Emacs does provide standard Lisp definitions of `pwd' and `cd', but
they lack somewhat in feel from the typical shell equivalents."
  :tag "Directory navigation"
  :group 'ushell-module)

(defgroup ushell-glob nil
  "This module provides extended globbing syntax, similar what is used
by zsh for filename generation."
  :tag "Extended filename globbing"
  :group 'ushell-module)

(defgroup ushell-hist nil
  "This module provides command history management."
  :tag "History list management"
  :group 'ushell-module)

(defgroup ushell-ls nil
  "This module implements the \"ls\" utility fully in Lisp.  If it is
passed any unrecognized command switches, it will revert to the
operating system's version.  This version of \"ls\" uses text
properties to colorize its output based on the setting of
`ushell-ls-use-colors'."
  :tag "Implementation of `ls' in Lisp"
  :group 'ushell-module)

(defgroup ushell-pred nil
  "This module allows for predicates to be applied to globbing
patterns (similar to zsh), in addition to string modifiers which can
be applied either to globbing results, variable references, or just
ordinary strings."
  :tag "Value modifiers and predicates"
  :group 'ushell-module)

(defgroup ushell-prompt nil
  "This module provides command prompts, and navigation between them,
as is common with most shells."
  :tag "Command prompts"
  :group 'ushell-module)

(defgroup ushell-rebind nil
  "This module allows for special keybindings that only take effect
while the point is in a region of input text.  By default, it binds
C-a to move to the beginning of the input text (rather than just the
beginning of the line), and C-p and C-n to move through the input
history, C-u kills the current input text, etc.  It also, if
`ushell-confine-point-to-input' is non-nil, does not allow certain
commands to cause the point to leave the input area, such as
`backward-word', `previous-line', etc.  This module intends to mimic
the behavior of normal shells while the user editing new input text."
  :tag "Rebind keys at input"
  :group 'ushell-module)

(defgroup ushell-script nil
  "This module allows for the execution of files containing Ushell
commands, as a script file."
  :tag "Running script files."
  :group 'ushell-module)

(defgroup ushell-smart nil
  "This module combines the facility of normal, modern shells with
some of the edit/review concepts inherent in the design of Plan 9's
9term.  See the docs for more details.

Most likely you will have to turn this option on and play around with
it to get a real sense of how it works."
  :tag "Smart display of output"
  :link '(info-link "(ushell)Smart display of output")
  :group 'ushell-module)

(defgroup ushell-term nil
  "This module causes visual commands (e.g., 'vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `ushell-visual-commands' defines
which commands are considered visual in nature."
  :tag "Running visual commands"
  :group 'ushell-module)

(defgroup ushell-unix nil
  "This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Ushell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Ushell background processes
by name)."
  :tag "UNIX commands in Lisp"
  :group 'ushell-module)

(defgroup ushell-xtra nil
  "This module defines some extra alias functions which are entirely
optional.  They can be viewed as samples for how to write Ushell alias
functions, or as aliases which make some of Emacs' behavior more
naturally accessible within Emacs."
  :tag "Extra alias functions"
  :group 'usell-module)

