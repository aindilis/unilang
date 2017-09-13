;; This is  a wrapper  for unilang and  eshell, if uni&amp;amp;amp;ang  thinks its
;; eshell  stuff, it  prompts the  user and  sends it  there.   Use an
;; eshell-send-input hook to do this.

;; It would be best to start with a nice shell, and then go from
;; there.  it is going to be somewhat difficult anyway we look at
;; this, however, UniLang is probably, at this point among the most
;; important things we could work on, probably that, PSE, Gourmet and
;; SHOPS.  RADAR is not particularly important right now.  Well, yes
;; it is because those parts actually affect the development of
;; UniLang, PSE, etc.  A question of Exploration or Exploitation.  No
;; sense debating this.  It's too close to call.  What's funny is that
;; my knowledge that I must collect software creates an Eigenvector
;; where that is what I do.  And it super imposes on my other
;; responsibilities to create a vector which I am on right now.  We
;; ought to model these eigenvectors and superpositions, that is
;; likely states that result from different levels of interpretation.
;; For instance, base media operates superpositions where it can
;; interfere with otherwise important things.  It is useful phenomenon
;; to describe strange equilibrium that are present in behaviour.

;; The perfect example of this behaviour actually is software
;; development, hahaha, (didn't realize that I was saying that it
;; describe it before, and then independently thought of it.)  Yes,
;; you have a difficult compromise and a lack of knowledge, so you
;; write an oddball script that satisfies that particular
;; superposition.  For instance, a script to rename a bunch of files,
;; its really too specialized and obscure, should be part of another
;; tool, yet the other tools don't cover it, so it gets put in a place
;; that doesn't really belong.

(defvar uea-connected nil)
(defvar uea-use-perl-receive-message nil)
(defvar uea-do-process-contents t)

(setq uea-use-perl-receive-message t)

(setq ushell-buffer-name "*ushell*")

(setq ushell-agents-to-start
 '(
   ;; "Audience"
   ;; "Broker"
   ;; "BusRoute"
   ;; "CLEAR"
   ;; "Corpus"			
   ;; "CSO"

   ;; "ELog"				
   ;; "Emacs-Client"

   ;; "Event-System"
   ;; "Gourmet"
   ;; "KBS"				
   ;; "Manager"
   ;; "OpenCyc"
   ;; "PSE"
   ;; "UniLang-Client"
   ))

;; (setq ushell-agents-to-start
;;  '(
;;    "Audience"
;;    "Broker"
;;    ; "BusRoute"
;;    "CLEAR"
;;    "Corpus"
;;    "CSO"
;;    "ELog"
;;    ; "Event-System"
;;    ; "Gourmet"
;;    "Manager"
;;    "OpenCyc"
;;    "PSE"
;;    "UniLang-Client"
;;    ))

;;;###autoload
(defvar ushell-default-port 9000)
(defvar ushell-last-port)
;; (defvar ushell-buffer-names '())

(defun unilang-next-unused-port ()
 "return the next available port number"
 (if (boundp 'ushell-last-port)
  (+ ushell-last-port 1)
  ushell-default-port))

(define-derived-mode ushell-mode
 eshell-mode "ushell"
 "Major mode for UniLang.
\\{ushell-mode-map}"
 ;; (setq case-fold-search nil)
 (define-key ushell-mode-map "\C-c\C-k" nil)
 )

(defun ushell (&optional arg)
 "Create an interactive Ushell buffer.
The buffer used for Ushell sessions is determined by the value of
`ushell-buffer-name'.  If there is already an Ushell session active in
that buffer, Emacs will simply switch to it.  Otherwise, a new session
will begin.  A new session is always created if the prefix argument
ARG is specified.  Returns the buffer selected (or created)."
 (interactive "P")
 (assert ushell-buffer-name)
 (let* ((bufname (if arg
		  (progn
		   (split-window-right)
		   (generate-new-buffer ushell-buffer-name)
		   )
		  ushell-buffer-name))
	(buf (get-buffer-create bufname))
	(host (if arg
	       (read-from-minibuffer "Host? " "localhost")
	       "localhost"))
	(port (if arg
	       (string-to-number
		(read-from-minibuffer "Port? " (int-to-string (unilang-next-unused-port))))
	       9000)))

  ;; (push bufname ushell-buffer-names)
  (setq ushell-last-port port)

  ;; Simply calling `pop-to-buffer' will not mimic the way that
  ;; shell-mode buffers appear, since they always reuse the same
  ;; window that that command was invoked from.  To achieve this,
  ;; it's necessary to add `ushell-buffer-name' to the variable
  ;; `same-window-buffer-names', which is done when Ushell is loaded
  (assert (and buf (buffer-live-p buf)))
  (pop-to-buffer buf)
  (unless (fboundp 'eshell-mode)
   (error "`ushell-auto' must be loaded before Ushell can be used"))
  (unless (derived-mode-p 'eshell-mode)
   (ushell-mode)
   (insert "cd ~/")
   (eshell-send-input)
   (insert
    (concat 
     frdcsa-internal-codebases
     "/unilang/start -s -u "
     host
     " "
     (int-to-string port)
     " -c"
     (if ushell-agents-to-start
      (concat " -a " (join " " ushell-agents-to-start))
      "")))
   (eshell-send-input)
   (message "Starting Ushell...")
   (sit-for 2)
   (if (not (string-match "^cannot launch:" (buffer-string)))
    (progn 
     (uea-connect host port)
     (message "Ushell started."))
    (message "Ushell failed to start.")
    ))	
  (assert (non-nil (derived-mode-p 'eshell-mode)))
  buf
  ))

(defun join (separator list)
 "Same as Perl join"
 (setq value "")
 (let* ((first nil)
	(value
	 (dolist (elt list value)
	  (setq value (concat value (if first separator "") elt))
	  (setq first t))))
  value))

;; should start it with the unilang program

(global-set-key "\C-cuuc" 'uea-connect)
(global-set-key "\C-cuud" 'uea-disconnect)
(global-set-key "\C-crS" 'ushell-restart-emacs-client)
(global-set-key "\C-cuug" 'uea-grab-connection)
(global-set-key "\C-cuuv" 'uea-verify-connection)
(global-set-key "\C-cuuV" 'uea-verify-connection-raw)
(global-set-key "\C-crU" 'uea-connect-as-nth-emacs-client)

(defvar uea-agent-name "Emacs-Client")

(defun uea-connect (&optional host port)
 "Connect to the unilang server"
 (interactive)
 (let ((process-connection-type nil))	; Use a pipe.
  (setq uea-process
   (start-process 
    "emacs-client"
    "emacs-client"
    "telnet" 
    (if host host "localhost")
    (if port (int-to-string port) "9000"))))
 (set-process-filter (get-process "emacs-client")
  'uea-receive-message)
 (setq uea-connected t)
 (uea-register))

(defun uea-grab-connection (&optional host port)
 "Connect to the unilang server"
 (interactive)
 (uea-send-contents "Deregister Emacs-Client")
 (uea-connect))

(defun uea-disconnect (&optional host port)
 "Connect to the unilang server"
 (interactive)
 (setq uea-connected nil)
 (uea-unregister)
 (delete-process (get-process "emacs-client")))

(defun uea-unregister ()
 "Send the current message to UniLang."
 (interactive)
 (uea-send-contents "Deregister"))

(defun uea-register ()
 "Send the current message to UniLang."
 (interactive)
 (uea-send-contents "Register"))

;; (uea-test-message)

(defun uea-test-message ()
 "Send the current message to UniLang."
 (interactive)
 (uea-send-contents "Testing - hello"))

(defun uea-autovivify ()
 "Start ushell if it's not already running"
 (if (not uea-connected)
  (progn
   (ushell)
   (sit-for 5))
  ;; FIXME: replace this with a test for the proper values being in
  ;; the shell
  ;; FIXME: if it fails, signal an error
  ))

(defun uea-query-agent-raw (message &optional recipient data)
 ""
 (interactive)
 (uea-autovivify)
 (setq uea-buffer-message nil)
 (setq uea-buffer-message-raw nil)
 (set-process-filter (get-process "emacs-client")
  'uea-receive-message-query-agent)
 (uea-send-contents message recipient data)
 (while (not uea-buffer-message)
  (sit-for 0.1))
 (set-process-filter (get-process "emacs-client")
  'uea-receive-message)
 uea-buffer-message-raw)

(defun uea-query-agent (message &optional recipient data)
 ""
 (interactive)
 (uea-autovivify)
 (setq uea-buffer-message nil)
 (set-process-filter (get-process "emacs-client")
  'uea-receive-message-query-agent)
 (uea-send-contents message recipient data)
 (while (not uea-buffer-message)
  (sit-for 0.1))
 (set-process-filter (get-process "emacs-client")
  'uea-receive-message)
 (eval (read (concat "'" uea-buffer-message))))

(defvar uea-ping-message
 (concat
  "<message>
  <id>-1</id>
  <sender>" uea-agent-name "</sender>
  <receiver>UniLang</receiver>
  <date></date>
  <contents></contents>
  <data>$VAR1 = {
          '_Ping' => 1,
          '_Force' => 1,
        };
  </data>
</message>
"))

(defun uea-send-contents (contents &optional recipient data)
 "Send the current message to UniLang."
 (interactive)
 ;;  (save-excursion
 ;;   (set-buffer (get-buffer-create "ushell outbound messages"))
 ;;   (end-of-buffer)
 ;;   (insert (concat message "\n"))
 ;;   )
 (uea-send-message (concat
		    "<message>\n"
		    "<id>-1</id>\n"
		    "<sender>" uea-agent-name "</sender>\n"
		    "<receiver>" (or recipient "UniLang") "</receiver>\n"
		    "<date>" "</date>\n"

		    "<contents>" (uea-xmlify-message contents) "</contents>\n"
		    "<data>" (uea-xmlify-message (or data "")) "</data>\n"
		    "</message>\n"))
 ;; (uea-send-message uea-ping-message)
 )

(defun uea-send-message (message)
 "Send the current message to UniLang."
 ;; (message message)
 ;; (setq message (replace-regexp-in-string "\\$" "\$" message t t))
 (let ((message-prime
	message
	;; (replace-regexp-in-string "\\\\\\$" (concat (make-string 1 ?\\) "$") message t t)
	;; (replace-regexp-in-string "\\$" (concat (make-string 1 ?\\) "$") message t t)
	))
  (condition-case nil
   (process-send-string uea-process message-prime)
   (error (setq uea-connected nil)))))

(defun uea-xmlify-message (message)
 "Send the current message to UniLang."
 ;; find out how to do this now since its causing problems
 (if 
  (and (boundp 'message)
   (equal message nil))
  ""
  (progn
   (while (string-match "&" message)
    (setq message (replace-match "zqxqzqxqz" nil nil message)))
   (while (string-match "zqxqzqxqz" message)
    (setq message (replace-match "&amp;" nil nil message)))
   (while (string-match "<" message)
    (setq message (replace-match "zqxqzqxqz" nil nil message)))
   (while (string-match "zqxqzqxqz" message)
    (setq message (replace-match "&lt;" nil nil message)))
   (while (string-match ">" message)
    (setq message (replace-match "zqxqzqxqz" nil nil message)))
   (while (string-match "zqxqzqxqz" message)
    (setq message (replace-match "&gt;" nil nil message)))
   message)
  )
 )

(setq uea-output-kept nil)

(defun uea-receive-message (process output)
 (add-to-list 'uea-output-kept output 1 (lambda (a b) nil))
 (uea-receive-message-try-kept))

(defun uea-receive-message-try-kept ()
 "Handle messages from UniLang"
 (let ((joined (join "" uea-output-kept)))
  (if (string-match "</data>" joined)
   (if 
    (and
     uea-use-perl-receive-message
     (> (length joined) 1000))
    (let ((filename (make-temp-file "uea-")))
     (write-region joined nil filename nil 'silent)
     (eval
      (read
       (shell-command-to-string
	(concat "/var/lib/myfrdcsa/codebases/internal/unilang/scripts/receive-message.pl -n -f " filename)))))
    (if 
     (string-match (concat
		    "[\t \n]*"
		    "<id>\\(.*?\\)</id>"
		    "[\t \n]*"
		    "<sender>\\(.*?\\)</sender>"
		    "[\t \n]*"
		    "<receiver>\\(.*?\\)</receiver>"
		    "[\t \n]*"
		    "<date>\\(.*?\\)</date>"
		    "[\t \n]*"
		    "<contents>\\(\\([^.]?[.]?\\)*\\)</contents>"
		    ;; "<contents>\\(.*?\\)</contents>"
		    "[\t \n]*"
		    "<data>\\(\\([^.]?[.]?\\)*\\)</data>"
		    "[\t \n]*"
		    ) joined)
     (let
      (
       (id (match-string 1 joined))
       (sender (match-string 2 joined))
       (receiver (match-string 3 joined))
       (date (match-string 4 joined))
       (contents (match-string 5 joined))
       (data (match-string 6 joined))
       )
      (setq uea-output-kept nil)
      (if uea-do-process-contents
       (uea-process-contents id sender receiver date contents))
      ;; (pop-to-buffer (get-buffer-create "joined"))
      ;;     (erase-buffer)
      ;;     (insert contents)
      )
     )
    )
   )
  )
 )

(defun uea-receive-message-query-agent (process output)
 ;; (message (concat "uea-receive-message-query-agent:: " (prin1-to-string output)))
 (add-to-list 'uea-output-kept output 1 (lambda (a b) nil))
 (uea-receive-message-query-agent-try-kept))

(defun uea-receive-message-query-agent-try-kept ()
 "Handle messages from UniLang"
 (let ((joined (join "" uea-output-kept)))
  (if (string-match "</data>" joined)
   (if 
    (and
     uea-use-perl-receive-message
     (> (length joined) ;; 1000 ;; FIXME: get this working with KBS2 true-false queries
      0
      ))
    (let ((filename (make-temp-file "uea-")))
     (write-region joined nil filename nil 'silent)
     (eval
      (read
       (shell-command-to-string
	(concat "/var/lib/myfrdcsa/codebases/internal/unilang/scripts/receive-message.pl -q -f " filename)))))
    (if 
     (string-match (concat
		    "[\t \n]*"
		    "<id>\\(.*?\\)</id>"
		    "[\t \n]*"
		    "<sender>\\(.*?\\)</sender>"
		    "[\t \n]*"
		    "<receiver>\\(.*?\\)</receiver>"
		    "[\t \n]*"
		    "<date>\\(.*?\\)</date>"
		    "[\t \n]*"
		    "<contents>\\(\\([^.]?[.]?\\)*\\)</contents>"
		    ;; "<contents>\\(.*?\\)</contents>"
		    "[\t \n]*"
		    "<data>\\(\\([^.]?[.]?\\)*\\)</data>"
		    "[\t \n]*"
		    ) joined)
     (let
      (
       (id (match-string 1 joined))
       (sender (match-string 2 joined))
       (receiver (match-string 3 joined))
       (date (match-string 4 joined))
       (contents (match-string 5 joined))
       (data (match-string 6 joined))
       )
      (setq uea-output-kept nil)
      (if uea-do-process-contents
       (uea-process-contents id sender receiver date contents))
      (setq uea-buffer-message-raw (list id sender receiver date contents data))
      (setq uea-buffer-message contents)
      )
     )
    )
   )
  )
 )

(defun uea-receive-message-query-agent-try-kept-orig ()
 "Handle messages from UniLang"
 (let ((joined (join "" uea-output-kept)))
  (if (string-match (concat
		     "[\t \n]*"
		     "<id>\\(.*?\\)</id>"
		     "[\t \n]*"
		     "<sender>\\(.*?\\)</sender>"
		     "[\t \n]*"
		     "<receiver>\\(.*?\\)</receiver>"
		     "[\t \n]*"
		     "<date>\\(.*?\\)</date>"
		     "[\t \n]*"
		     "<contents>\\(\\([^.]?[.]?\\)*\\)</contents>"
		     ;; "<contents>\\(.*?\\)</contents>"
		     "[\t \n]*"
		     "<data>\\(\\([^.]?[.]?\\)*\\)</data>"
		     "[\t \n]*"
		     ) joined)
   (let
    (
     (id (match-string 1 joined))
     (sender (match-string 2 joined))
     (receiver (match-string 3 joined))
     (date (match-string 4 joined))
     (contents (match-string 5 joined))
     (data (match-string 7 joined))
     )
    (setq uea-output-kept nil)
    (if uea-do-process-contents
     (uea-process-contents id sender receiver date contents))
    (setq uea-buffer-message-raw (list id sender receiver date contents data))

    (setq uea-buffer-message contents)
    ;; (pop-to-buffer (get-buffer-create "joined"))
    ;;     (erase-buffer)
    ;;     (insert contents)
    )
   )
  )
 )

;; (defun uea-process-contents (id sender receiver date contents)
;;   "Handle incoming UniLang messages"
;;  (uea-notify-user contents))

(defun uea-process-contents (id sender receiver date contents)
 "Handle incoming UniLang messages"
 (cond
  ((string-match "eval \\(.*\\)" contents)
   (let* ((result (eval (read (match-string 1 contents))))
	  (arguments (list
		      (cons "_DoNotLog" 1)
		      )))
    (if (non-nil-2 result)
     (push (cons "Result" result) arguments))
    (uea-send-contents "" sender
     (freekbs2-util-data-dumper arguments))))
  ((string-match "ps \\(.*\\)" contents)
   (ps-queue-message (match-string 1 contents)))
  ((string= sender "KBS")
   (freekbs-process-message contents))
  (t 
   (uea-notify-user contents))
  ))

(defun uea-process-contents-query-agent (id sender receiver date contents)
 "Handle incoming UniLang messages"
 (setq uea-buffer-message contents))

(defun ushell-kill ()
 "Destroy the ushell window and kill all agents"
 (interactive)
 ;; FIX ME: this is really dangerous
 (shell-command (concat frdcsa-internal-codebases "/unilang/scripts/killall.sh"))
 (makunbound 'ushell-last-port)
 ;; (while (> (length ushell-buffer-names) 0)
 ;; (kill-buffer (switch-to-buffer (get-buffer (pop ushell-buffer-names)))))
 (message "Stopping Ushell...")
 (save-excursion
  (dolist (buffer (buffer-list)) 
   (set-buffer buffer)
   (if (derived-mode-p 'ushell-mode)
    (progn
     (ignore-errors
      (delete-process buffer))
     (kill-buffer buffer)))))
 (let* ((buffer (get-buffer "emacs-client")))
  (if (non-nil buffer)
   (progn
    (ignore-errors
     (delete-process buffer))
    (kill-buffer buffer))))
 (setq uea-output-kept nil)
 (message "Ushell stopped.")
 )

(defun ushell-restart ()
 "Restart the ushell window"
 (interactive)
 (ushell-kill)
 (ushell))

(defun ushell-restart-emacs-client ()
 "Restart the ushell window"
 (interactive)
 (if (gnus-buffer-exists-p (get-buffer "emacs-client"))
  (kill-buffer "emacs-client"))
 ;; FIXME: send a UniLang, deregister Emacs-Client using a CLI perl script
 (ushell-fix-junk)
 (uea-connect))

(defun ushell-fix-junk ()
 ""
 (interactive)
 (shell-command "mkdir -p /tmp/depository-uea; mv /tmp/uea-* /tmp/depository-uea"))

(defun uea-notify-user (contents)
 ""
 (message contents))

;; (defun uea-notify-user (contents)
;;  ""
;;  (if (> (length contents) 300)
;;   (progn
;;    (switch-to-buffer "unilang-messages")
;;    (insert contents)
;;    )
;;   (message contents)))

(defun uea-test-fast-sending ()
 ""
 (interactive)
 (uea-send-contents "UniLang, echo 1")
 (uea-send-contents "UniLang, echo 2")
 (uea-send-contents "UniLang, echo 3")
 ;; (uea-send-contents "UniLang, echo 4")
 ;; (uea-send-contents "UniLang, stats")
 )

;; (defun uea-verify-connection ()
;;  ""
;;  (interactive)
;;  (uea-autovivify)
;;  (setq uea-buffer-message nil)
;;  (set-process-filter (get-process "emacs-client")
;;   'uea-receive-message-query-agent)
;;  (uea-send-message uea-ping-message)
;;  (while (not uea-buffer-message)
;;   (sit-for 0.1))
;;  (set-process-filter (get-process "emacs-client")
;;   'uea-receive-message)
;;  (eval (read (concat "'" uea-buffer-message))))

;; (defun uea-verify-connection-raw ()
;;  ""
;;  (interactive)
;;  (uea-autovivify)
;;  (setq uea-buffer-message nil)
;;  (setq uea-buffer-message-raw nil)
;;  (set-process-filter (get-process "emacs-client")
;;   'uea-receive-message-query-agent)
;;  (uea-send-message uea-ping-message)
;;  (while (not uea-buffer-message)
;;   (sit-for 0.1))
;;  (set-process-filter (get-process "emacs-client")
;;   'uea-receive-message)
;;  uea-buffer-message-raw)

(defun uea-connect-as-nth-emacs-client (&optional n-arg)
 ""
 (interactive)
 (let* ((n (or n-arg (read-from-minibuffer "Nth Emacs Client? ")))
	(agent-name (if (integerp (read n))
		     (concat "Emacs-Client-" n)
		     (kmax-not-yet-implemented))))
  (setq uea-agent-name agent-name)
  (uea-connect)))
