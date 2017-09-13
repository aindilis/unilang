(global-set-key "\C-crxx" 'unilang-test-xmlrpc-server-client)
(global-set-key "\C-crerg" 'unilang-edit-agent-registry)

;; /var/lib/myfrdcsa/codebases/internal/unilang/ushell.el

(load (concat frdcsa-internal-codebases "/unilang/ushell.el"))

(defun unilang-insert-region-into-unilang ()
 (interactive)
 (uea-send-contents
  (buffer-substring-no-properties (point) (mark))))

(defun unilang-test-xmlrpc-server-client ()
 ""
 (interactive)
 (ushell-kill)
 (ushell)
 (end-of-buffer)
 (sit-for 1)
 (insert "WS-Server-XMLRPC, echo hi")
 (eshell-send-input)
 (sit-for 1)
 (insert "WS-Client-XMLRPC, echo hi")
 (eshell-send-input)
 )

(defun unilang-edit-agent-registry ()
 "Edit the agent registry"
 (interactive)
 (ffap "/var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Util/AgentRegistry.pm"))
