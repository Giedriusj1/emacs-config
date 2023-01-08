(on-linux
 (g/up s)
 (g/up editorconfig)

 ;; git clone https://github.com/zerolfx/copilot.el ~/prog/copilot.el

 (load-library "~/prog/copilot.el/copilot.el")

 ;; enable copilot-mode for every programming mode
 (add-hook 'prog-mode-hook 'copilot-mode)

 (i-defun copilot-accept-or-indent-for-tab-command (&optional arg)
   (if (copilot--overlay-visible)
       (copilot-accept-completion)
     (indent-for-tab-command arg)))



 (global-set-key (kbd "TAB") 'copilot-accept-or-indent-for-tab-command)

 (g/up gpt :bind
   ("C--" . (lambda ()
	      (interactive)
	      (gpt-dwim ;; :temperature "0.123" :max-tokens "500"
	       ))))

 )
