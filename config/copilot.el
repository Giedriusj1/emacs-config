(on-linux
 (g/up s)
 (g/up editorconfig)

 ;; git clone https://github.com/zerolfx/copilot.el ~/prog/EMACS/copilot.el
 (let ((f "~/prog/EMACS/copilot.el/copilot.el"))
   (when (file-exists-p f)
     (load-library f))

   ;; enable copilot-mode for every programming mode
   (add-hook 'prog-mode-hook 'copilot-mode)

   (i-defun copilot-accept-or-indent-for-tab-command (&optional arg)
     (if (copilot--overlay-visible)
	 (copilot-accept-completion)
       (indent-for-tab-command arg)))

   (global-set-key (kbd "TAB") 'copilot-accept-or-indent-for-tab-command))

 (g/up gpt :bind ("C--" . g/gpt-transient)

   :config
   (transient-define-prefix g/gpt-transient ()
     ["codex"
      ("c" "codex temp 0" (lambda ()
			    (interactive)
			    (gpt-dwim :temperature "0" :max-tokens "500" :engine "code-davinci-002")))
      ("C" "codex temp 0.5" (lambda ()
			      (interactive)
			      (gpt-dwim :temperature "0.5" :max-tokens "500" :engine "code-davinci-002")))]
     ["GPT-3"
      ("d" "Davinci temp 0" (lambda ()
			      (interactive)
			      (gpt-dwim :temperature "0" :max-tokens "4000" :engine "text-davinci-003")))
      ("D" "Davinci temp 0.5" (lambda ()
				(interactive)
				(gpt-dwim :temperature "0.5" :max-tokens "4000" :engine "text-davinci-003")))
      ])))
