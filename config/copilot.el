;;; -*- lexical-binding: t -*-

(on-linux
 (g/up s)
 (g/up editorconfig)

 ;; git clone https://github.com/zerolfx/copilot.el ~/prog/EMACS/copilot.el
 (let ((f "~/prog/EMACS/copilot.el/copilot.el"))
   (when (file-exists-p f)
     (progn
       (add-to-list 'load-path (expand-file-name "~/prog/EMACS/copilot.el"))


       ;; lazy load copilot only when needed
       (add-hook 'prog-mode-hook (lambda ()
				   (require 'copilot)
				   (copilot-mode)))

       (global-set-key (kbd "TAB") 'copilot-accept-or-indent-for-tab-command)

       (i-defun copilot-accept-or-indent-for-tab-command (&optional arg)
	 (if (copilot--overlay-visible)
	     (copilot-accept-completion)
	   (indent-for-tab-command arg)))
       )))


 (g/up gpt :bind ("C--" . g/gpt-transient)
   :config
   (transient-define-prefix g/gpt-transient ()
     ["codex"
      ("c" "codex temp 0" (lambda ()
                            (interactive)
                            (setq gpt-openai-temperature "0")
                            (setq gpt-openai-model "code-davinci-003")
                            (setq gpt-openai-max-tokens "2000")
                            (gpt-dwim)))
      ("C" "codex temp 0.5" (lambda ()
                              (interactive)
                              (setq gpt-openai-temperature "0.5")
                              (setq gpt-openai-model "code-davinci-003")
                              (setq gpt-openai-max-tokens "2000")
                              (gpt-dwim)))]
     ["GPT-3"
      ("d" "Davinci temp 0" (lambda ()
                              (interactive)
                              (setq gpt-openai-temperature "0")
                              (setq gpt-openai-model "gpt-3.5-turbo")
                              (message "gpt-openai-model: %s" gpt-openai-model)
                              (setq gpt-openai-max-tokens "2000")
                              (gpt-dwim)))
      ("D" "Davinci temp 0.5" (lambda ()
                                (interactive)
                                (setq gpt-openai-temperature "0.5")
                                (setq gpt-openai-model "gpt-3.5-turbo")
                                (setq gpt-openai-max-tokens "4000")
                                (gpt-dwim)))
      ])))
