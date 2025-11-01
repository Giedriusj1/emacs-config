;;; -*- lexical-binding: t -*-

(on-linux-or-mac

 ;; COPILOT
 (g/up copilot :defer nil
   :init

   (defalias 'cop 'copilot-mode)

   (dolist (mode '(prog-mode-hook text-mode-hook))
     (add-hook mode
               (lambda ()
                 ;; Set key binding immediately
                 (bind-keys* ("C-M-i" . copilot-accept-completion))

                 ;; Defer activation of copilot-mode slightly, so it does not interfere
                 ;; with text rendering
                 (run-at-time "0.5 sec" nil #'copilot-mode)

                 ))))

 ;; CLAUDE CODE
 ;; install required inheritenv dependency:
 (use-package inheritenv
   :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

 ;; for eat terminal backend:
 (use-package eat :ensure t)

 ;; for vterm terminal backend:
 (use-package vterm :ensure t)

 ;; install claude-code.el
 (use-package claude-code :ensure t
   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
   :config

   (use-package monet
     :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

   ;; optional IDE integration with Monet
   (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
   (monet-mode 1)

   (claude-code-mode)
   :bind-keymap ("C-c c" . claude-code-command-map)

   ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
   :bind
   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

 )
