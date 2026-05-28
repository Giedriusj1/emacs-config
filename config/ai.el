;;; -*- lexical-binding: t -*-

(on-linux-or-mac

 ;; COPILOT
  (g/up copilot
    :init

    (bind-keys* ("C-M-i" . copilot-accept-completion))

    (defun g/copilot--turn-on ()
      (when (derived-mode-p 'prog-mode 'text-mode)
        (copilot-mode 1)))

    (define-minor-mode g/copilot-mode
      "Toggle Copilot in programming and text buffers."
      :global t
      (if g/copilot-mode
          (progn
            (add-hook 'prog-mode-hook #'g/copilot--turn-on)
            (add-hook 'text-mode-hook #'g/copilot--turn-on)
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                (g/copilot--turn-on))))
        (remove-hook 'prog-mode-hook #'g/copilot--turn-on)
        (remove-hook 'text-mode-hook #'g/copilot--turn-on)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (bound-and-true-p copilot-mode)
              (copilot-mode -1))))))

    (defalias 'cop #'g/copilot-mode))

 ;; ;; CLAUDE CODE
 ;; ;; install required inheritenv dependency:
 ;; (use-package inheritenv
 ;;   :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

 ;; ;; for eat terminal backend:
 ;; (use-package eat :ensure t)

 ;; ;; for vterm terminal backend:
 ;; (use-package vterm :ensure t)

 ;; ;; install claude-code.el
 ;; (use-package claude-code :ensure t
 ;;   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
 ;;   :config

 ;;   (use-package monet
 ;;     :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

 ;;   ;; optional IDE integration with Monet
 ;;   (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
 ;;   (monet-mode 1)

 ;;   (claude-code-mode)
 ;;   :bind-keymap ("C-c c" . claude-code-command-map)

 ;;   ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
 ;;   :bind
 ;;   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

 )
