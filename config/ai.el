;;; -*- lexical-binding: t -*-

(on-linux
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

                 )))))
