;;; -*- lexical-binding: t -*-

(on-linux
 (g/up copilot :defer nil
   :init

   (defalias 'cop 'copilot-mode)

   (dolist (mode '(prog-mode-hook text-mode-hook))
     (add-hook mode
               (lambda ()
                 (bind-keys* ( "C-M-i" . copilot-accept-completion))

                 (copilot-mode)
                 )))))
