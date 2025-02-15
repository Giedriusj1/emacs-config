;;; -*- lexical-binding: t -*-

(on-linux
 (g/up copilot
   :init
   (dolist (mode '(prog-mode-hook text-mode-hook))
     (add-hook
      mode
      (lambda ()
        (require 'copilot)

        (if (copilot-installed-version)
            (progn

              (i-defun copilot-accept-or-indent-for-tab-command (&optional arg)
                (if (copilot--overlay-visible)
                    (copilot-accept-completion)
                  (indent-for-tab-command arg)))

              (global-set-key (kbd "TAB") 'copilot-accept-or-indent-for-tab-command)


              (copilot-mode))

          (message "Copilot server is not installed")))))))
