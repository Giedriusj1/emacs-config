;;; -*- lexical-binding: t -*-

(on-linux
 (i-defun private-sync-fn ()
   (let* ((default-directory "~/private-sync"))
     (g/message "pulling private-sync repo")
     (start-process "proc-git-pull" "notes-sync-output" "git" "pull")))

 ;; Run the above every 2 mins (if we are idle)
 (run-with-idle-timer (* 60 2) t 'private-sync-fn)

 (use-package git-auto-commit-mode
   :diminish git-auto-commit-mode)

 (defun turn-on-auto-commit-hook ()
   (cond ((string-match (concat "^" (expand-file-name "~/private-sync")) buffer-file-name)
          (progn
            (git-auto-commit-mode t)
            (setq gac-automatically-push-p t)))))

 (add-hook 'find-file-hook 'turn-on-auto-commit-hook))
