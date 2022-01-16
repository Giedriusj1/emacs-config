;;; -*- lexical-binding: t -*-

(defun private-synch-fn ()
  (interactive)
  (let* ((default-directory "~/private-sync"))
	(g/message "pulling private-sync repo")
	(start-process "proc-git-pull" "notes-sync-output" "git" "pull")))

;; Run the above every 2 mins (if we are idle)
(run-with-idle-timer (* 60 2) t 'private-synch-fn)

(use-package git-auto-commit-mode :defer t)

(defun turn-on-auto-commit-hook ()
  (cond ((string-match (concat "^" (expand-file-name "~/private-sync")) buffer-file-name)
         (progn
           (git-auto-commit-mode 1)
           (setq gac-automatically-push-p t)))))

(add-hook 'find-file-hook 'turn-on-auto-commit-hook)
