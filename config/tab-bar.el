;;; -*- lexical-binding: t -*-

(setq tab-bar-show -1)
(tab-bar-mode)

(global-set-key (kbd "<C-tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "<C-S-tab>") 'tab-bar-switch-to-prev-tab)


(transient-define-prefix g/frame-helper-transient ()
  ["tab-bar"
   ("M-c" "new" tab-bar-new-tab)
   ("M-k" "close" tab-bar-close-tab)]
  ["frame"
   ("M-m" "new" make-frame)
   ("M-o" "other" other-frame)
   ("M-SPC" "other" other-frame)
   ("M-d" "delete" delete-frame)])

(global-set-key (kbd "M-SPC") 'g/frame-helper-transient)
