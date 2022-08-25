;;; -*- lexical-binding: t -*-

(setq tab-bar-show -1)
(tab-bar-mode)

(global-set-key (kbd "<C-tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "<C-S-tab>") 'tab-bar-switch-to-prev-tab)

(pretty-hydra-define hydra-frame-helper (:color blue)
  ("tab-bar"
   (("M-c" tab-bar-new-tab "new")
    ("M-k" tab-bar-close-tab "close"))
   "frame"
   (("M-m" make-frame "new")
    ("M-o" other-frame "other")
    ("M-SPC" other-frame "other")
    ("M-d" delete-frame "delete"))))

(global-set-key (kbd "M-SPC") 'hydra-frame-helper/body)
