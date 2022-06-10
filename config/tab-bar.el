;;; -*- lexical-binding: t -*-
(setq tab-bar-show 1)
(tab-bar-mode)

(global-set-key (kbd "<C-tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "<C-S-tab>") 'tab-bar-switch-to-prev-tab)

(defhydra hydra-frame-helper
  (:color blue)
  "
 tab-bar                 frame management
------------------------------------------
                        [_M-m_]ake frame
[_M-c_]reate              [_M-o_]ther frame
[_M-k_]lose current       [_M-SPC_]other frame
                        [_M-d_]elete frame
"
  ("M-m" make-frame nil)
  ("M-o" other-frame nil)
  ("M-SPC" other-frame nil)
  ("M-d" delete-frame nil)
  ("M-c" tab-bar-new-tab)
  ("M-k" tab-bar-close-tab))

(global-set-key (kbd "M-SPC") 'hydra-frame-helper/body)
