(use-package eyebrowse :ensure t
  :config
  (setq eyebrowse-mode-line-separator " " eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode t)
  (customize-set-variable 'eyebrowse-mode-line-style 'smart))

(defhydra hydra-frame-helper
  (:color blue)
  "
eyebrowse               frame management
------------------------------------------
[_M-r_]ename              [_M-m_]ake frame
[_M-c_]reate              [_M-o_]ther frame
[_M-k_]lose current       [_M-SPC_]other frame
                        [_M-d_]elete frame
%s(eyebrowse-mode-line-indicator)^^
"
  ("M-m" make-frame nil)
  ("M-o" other-frame nil)
  ("M-SPC" other-frame nil)
  ("M-d" delete-frame nil)
  ("M-r" eyebrowse-rename-window-config nil)
  ("M-c"  (lambda ()
	        (interactive)
	        (progn
	          (eyebrowse-create-window-config)
	          (show-eyebrowse-posframe)) nil))
  ("M-k" (lambda ()
	       (interactive)
	       (progn
	         (eyebrowse-close-window-config)
	         (show-eyebrowse-posframe)) nil)))

(global-set-key (kbd "M-SPC") 'hydra-frame-helper/body)
(global-set-key (kbd "<C-tab>") 'eyebrowse-next)
(global-set-key (kbd "<C-iso-lefttab>") 'eyebrowse-prev)
(global-set-key (kbd "<C-S-tab>") 'eyebrowse-prev)

(defun show-eyebrowse-posframe ()
  (interactive)
  (progn
    (when (posframe-workable-p)
      (posframe-show " *eyebrowse-posframe*"
                     :string (eyebrowse-mode-line-indicator)
                     :position (cons -40 10)
                     :font "Monospace-16"
                     :timeout 3
                     :internal-border-width 4
                     :internal-border-color "orange"))))

(custom-set-faces '(eyebrowse-mode-line-active ((t (:foreground "DarkMagenta" :weight bold :height 1.2)))))





(defun eyebrowse-next (args)
  (interactive "P")
  (progn (eyebrowse-next-window-config args)
         (show-eyebrowse-posframe)))

(defun eyebrowse-prev (args)
  (interactive "P")
  (progn (eyebrowse-prev-window-config args)
         (show-eyebrowse-posframe)))

