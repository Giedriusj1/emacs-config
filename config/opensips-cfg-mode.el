;;; -*- lexical-binding: t -*-

(setq opensips-cfg-highlights
      '(("#.*" . font-lock-comment-face)        
        ;; ("\ .*(" . font-lock-function-name-face)
        ("record_route\\|socket\\|server_signature\\|tcp_workers\\|udp_workers\\|log_facility\\|route\\|is_method\\|modparam\\|xlog_level\\|loadmodule\\|mpath\\|debug_mode\\|log_level\\|xlog\\|log_stderror\\|auto_aliases"
         . font-lock-constant-face)
        ("on \\| if\\|goto\\|exit" . font-lock-keyword-face)))

(define-derived-mode opensips-cfg-mode c-mode "opensips-cfg"
  "major mode for editing opensips cfg code."
  (setq font-lock-defaults '(opensips-cfg-highlights)))

(add-to-list 'auto-mode-alist '("\\opensips.*.cfg\\'" . opensips-cfg-mode))
