(on-linux
 (g/up s)
 (g/up editorconfig)

 ;; git clone https://github.com/zerolfx/copilot.el ~/prog/copilot.el

 (load-library "~/prog/copilot.el/copilot.el")

 (global-copilot-mode))
