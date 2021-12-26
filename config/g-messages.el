(setq g/message-buff (generate-new-buffer "G/Messages"))

(defun g/message (msg)
  (with-current-buffer "G/Messages" ; replace with the name of the buffer you want to append
    (goto-char (point-max))
    (insert (format "%s %s\n"
                    (format-time-string "%d/%m/%Y - %H:%M:%S")
                    msg) )))

(g/message "Emacs booting up")
(g/message (emacs-version))
