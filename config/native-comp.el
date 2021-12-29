(setq comp-deferred-compilation nil
      package-native-compile t)

(defun compile-elpa-and-custom-to-native ()
  (interactive)
  (progn
    (recompile-custom-packages)
    (recompile-config)
    (native-compile-async '("~/.emacs.d/custom-packages"
                            "~/.emacs.d/elpa"
                            "~/.emacs.d/config"
                            ) 'recursively)))
