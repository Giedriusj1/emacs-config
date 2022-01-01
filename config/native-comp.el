(setq comp-deferred-compilation nil
      package-native-compile t)

(defun compile-elpa-and-custom-to-native ()
  (interactive)
  (progn
    (g/recompile-custom-packages)
    (g/recompile-config)
    (native-compile-async '("~/.emacs.d/custom-packages"
                            "~/.emacs.d/elpa"
                            "~/.emacs.d/config"
                            ) 'recursively)))
