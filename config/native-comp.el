(setq comp-deferred-compilation nil
      package-native-compile t)

(i-defun g/compile-elpa-and-custom-to-native ()
  (progn
    (g/recompile-custom-packages)
    (g/recompile-config)
    (native-compile-async '("~/.emacs.d/custom-packages"
                            "~/.emacs.d/elpa"
                            "~/.emacs.d/config"
                            )'recursively)))
