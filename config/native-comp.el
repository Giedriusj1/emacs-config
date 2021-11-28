(setq comp-deferred-compilation nil
      package-native-compile t)

(defun compile-elpa-and-custom-to-native ()
  (interactive)
  (progn
    (recompile-custom-packages)
    (native-compile-async '("~/.emacs.d/custom-packages" "~/.emacs.d/elpa") 'recursively)))
