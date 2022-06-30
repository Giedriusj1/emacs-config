;;; -*- lexical-binding: t -*-

(on-linux
 (defhydra hydra-gdb-helper (:color blue)

   "
_h_  restore-windows  |  _j_  next       _b_  set break     _p_ print
_m_  many-windows     |  _k_  step       _r_  remove break
                    |  _l_  up
                    |  _c_  cont
"
   ( "h" gdb-restore-windows nil)
   ( "m" gdb-many-windows nil)
   ( "j" gud-next nil)
   ( "k" gud-step nil)
   ( "l" gud-up nil)
   ( "b" gud-break nil)
   ( "r" gud-remove nil)
   ( "c" gud-cont nil)
   ( "p" gud-print nil))

 (define-key tab-map (kbd "k") 'hydra-gdb-helper/body)
 ;; (use-package dap-mode :defer t)
 )
