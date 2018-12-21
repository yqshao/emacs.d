(require-package 'shell-pop)
(custom-set-variables
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 40)	       
 '(shell-pop-full-span nil)
 '(shell-pop-window-position "bottom"))
(provide 'init-shell)
