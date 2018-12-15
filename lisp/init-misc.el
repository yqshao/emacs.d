(fset 'yes-or-no-p 'y-or-n-p)
;; Fix tramp for customized prompt
(setq tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(provide 'init-misc)
