;; Git
(require-package 'magit)
(setq-default magit-diff-refine-hunk t)
;; Hint: customize `magit-repository-directories'
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(provide 'init-git)
