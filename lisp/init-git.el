;; Git
(require-package 'magit)
(setq-default magit-diff-refine-hunk t)
;; Hint: customize `magit-repository-directories'
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require-package 'diff-hl)
(add-hook 'after-init-hook 'global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'init-git)
