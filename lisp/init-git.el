;; Git
(use-package magit
  :ensure
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :init
  (setq-default magit-diff-refine-hunk t))


(use-package diff-hl
  :ensure
  :init
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)
