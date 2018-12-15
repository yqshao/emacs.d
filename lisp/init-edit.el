;; Electric
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

;; Undo tree
(require-package 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)

(provide 'init-edit)
