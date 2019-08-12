;; Electric
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

;; Backup folders
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Undo tree
(use-package undo-tree
  :ensure
  :init
  (add-hook 'after-init-hook 'global-undo-tree-mode))

(provide 'init-edit)
