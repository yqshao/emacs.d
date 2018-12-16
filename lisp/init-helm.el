;; helm
(require-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(add-hook 'helm-before-initialize-hook
    (lambda () 
      (set-face-attribute 'helm-source-header nil
                          :height 1.0)))
(helm-mode 1)
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)
(setq helm-ff-skip-boring-files t)
(add-to-list 'helm-boring-file-regexp-list "\~$")



(provide 'init-helm)
