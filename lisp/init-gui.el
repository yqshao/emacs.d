;;; -*- lexical-binding: t -*-

;; Fix font size
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))

;; Disable some elements
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; switch-window
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; windmove
(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)
(column-number-mode)

(require-package 'delight)
;; (delight 'undo-tree-mode nil 'undo-tree)
;; (delight 'guide-key-mode nil 'guide-key)
;; (delight 'helm-mode nil)
;; (delight 'elpy-mode nil 'elpy)
(delight 'emacs-lisp-mode "Elisp" :major)
(delight 'org-agenda-mode "Agenda" :major)

;; Spaceline
(require-package 'spaceline)
(require 'spaceline-config)
(require 'delight-powerline)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-position-off)

(custom-set-faces
 '(powerline-inactive1 ((t (:inherit mode-line-inactive))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive)))))
(spaceline-helm-mode)
(add-hook 'after-init-hook 'spaceline-emacs-theme)

(provide 'init-gui)
