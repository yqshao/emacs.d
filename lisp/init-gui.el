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
(setq frame-resize-pixelwise t)
;; switch-window
(use-package switch-window
  :ensure
  :bind  (("C-x o" . switch-window))
  :init
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil))

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

(use-package delight
  :ensure 
  :init
  (delight 'emacs-lisp-mode "Elisp" :major)
  (delight 'org-agenda-mode "Agenda" :major)
  )

;; Spaceline
(use-package spaceline
  :ensure
  :init
  (require 'spaceline-config)
  (require 'delight-powerline)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-buffer-position-off)

  (custom-set-faces
   '(powerline-inactive1 ((t (:inherit mode-line-inactive))))
   '(powerline-inactive2 ((t (:inherit mode-line-inactive)))))
  (spaceline-helm-mode)
  )

(defun spaceline--get-face (face active)
  "Universal function to get the right face.
FACE and ACTIVE have the same meanings as in
`spaceline-face-func'.  It delegates the work to
`spaceline-face-func' if it is given, otherwise falls back to
default configuration."
  (if spaceline-face-func
      (funcall spaceline-face-func face active)
    (cond
     ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
     ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
     ((eq 'line face) (if active 'powerline-active1 'powerline-inactive2))
     ((eq 'highlight face) (if active
                               (funcall spaceline-highlight-face-func)
                             'powerline-inactive1)))))

(defun myspaceline ()
  (spaceline-install
    'main
    '(((buffer-modified buffer-size) :face highlight-face)
      (buffer-id remote-host)
      (org-pomodoro :when active)
      )
    '((version-control)
      (process)
      (major-mode)
      (line-column)))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
(add-hook 'after-init-hook 'myspaceline)

(provide 'init-gui)
