(use-package color-theme-sanityinc-tomorrow
  :ensure
  :init
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))
)



(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-eighties))
  (reapply-themes))

;; (when (maybe-require-package 'dimmer)
;;   (setq-default dimmer-fraction 0.15)
;;   (add-hook 'after-init-hook 'dimmer-mode))

(provide 'init-theme)
