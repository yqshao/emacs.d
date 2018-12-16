(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Bootstrap
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)

;; Features
(require 'init-edit)
(require 'init-theme)
(require 'init-gui)
(require 'init-helm)
(require 'init-git)

;; Language specific
;; (require 'init-latex)
(require 'init-python)
;; (require 'init-js)

;; Finishing up
(require 'init-misc)
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
