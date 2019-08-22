(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gpu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-utils)
(require 'init-site-lisp)
;; (require 'init-elpa)

;; Features
(require 'init-gui)
(require 'init-helm)
(require 'init-edit)
(require 'init-theme)
(require 'init-git)
(require 'init-gtd)
(require 'init-shell)

;; Language specific
(require 'init-c)
(require 'init-latex)
(require 'init-python)
;; ;; (require 'init-js)

;; Finishing
(require 'init-misc)
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
