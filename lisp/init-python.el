(require-package 'elpy)
(require-package 'snakemake-mode)
(elpy-enable)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(eval-after-load "elpy"
  '(cl-dolist (key '("C-<up>" "C-<down>" "C-<left>" "C-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(provide 'init-python)
