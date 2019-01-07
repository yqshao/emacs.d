;; latex and pdf tools
(require-package 'auctex)
(require-package 'pdf-tools)
(require-package 'helm-bibtex)
(pdf-tools-install)


(helm-delete-action-from-source "Insert citation" helm-source-bibtex)
(helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
;; Use pdf-tools to open PDF files
;; From https://emacs.stackexchange.com/questions/
;; 19472/how-to-let-auctex-open-pdf-with-pdf-tools
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(provide 'init-latex)
