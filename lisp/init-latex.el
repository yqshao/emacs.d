;; latex and pdf tools
(require-package 'auctex)
(require-package 'pdf-tools)
(pdf-tools-install)

;; Use pdf-tools to open PDF files
;; From https://emacs.stackexchange.com/questions/
;; 19472/how-to-let-auctex-open-pdf-with-pdf-tools
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(provide 'init-latex)
