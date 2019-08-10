;; latex and pdf tools
(use-package auctex :ensure :defer)
(use-package pdf-tools :ensure
  :init
  (pdf-tools-install))
(use-package helm-bibtex :ensure
  :init
  (setq helm-bibtex-full-frame nil)
  (define-key global-map (kbd "C-c b") 'helm-bibtex)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (define-key LaTeX-mode-map (kbd "C-c b") 'helm-bibtex-cite))))

(defvar helm-source-bibtex-cite
  (helm-build-sync-source "BibTeX entries - for cite"
    :header-name (lambda (name)
                   (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
	     "Insert citation"            'helm-bibtex-insert-citation
	     "Open PDF, URL or DOI"       'helm-bibtex-open-any
	     "Insert reference"           'helm-bibtex-insert-reference
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex))
  "Source for searching in BibTeX files.")

(defun helm-bibtex-cite (&optional arg local-bib input)
  "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications)."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (helm :sources (list helm-source-bibtex-cite helm-source-fallback-options)
          :full-frame helm-bibtex-full-frame
          :buffer "*helm bibtex*"
          :input input
          :preselect (lambda ()
                       (and preselect
                            (> preselect 0)
                            (helm-next-line preselect)))
          :candidate-number-limit (max 500 (1+ (or preselect 0)))
          :bibtex-candidates candidates
          :bibtex-local-bib local-bib)))

;; Library setup
(setq bibtex-completion-bibliography '("~/zotero/storage/uppsala.bib"))
(setq bibtex-completion-library-path '("~/zotero/storage/"))
(setq bibtex-completion-pdf-field "file")

;; Use pdf-tools to open PDF files
;; From https://emacs.stackexchange.com/questions/
;; 19472/how-to-let-auctex-open-pdf-with-pdf-tools
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)


;;; Tweaked bibtex find pdf for WSL
(defun my/bibtex-completion-find-pdf-in-field (key-or-entry)
  "Returns the path of the PDF specified in the field
`bibtex-completion-pdf-field' if that file exists.  Returns nil if no
file is specified, or if the specified file does not exist, or if
`bibtex-completion-pdf-field' is nil."
  (when bibtex-completion-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (bibtex-completion-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (replace-regexp-in-string "\\\\\\\\" "/"
					    (bibtex-completion-get-value bibtex-completion-pdf-field entry))))
      (cond
       ((not value) nil)         ; Field not defined.
       ((-any 'f-file? (--map (f-join it (f-filename value))
			      (-flatten bibtex-completion-library-path)))
	(-filter 'f-file? (--map (f-join it (f-filename value))
				 (-flatten bibtex-completion-library-path))))
       (t                               ; Zotero/Mendeley/JabRef format:
        (let ((value (replace-regexp-in-string "\\([^\\]\\);" "\\1\^^" value)))
          (cl-loop  ; Looping over the files:
           for record in (s-split "\^^" value)
                                        ; Replace unescaped colons by field separator:
           for record = (replace-regexp-in-string "\\([^\\]\\|^\\):" "\\1\^_" record)
                                        ; Unescape stuff:
           for record = (replace-regexp-in-string "\\\\\\(.\\)" "\\1" record)
                                        ; Now we can safely split:
           for record = (s-split "\^_" record)

           for file-name = (nth 0 record)
           for path = (or (nth 1 record) "")
           for paths = (append
			(list
			 file-name)
			(--map (f-join it path file-name)
			       (-flatten bibtex-completion-library-path))) ; Jabref #100

           for result = (-first (lambda (path)
                                  (if (and (not (s-blank-str? path))
                                           (f-exists? path))
                                      path nil)) paths)
           if result collect result)))))))

(defun my/bibtex-completion-find-pdf (key-or-entry &optional find-additional)
  "Returns the path of the PDF associated with the specified
entry.  This is either the path(s) specified in the field
`bibtex-completion-pdf-field' or, if that does not exist, the
first PDF in any of the directories in
`bibtex-completion-library-path' whose name is composed of the
the BibTeX key plus `bibtex-completion-pdf-extension' (or if
FIND-ADDITIONAL is non-nil, all PDFs in
`bibtex-completion-library-path' whose name starts with the
BibTeX key and ends with `bibtex-completion-pdf-extension').
Returns nil if no PDF is found."
  (or (my/bibtex-completion-find-pdf-in-field key-or-entry)
      (bibtex-completion-find-pdf-in-library key-or-entry find-additional)))

(defun bibtex-completion-open-pdf (keys &optional fallback-action)
  "Open the PDFs associated with the marked entries using the
function specified in `bibtex-completion-pdf-open-function'.
If multiple PDFs are found for an entry, ask for the one to
open using `completion-read'.  If FALLBACK-ACTION is non-nil, it is called in
case no PDF is found."
  (dolist (key keys)
    (let ((pdf (my/bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs)))
      (cond
       ((> (length pdf) 1)
        (let* ((pdf (f-uniquify-alist pdf))
               (choice (completing-read "File to open: " (mapcar 'cdr pdf) nil t))
               (file (car (rassoc choice pdf))))
          (funcall bibtex-completion-pdf-open-function file)))
       (pdf
        (funcall bibtex-completion-pdf-open-function (car pdf)))
       (fallback-action
        (funcall fallback-action (list key)))
       (t
        (message "No PDF(s) found for this entry: %s"
                 key))))))

(require 'tex-buf)
(defun TeX-command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
  (cond ((if (string-equal name TeX-region)
			     (TeX-check-files (concat name "." (TeX-output-extension))
					      (list name)
					      TeX-file-extensions)
			   (TeX-save-document (TeX-master-file)))
			 TeX-command-default)
			((and (memq major-mode '(doctex-mode latex-mode))
			      (TeX-check-files (concat name ".bbl")
					       (mapcar 'car
						       (LaTeX-bibliography-list))
					       BibTeX-file-extensions))
			 ;; We should check for bst files here as well.
			 TeX-command-BibTeX)
			((TeX-process-get-variable name
						   'TeX-command-next
						   TeX-command-Show))
			(TeX-command-Show)))


(defcustom TeX-texify-Show t "Start view-command at end of TeX-texify?" :type 'boolean :group 'TeX-command)
(defcustom TeX-texify-max-runs-same-command 5 "Maximal run number of the same command" :type 'integer :group 'TeX-command)

(defun TeX-texify-sentinel (&optional proc sentinel)
  "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
If there is still something left do do start the next latex-command."
  (set-buffer (process-buffer proc))
  (funcall TeX-texify-sentinel proc sentinel)
  (let ((case-fold-search nil))
    (when (string-match "\\(finished\\|exited\\)" sentinel)
      (set-buffer TeX-command-buffer)
      (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
	(TeX-texify)))))

(defun TeX-texify ()
  "Get everything done."
  (interactive)
  (let ((nextCmd (TeX-command-default (TeX-master-file)))
	proc)
    (if (and (null TeX-texify-Show)
	     (equal nextCmd TeX-command-Show))
	(when  (called-interactively-p 'any)
	  (message "TeX-texify: Nothing to be done."))
      (TeX-command nextCmd 'TeX-master-file)
      (when (or (called-interactively-p 'any)
		(null (boundp 'TeX-texify-count-same-command))
		(null (boundp 'TeX-texify-last-command))
		(null (equal nextCmd TeX-texify-last-command)))
	(mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
	(setq TeX-texify-count-same-command 1))
      (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
	  (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
	(setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
	(setq TeX-texify-last-command nextCmd)
	(and (null (equal nextCmd TeX-command-Show))
	     (setq proc (get-buffer-process (current-buffer)))
	     (setq TeX-texify-sentinel (process-sentinel proc))
	     (set-process-sentinel proc 'TeX-texify-sentinel))))))
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'TeX-texify)))

(provide 'init-latex)
