;;; Copied from Purcell's init-org.el
(use-package org-pomodoro
  :ensure
  :init
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))

(require 'org-habit)

;; Personal settings
(eval-after-load "org-agenda"
  '(add-to-list 'org-agenda-files "~/notes/gtd/"))

(defun org-gtd-agenda (&optional arg)
  (interactive "P") (org-agenda arg "g"))
(define-key global-map (kbd "C-c g") 'org-gtd-agenda)


;; Capture, inbox and refile
(define-key global-map (kbd "C-c c") (lambda () (interactive) (org-capture nil "t")))
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/gtd/inbox.org" "Tasks")
         "* NEXT %?\n  %i\n  %a")))

;; Style
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 0 (window-width)))
  (org-agenda-align-tags))
(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; Custom view
;; https://hamberg.no/gtd/
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJ(p)" "|" "DONE(d!/!)" "CANC(c@/!)")
              (sequence "WAIT(w@/!)" "DELE(e!)" "HOLD(h)" "|")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJ" :inherit font-lock-string-face))))

(let ((active-project-match "-INBOX/PROJ"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday 1
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up deadline-up priority-down user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags-todo "INBOX"
                  ((org-agenda-overriding-header "Inbox")
		   (org-agenda-tags-todo-honor-ignore-options t)))
             (tags-todo "-INBOX-BOOK"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT"))
				(org-agenda-skip-entry-if 'regexp "habit")
				)))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(priority-down effort-up category-keep))))
	    (tags-todo "-INBOX+BOOK"
                       ((org-agenda-overriding-header "Books to read")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT"))
				(org-agenda-skip-entry-if 'regexp "habit")
				)))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(priority-down effort-up category-keep))))
            ;; (tags-todo ,active-project-match
            ;;            ((org-agenda-overriding-header "Projects")
            ;;             (org-tags-match-list-sublevels t)
            ;;             (org-agenda-sorting-strategy
            ;;              '(category-keep))))
            ;; (tags-todo "-INBOX/-NEXT"
            ;;            ((org-agenda-overriding-header "Orphaned Tasks")
            ;;             (org-agenda-tags-todo-honor-ignore-options t)
            ;;             (org-agenda-todo-ignore-scheduled 'future)
            ;;             (org-agenda-skip-function
            ;;              '(lambda ()
            ;;                 (or (org-agenda-skip-subtree-if 'todo '("PROJ" "HOLD" "WAIT" "DELE"))
            ;;                     (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
            ;;             (org-tags-match-list-sublevels t)
            ;;             (org-agenda-sorting-strategy
            ;;              '(category-keep))))
	    (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "/WAIT"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "/DELE"
            ;;            ((org-agenda-overriding-header "Delegated")
            ;;             (org-agenda-tags-todo-honor-ignore-options t)
            ;;             (org-agenda-todo-ignore-scheduled 'future)
            ;;             (org-agenda-sorting-strategy
            ;;              '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "TODO"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(provide 'init-gtd)
