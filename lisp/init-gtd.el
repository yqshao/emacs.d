;;; org-caldav
(use-package oauth2 :ensure)

(use-package org-caldav
  :ensure
  :init
  (setq
   ;; gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
   plstore-cache-passphrase-for-symmetric-encryption t
   org-icalendar-include-todo t
   org-caldav-show-sync-results nil
   org-icalendar-use-scheduled '(event-if-todo todo-start)
   org-icalendar-use-deadline '(event-if-todo event-if-not-todo)   
   org-icalendar-with-timestamps nil
   org-caldav-url 'google
   org-caldav-calendar-id "vf1djf1c314sk6c12bpe97g19c@group.calendar.google.com"
   org-caldav-files '("~/notes/gtd/uppsala.org"
		      "~/notes/gtd/inbox.org")
   org-caldav-save-directory "~/notes/gtd"
   org-caldav-inbox "~/notes/gtd/gcal.org"
   org-icalendar-timezone "Europe/Stockholm"
   org-caldav-oauth2-client-id
   "946592304530-coplp636hn85dre2d900bv62fum02463.apps.googleusercontent.com"
   org-caldav-oauth2-client-secret "m5GDMJZFII1nASLx0e8gRIqK"))

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
	(900 1800)
	"......" "----------------"))
      org-habit-show-habits nil)

;;; Copied from Purcell's init-org.el
(use-package org-pomodoro
  :ensure
  :init
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))

(require 'org-habit)

;; Personal settings
(setq org-directory "~/notes/gtd/")
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
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            )))))


(provide 'init-gtd)
