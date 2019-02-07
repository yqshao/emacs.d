;; Simplify yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

;; Fix tramp for customized prompt
(setq tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

;; Quote for scratch
(setq-default
 initial-scratch-message
 (concat ";; It is not knowledge, but the act of learning, not possession but the act of\n"
	 ";; getting there, which grants the greatest enjoyment. When I have clarified\n"
	 ";; and exhausted a subject, then I turn away from it, in order to go into\n"
	 ";; darkness again; the never-satisfied man is so strange if he has completed\n"
	 ";; a structure, then it is not in order to dwell in it peacefully,but in order\n"
	 ";; to begin another. I imagine the world conqueror must feel thus, who, after\n"
	 ";; one kingdom is scarcely conquered, stretches out his arms for others.\n"
	 ";;\n"
	 ";;                             - Carl Friedrich Gauss, Letter to Bolyai, 1808.\n\n"
	 ))

;; Guide-key, from purcell's config
(use-package guide-key
  :ensure
  :init
  (setq guide-key/guide-key-sequence t)
  (add-hook 'after-init-hook 'guide-key-mode))

(provide 'init-misc)
