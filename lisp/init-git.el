;;; init-git.el --- git: magit and changed-line markers -*- lexical-binding: t; -*-

(require 'init-keys)

(use-package magit)

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(my/leader
  "g"  '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "git status")
  "gl" '(magit-log-current :which-key "git log"))

(provide 'init-git)
;;; init-git.el ends here
