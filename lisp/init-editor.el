;;; init-editor.el --- basic editing -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore)
(electric-pair-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(use-package dired-subtree
  :after dired
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "TAB") #'dired-subtree-toggle)))
(provide 'init-editor)
;;; init-editor.el ends here
