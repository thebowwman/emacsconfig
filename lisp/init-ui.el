;;; init-ui.el --- appearance -*- lexical-binding: t; -*-

;; Minimal chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq use-dialog-box nil
      frame-resize-pixelwise t)
(pixel-scroll-precision-mode 1)

;; Font: a Nerd Font if installed, otherwise the first font that exists
(let ((font (seq-find (lambda (f) (member f (font-family-list)))
                      '("JetBrainsMono Nerd Font" "Ubuntu Sans Mono" "monospace"))))
  (when font
    (set-face-attribute 'default nil :family font :height 120)))
(setq-default line-spacing 0.15)

;; Theme
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

;; Icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Modeline
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  :init
  (doom-modeline-mode 1))

;; Code readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  :hook ((python-base-mode . indent-bars-mode)))

(global-hl-line-mode 1)

;; Startup dashboard with recent files and projects
(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5) (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui)
;;; init-ui.el ends here
