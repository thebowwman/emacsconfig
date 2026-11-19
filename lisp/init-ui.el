;;; init-ui.el --- appearance -*- lexical-binding: t; -*-

;; Minimal chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq use-dialog-box nil
      frame-resize-pixelwise t)
(set-face-attribute 'default nil :height 120)

;; Theme
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

;; Icons and modeline
(use-package nerd-icons)
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  :init
  (doom-modeline-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
