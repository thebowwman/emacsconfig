;;; init-completion.el --- minibuffer and in-buffer completion -*- lexical-binding: t; -*-

(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode 1))

(provide 'init-completion)
;;; init-completion.el ends here
