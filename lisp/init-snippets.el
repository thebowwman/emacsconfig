;;; init-snippets.el --- snippets with yasnippet -*- lexical-binding: t; -*-

(require 'init-keys)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(my/leader
  "is" '(yas-insert-snippet :which-key "insert snippet"))

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  (define-key yas-keymap (kbd "C-c n") #'yas-next-field)
  (define-key yas-keymap (kbd "C-c p") #'yas-prev-field))

(provide 'init-snippets)
;;; init-snippets.el ends here
