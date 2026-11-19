;;; init-lsp.el --- language server commands -*- lexical-binding: t; -*-

(require 'init-keys)

;; K shows the documentation for the symbol under the cursor
(with-eval-after-load 'evil
  (setq evil-lookup-func #'eldoc-doc-buffer))

(my/leader
  "c"  '(:ignore t :which-key "code")
  "cd" '(xref-find-definitions :which-key "go to definition")
  "cr" '(xref-find-references :which-key "references")
  "ca" '(eglot-code-actions :which-key "code actions")
  "cR" '(eglot-rename :which-key "rename symbol"))


(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
