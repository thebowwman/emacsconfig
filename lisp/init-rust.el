;;; init-rust.el --- Rust: tree-sitter, rust-analyzer, rustfmt -*- lexical-binding: t; -*-

(require 'init-keys)

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(use-package eglot
  :ensure nil
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer"))))

(with-eval-after-load 'apheleia
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(setq-default eglot-workspace-configuration
              '((:rust-analyzer . (:check (:command "clippy")))))


(defcustom my/rust-binary-name nil
  "Name of the binary to debug, as in Cargo.toml.
When nil, the project folder name is used."
  :type '(choice (const nil) string)
  :safe #'string-or-null-p
  :group 'rust)

(defun my/rust-program ()
  "Path to the debug binary of this Cargo project."
  (let* ((root (project-root (project-current t)))
         (name (or my/rust-binary-name
                   (file-name-nondirectory (directory-file-name root)))))
    (expand-file-name (format "target/debug/%s" name) root)))

(with-eval-after-load 'dape
  (setf (alist-get 'rust-project dape-configs)
        (thread-first (copy-tree (alist-get 'lldb-dap dape-configs))
                      (plist-put 'compile "cargo build")
                      (plist-put :program #'my/rust-program))))

(provide 'init-rust)

;;; init-rust.el ends here
