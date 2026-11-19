;;; init-python.el --- Python: tree-sitter, venvs, LSP, ruff -*- lexical-binding: t; -*-

(setq major-mode-remap-alist '((python-mode . python-ts-mode)))
(setq treesit-font-lock-level 4)

(use-package pet
  :init
  (add-hook 'python-base-mode-hook #'pet-mode -10))

(use-package eglot
  :ensure nil
  :hook (python-ts-mode . eglot-ensure)
  :config
  (defun my/basedpyright-contact (&rest _)
    (list (or (pet-executable-find "basedpyright-langserver")
              "basedpyright-langserver")
          "--stdio"))
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . my/basedpyright-contact))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-base-mode)
                (eglot-semantic-tokens-mode -1)))))

(use-package flymake-ruff
  :hook (python-ts-mode . flymake-ruff-load))

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode))

(my/leader
  "ce" '(flymake-show-buffer-diagnostics :which-key "error list")
  "cn" '(flymake-goto-next-error :which-key "next error")
  "cp" '(flymake-goto-prev-error :which-key "previous error"))

(defcustom my/python-main-program "main.py"
  "Entry file for this project, relative to the project root."
  :type 'string
  :safe #'stringp
  :group 'python)

(defun my/python-command ()
  "The project's venv Python, falling back to `python'."
  (or (pet-executable-find "python") "python"))

(defun my/python-main-path ()
  "Absolute path of this project's entry file."
  (expand-file-name my/python-main-program
                    (project-root (project-current t))))

(defun my/python-project-fn (config)
  "Set the program to this project's entry file before launching."
  (plist-put config :program (my/python-main-path)))

(with-eval-after-load 'dape
  (setf (alist-get 'python-project dape-configs)
        (thread-first (copy-tree (alist-get 'debugpy dape-configs))
                      (plist-put 'command #'my/python-command)
                      (plist-put 'fn #'my/python-project-fn))))

(use-package python-pytest
  :custom
  (python-pytest-executable "uv run pytest"))

(my/leader
  "ct" '(python-pytest-dispatch :which-key "run tests"))

(use-package apheleia
  :config
  ;; Run ruff from the project's venv, using its pyproject.toml settings
  (add-to-list 'apheleia-formatters
               '(ruff-project "uv" "run" "ruff" "format"
                              "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff-project)
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff-project)
  (apheleia-global-mode 1))

(my/leader
  "cf" '(apheleia-format-buffer :which-key "format buffer"))

(provide 'init-python)
;;; init-python.el ends here
