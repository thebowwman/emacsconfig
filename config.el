;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; To load the enviroment variable to emacs config
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-variables '("PATH" "PYENV_ROOT" "POETRY_HOME"))
    (exec-path-from-shell-initialize)))

(defun my/project-maybe-deactivate-venv ()
  "Deactivate Python venv when entering a non-Python project."
  (unless (or (locate-dominating-file default-directory "Pipfile")
              (locate-dominating-file default-directory "pyproject.toml")
              (locate-dominating-file default-directory "requirements.txt")
              (locate-dominating-file default-directory ".venv"))
    (when (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
      (pyvenv-deactivate))))
(add-hook 'projectile-after-switch-project-hook #'my/project-maybe-deactivate-venv)


;; Auto-switch Poetry venvs as you switch Python buffers/projects
(use-package! poetry
  :after python
  :config
  (add-hook 'python-mode-hook (lambda () (poetry-tracking-mode 1))))

;; When the venv changes, gently restart Pyright so it picks it up
(with-eval-after-load 'lsp-pyright
  (dolist (fn '(poetry-venv-workon poetry-venv-activate))
    (advice-add fn :after (lambda (&rest _)
                            (when (bound-and-true-p lsp-mode)
                              (lsp-restart-workspace))))))


;; Tell gopls to build with -tags=integration
(after! lsp-mode
  (setq lsp-go-build-flags ["-tags=integration"])
  ;; If the above var isn’t available in your lsp-mode, force it via gopls settings:
  (lsp-register-custom-settings
   '(("gopls.buildFlags" ["-tags=integration"]))))




;; Ensure .tsx uses correct mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;; Use LSP for TypeScript & React
(after! lsp-mode
  (setq lsp-enable-snippet t
        lsp-enable-indentation nil       ; disable LSP formatting, use Prettier/ESLint
        lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")
        lsp-typescript-npm-ls-path nil)) ; let lsp find tsserver automatically

;; Prettier integration
(use-package! prettier
  :hook ((typescript-mode . prettier-mode)
         (typescript-tsx-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

;; Use prettier by default for formatting
(setq-hook! 'typescript-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)

;; If you prefer ESLint formatting instead of Prettier:
;; (setq-hook! 'typescript-mode-hook +format-with 'eslint_d)
;; (setq-hook! 'typescript-tsx-mode-hook +format-with 'eslint_d)

;; Company (completion) tweaks
(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ; instant completions

;; Snippets for React (expand with `yas-expand`)
(use-package! yasnippet
  :config
  (yas-global-mode 1))

;; Example: add a "rfc" snippet for React functional components
(yas-define-snippets 'typescript-tsx-mode
                     '(("rfc"
                        "import React from 'react';\n\nexport default function ${1:Component}() {\n  return (\n    <div>$0</div>\n  );\n}"
                        "React functional component")))

;; Optional: better syntax highlighting
(after! tree-sitter
  (add-hook 'typescript-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-tsx-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'typescript-tsx-mode-hook #'tree-sitter-hl-mode))






;; Ensure LSP uses the correct interpreter after venv is activated
;;(load! "modules/python.el")
(load! "modules/modelline.el")
(load! "modules/vertico.el")
(load! "modules/marginalia.el")
(load! "modules/consult.el")
(load! "modules/company.el")
(load! "modules/org.el")
(load! "modules/org-roam.el")
(load! "modules/evil-mode.el")
