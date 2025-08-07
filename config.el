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

;; Setup for the pyenv to select the proper virtual environment for the project

(use-package pyvenv
  :ensure t ;; ensure that the package is installed
  :config   ;; excute the body under config after the package has been loaded
  (pyvenv-mode 1)
  (message "[pyvenv] pyvenv-mode enabled.")

  ;; Update python-shell-interpreter when venv is activated
  ;; list of functions to run once the virtual environment is activated
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (let ((interpreter (expand-file-name "bin/python3" pyvenv-virtual-env)))
                  (setq python-shell-interpreter interpreter)
                  (message "[pyvenv] Activated venv: %s" pyvenv-virtual-env)
                  (message "[pyvenv] Set python-shell-interpreter to: %s" interpreter)))))

  ;; Restore default interpreter when deactivating
  ;; list of function to run after the virtual environment is deactivated
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
                (message "[pyvenv] Deactivated venv. Restored python-shell-interpreter to default.")))))

;; Fallback parser for Poetry venv path
;; run the poetry show -v to get the virtual env location and add it to the
(defun my/get-poetry-venv ()
  "Return the Poetry virtualenv path using `poetry env info -p`, or fallback to parsing `poetry show -v`."
  (let ((venv (string-trim (shell-command-to-string "poetry env info -p"))))
    (if (and venv (not (string-empty-p venv)))
        venv
      ;; fallback to parsing `poetry show -v`
      (let* ((output (shell-command-to-string "poetry show -v"))
             (lines (split-string output "\n"))
             (venv-line (seq-find (lambda (line)
                                    (string-match-p "Using virtualenv:" line))
                                  lines))
             (path (when venv-line
                     (string-trim (replace-regexp-in-string "Using virtualenv: " "" venv-line)))))
        (when (and path (not (string-empty-p path)))
          (message "[poetry] Fallback venv path from show -v: %s" path)
          path)))))


(defun my/auto-activate-poetry-venv ()
  "Automatically activate Poetry venv if a pyproject.toml is found."
  (when (and (executable-find "poetry")
             (locate-dominating-file default-directory "pyproject.toml"))
    (let ((venv (my/get-poetry-venv)))
      (if (not venv)
          (message "[poetry] No Poetry venv found — skipping activation.")
        (message "[poetry] Poetry venv path: %s" venv)
        (when (and (file-exists-p venv)
                   (not (string-equal venv pyvenv-virtual-env)))
          (pyvenv-activate venv))))))

(add-hook 'python-mode-hook #'my/auto-activate-poetry-venv)

;; Ensure LSP uses the correct interpreter after venv is activated
(defun my/update-lsp-interpreter ()
  "Update LSP pyright interpreter to match activated venv."
  (when (and (boundp 'lsp-pyright-python-executable-cmd)
             (boundp 'python-shell-interpreter))
    (setq lsp-pyright-python-executable-cmd python-shell-interpreter)
    (message "[lsp] Updated lsp-pyright-python-executable-cmd to: %s"
             lsp-pyright-python-executable-cmd)))

(add-hook 'pyvenv-post-activate-hooks #'my/update-lsp-interpreter)

;; Restart LSP after updating interpreter
(defun my/restart-lsp-after-venv ()
  "Restart LSP to pick up new Python interpreter after venv change."
  (when (bound-and-true-p lsp-mode)
    (message "[lsp] Restarting LSP workspace to use new interpreter.")
    (lsp-restart-workspace)))

(add-hook 'pyvenv-post-activate-hooks #'my/restart-lsp-after-venv)

;; Initial setup (run once after Doom loads)
(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd python-shell-interpreter)
  (message "[lsp-pyright] Set executable cmd to: %s" lsp-pyright-python-executable-cmd))


