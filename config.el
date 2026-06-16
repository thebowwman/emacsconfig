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


;; -------------------------------
;; Dynamic NVM / Node path detection
;; -------------------------------
(defun my/nvm-add-to-path ()
  "Dynamically find and add the active nvm node version to exec-path and PATH."
  (let* ((nvm-dir (expand-file-name "~/.nvm/versions/node"))
         (default-version-file (expand-file-name "~/.nvm/alias/default"))
         ;; Try reading the default alias first
         (node-version
          (when (file-exists-p default-version-file)
            (string-trim (with-temp-buffer
                           (insert-file-contents default-version-file)
                           (buffer-string)))))
         ;; If no default alias, fall back to highest installed version
         (node-version
          (or node-version
              (when (file-directory-p nvm-dir)
                (car (last (sort
                            (directory-files nvm-dir nil "^v[0-9]")
                            #'string<))))))
         (node-bin (when node-version
                     (expand-file-name (concat node-version "/bin") nvm-dir))))
    (when (and node-bin (file-directory-p node-bin))
      (add-to-list 'exec-path node-bin)
      (setenv "PATH" (concat node-bin ":" (getenv "PATH")))
      (message "Node.js added to PATH from nvm: %s" node-bin))))

;; Run early so LSP and other tools can find node
(my/nvm-add-to-path)


;; -------------------------------
;; TSX / TypeScript Mode Fix
;; -------------------------------

;; Force .tsx files to open in tsx-ts-mode (built-in Emacs 29+ tree-sitter mode)
;; instead of typescript-ts-mode which Doom maps by default
(setq auto-mode-alist
      (cons '("\\.tsx\\'" . tsx-ts-mode)
            (delete '("\\.tsx\\'" . typescript-ts-mode) auto-mode-alist)))

;; Hook LSP into the tree-sitter modes so autocomplete works
(add-hook 'tsx-ts-mode-hook #'lsp!)
(add-hook 'typescript-ts-mode-hook #'lsp!)


;; -------------------------------
;; Buffer Management - Auto-close inactive saved buffers
;; -------------------------------

(defvar my/buffer-idle-timeout (* 10 60) ; 10 minutes in seconds
  "Time in seconds before an inactive saved buffer is automatically closed.")

(defvar my/buffer-never-kill-list
  '("*scratch*" "*Messages*" "*dashboard*" "*doom*")
  "List of buffer names that should never be auto-killed.")

(defvar my/buffer-never-kill-patterns
  '("^magit" "^\\*Org Src" "^\\*Org Agenda")
  "Regexp patterns for buffers that should never be auto-killed.")

(defun my/should-auto-kill-buffer-p (buffer)
  "Return non-nil if BUFFER should be auto-killed when inactive."
  (let ((name (buffer-name buffer)))
    (and
     ;; Has a file
     (buffer-file-name buffer)
     ;; Not modified (saved)
     (not (buffer-modified-p buffer))
     ;; Not in never-kill list
     (not (member name my/buffer-never-kill-list))
     ;; Doesn't match never-kill patterns
     (not (cl-some (lambda (pattern) (string-match-p pattern name))
                   my/buffer-never-kill-patterns))
     ;; Not currently visible in any window
     (not (get-buffer-window buffer t)))))

(defun my/kill-idle-buffers ()
  "Kill buffers that are saved and have been inactive for more than `my/buffer-idle-timeout'."
  (interactive)
  (let ((current-time (current-time))
        (killed-count 0))
    (dolist (buffer (buffer-list))
      (when (my/should-auto-kill-buffer-p buffer)
        (let* ((last-access (or (buffer-local-value 'buffer-display-time buffer)
                               (with-current-buffer buffer
                                 (visited-file-modtime))))
               (idle-time (if (time-less-p last-access current-time)
                              (time-to-seconds (time-subtract current-time last-access))
                            0)))
          (when (> idle-time my/buffer-idle-timeout)
            (kill-buffer buffer)
            (setq killed-count (1+ killed-count))))))
    (when (> killed-count 0)
      (message "Auto-killed %d idle buffer%s" killed-count (if (= killed-count 1) "" "s")))))

;; Run cleanup every 2 minutes
(run-with-timer 120 120 #'my/kill-idle-buffers)

;; Recentf - remember recently opened files
(use-package! recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 50)
  (run-at-time nil (* 5 60) 'recentf-save-list)) ; Save every 5 minutes

;; Load PATH from shell
(use-package! exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "GOROOT"))
  (exec-path-from-shell-initialize))

;; Ensure npm global bin is in PATH
(add-to-list 'exec-path (expand-file-name "~/.npm-global/bin"))

;; Ensure Go tools are in PATH
(add-to-list 'exec-path "/usr/local/go/bin")
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(setenv "PATH" (concat (expand-file-name "~/go/bin") ":" (getenv "PATH")))

;; Ensure Cargo/Rust tools are in PATH
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setenv "PATH" (concat (expand-file-name "~/.cargo/bin") ":" (getenv "PATH")))

(use-package! pyvenv
  :config
  (pyvenv-mode 1)

  ;; Helper function to find and activate .venv
  (defun my/auto-activate-venv ()
    "Auto-activate .venv if found in project root or parent directories."
    (let ((venv-dir (locate-dominating-file default-directory ".venv")))
      (when venv-dir
        (let ((venv-path (expand-file-name ".venv" venv-dir)))
          (unless (and (boundp 'pyvenv-virtual-env)
                       (string= pyvenv-virtual-env venv-path))
            (pyvenv-activate venv-path)
            (message "Activated venv: %s" venv-path))))))

  ;; Auto-manage venv on project/workspace switch.
  ;; If the new project has a .venv, activate it. If not, deactivate.
  ;; This prevents venv from bleeding into non-Python workspaces.
  ;; Uses the current buffer's directory or projectile root to find .venv,
  ;; with a short delay on workspace switch so the buffer has time to update.
  (defun my/pyvenv-on-project-switch ()
    "Activate .venv if the project has one, otherwise deactivate."
    (let* ((dir (or (and (fboundp 'projectile-project-root)
                         (ignore-errors (projectile-project-root)))
                    default-directory))
           (venv-dir (locate-dominating-file dir ".venv")))
      (if venv-dir
          (let ((venv-path (expand-file-name ".venv" venv-dir)))
            (unless (and (boundp 'pyvenv-virtual-env)
                         (string= pyvenv-virtual-env venv-path))
              (pyvenv-activate venv-path)
              (message "Activated venv: %s" venv-path)))
        ;; No .venv in this project — deactivate if one is active
        (when (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
          (pyvenv-deactivate)))))

  ;; Activate venv when opening Python files
  (add-hook 'python-mode-hook #'my/auto-activate-venv)
  (add-hook 'python-ts-mode-hook #'my/auto-activate-venv)

  ;; Activate/deactivate on project switch
  (add-hook 'projectile-after-switch-project-hook #'my/pyvenv-on-project-switch)
  ;; On workspace switch, delay slightly so the buffer updates first.
  ;; Skip during kill-emacs (Doom cycles workspaces to save them on quit).
  (defvar my/emacs-quitting nil "Non-nil when Emacs is shutting down.")
  (add-hook 'kill-emacs-hook (lambda () (setq my/emacs-quitting t)) -100)
  (advice-add #'save-buffers-kill-emacs :before
              (lambda (&rest _) (setq my/emacs-quitting t)))

  (add-hook 'persp-activated-functions
            (lambda (_name)
              (unless my/emacs-quitting
                (run-at-time 0.1 nil #'my/pyvenv-on-project-switch)))))


(setq python-shell-interpreter "python")

;; -------------------------------
;; Basedpyright LSP (stricter pyright fork — matches pre-commit)
;; Uses basedpyright-langserver from the project venv.
;; -------------------------------
;; Disable other Python LSP servers — basedpyright is our only Python LSP
(setq lsp-disabled-clients '(pylsp pyright pyls semgrep-ls ruff ty-ls))

;; Start LSP AFTER venv activation. Hook depth -90 ensures lsp! runs after
;; my/auto-activate-venv (depth 0), so the venv is active and
;; basedpyright-langserver is in PATH when LSP starts.
(add-hook 'python-mode-hook #'lsp! -90)
(add-hook 'python-ts-mode-hook #'lsp! -90)

(after! lsp-mode
  ;; Register basedpyright as a higher-priority LSP client.
  ;; basedpyright uses the same wire protocol as pyright (python.* settings)
  ;; but reads typeCheckingMode from [tool.basedpyright] in pyproject.toml.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (list (or (executable-find "basedpyright-langserver")
                                 "basedpyright-langserver")
                             "--stdio")))
    :major-modes '(python-mode python-ts-mode)
    :server-id 'basedpyright
    :multi-root t
    :priority 3  ;; higher than pyright (priority 2)
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "python"))))
    :notification-handlers (lsp-ht)))

  ;; Settings sent via LSP — basedpyright uses python.analysis.* like pyright
  (lsp-register-custom-settings
   '(("python.analysis.autoImportCompletions" t t)
     ("python.analysis.useLibraryCodeForTypes" t t)
     ("python.analysis.autoSearchPaths" t t)
     ("python.analysis.diagnosticMode" "openFilesOnly"))))

;; Optional: auto-organize imports when saving
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
(add-hook 'python-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; -------------------------------
;; LSP Configuration (consolidated)
;; -------------------------------
(after! lsp-mode
  ;; Session Management — persist workspace folders so LSP remembers project roots
  ;; (prevents "not part of any project" prompts on every restart)
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 500)
  (setq lsp-keep-workspace-alive nil)  ; Kill workspace when last buffer closes

  ;; Route LSP diagnostics through flycheck so sideline/inline packages see them
  (setq lsp-diagnostics-provider :flycheck)

  ;; Go configuration
  (setq lsp-go-build-flags ["-tags=integration"])
  (lsp-register-custom-settings
   '(("gopls.buildFlags" ["-tags=integration"])))

  ;; TypeScript & React configuration (enhanced for full-stack)
  (setq lsp-enable-snippet t
        lsp-enable-indentation nil       ; disable LSP formatting, use Prettier/ESLint
        lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")
        lsp-typescript-npm-ls-path nil)  ; let lsp find tsserver automatically

  ;; Enhanced TypeScript/JavaScript LSP settings
  (lsp-register-custom-settings
   '(("typescript.suggest.autoImports" t t)
     ("typescript.updateImportsOnFileMove.enabled" "always" t)
     ("typescript.preferences.importModuleSpecifier" "relative" t)
     ("typescript.preferences.quoteStyle" "single" t)
     ("javascript.suggest.autoImports" t t)
     ("javascript.updateImportsOnFileMove.enabled" "always" t))))


;; -------------------------------
;; LSP UI — fully committed to sideline + sideline-flycheck stack
;;
;; Diagnostics:   LSP → flycheck → sideline-flycheck (catches clippy, eslint,
;;                pyright — anything flycheck sees, not just LSP sources)
;; Code actions:  LSP → sideline-lsp (lightbulb hints inline, no lsp-ui needed)
;; Result:        lsp-ui-sideline disabled entirely; no duplicate rendering.
;; -------------------------------
(after! lsp-ui
  ;; Disable lsp-ui-sideline completely — sideline-flycheck owns the right gutter
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  ;; Keep lsp-ui-doc for hover popups (peek on K) — still useful
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)   ; only on demand (K), not auto-popup
  (setq lsp-ui-doc-show-with-mouse nil))


;; -------------------------------
;; Rust LSP (rust-analyzer)
;; -------------------------------

;; Hook LSP into rustic-mode (Doom uses rustic-mode, not rust-mode)
(add-hook 'rustic-mode-hook #'lsp!)

(after! lsp-rust
  ;; Inlay hints - type info, chaining, parameter names
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-max-inlay-hint-length 25)
  (setq lsp-inlay-hint-enable t)

  ;; Proc macros support (essential for serde, thiserror, etc.)
  (setq lsp-rust-analyzer-proc-macro-enable t)

  ;; Use clippy instead of plain cargo check (much richer linting)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Auto-import completions (note: trailing 's' in variable name)
  (setq lsp-rust-analyzer-completion-add-call-parentheses t)

  ;; Cargo features awareness
  (setq lsp-rust-analyzer-cargo-all-features t))

;; Auto-format on save using rustfmt
(add-hook 'rustic-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; -------------------------------
;; Rust Clippy / Flycheck Integration
;; -------------------------------
(after! (rustic flycheck)
  ;; Use clippy for flycheck — must wait for both rustic AND flycheck
  (setq rustic-flycheck-clippy-params "--message-format=json")
  (push 'rustic-clippy flycheck-checkers))

;; -------------------------------
;; Flycheck (consolidated)
;; -------------------------------
(after! flycheck
  ;; Fringe indicator only, symbol underline — sideline handles the text
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-highlighting-mode 'symbols)
  ;; Disable flycheck's own inline display — sideline handles it on the right
  (setq flycheck-display-errors-function nil)

  ;; Disable jshint in favour of eslint for JS/TS
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode))

;; -------------------------------
;; Inline Diagnostics (Error Lens style)
;; -------------------------------

;; flycheck-inline disabled — sideline already shows errors on the right,
;; enabling both causes tripled messages
;; (use-package! flycheck-inline
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-inline-mode))

;; sideline shows diagnostics + code actions inline on the right side of the buffer
;; Backend order: sideline-lsp first (code actions / lightbulbs),
;;                sideline-flycheck second (errors from clippy, eslint, pyright, etc.)
(use-package! sideline
  :after flycheck
  :hook (flycheck-mode . sideline-mode)
  :config
  (setq sideline-backends-right '(sideline-lsp sideline-flycheck)
        sideline-delay 0.2
        sideline-display-backend-name t))

(use-package! sideline-flycheck
  :after (sideline flycheck)
  :hook (flycheck-mode . sideline-flycheck-setup))

;; sideline-lsp: shows LSP code actions inline (lightbulb hints)
;; replaces lsp-ui-sideline code-action display without enabling lsp-ui-sideline
(use-package! sideline-lsp
  :after (sideline lsp-mode)
  :hook (lsp-mode . sideline-mode))


;; -------------------------------
;; Dape (Debug Adapter Protocol for Emacs)
;; Replaces dap-mode — zero dependencies, built into Doom's debugger module
;; Doom's debugger module handles: projectile CWD, breakpoint persistence,
;; window arrangement (right), inlay hints, and REPL mode-line.
;; Below we add language-specific configs and keybindings.
;; -------------------------------

;; Autoload dape commands so SPC d keybindings work before dape is loaded
(dolist (cmd '(dape-pause dape-continue dape-next dape-step-in dape-step-out
              dape-restart dape-restart-frame dape-info dape-repl dape-memory
              dape-disassemble dape-breakpoint-log dape-breakpoint-expression
              dape-breakpoint-hits dape-breakpoint-toggle dape-breakpoint-remove-all
              dape-select-thread dape-select-stack dape-stack-select-down
              dape-stack-select-up dape-evaluate-expression dape-watch-dwim
              dape-disconnect-quit dape-quit dape-until dape-breakpoint-remove-at-point))
  (autoload cmd "dape" nil t))

(after! dape
  ;; Save buffers before starting a debug session (useful for interpreted langs)
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; --- Python (debugpy) ---
  ;; Built-in dape config `debugpy` already handles python-mode / python-ts-mode.
  ;; Defaults: justMyCode nil, showReturnValue t — matches our old dap-mode setup.
  ;; Override command to dynamically resolve the venv python at debug time.
  ;; The system may not have a bare `python` binary, and debugpy is only
  ;; installed inside the project venv.
  (defun my/dape-python-command ()
    "Return the python executable from the active venv, or fall back to python3."
    (cond
     ;; pyvenv is active — use its python
     ((and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
      (expand-file-name "bin/python" pyvenv-virtual-env))
     ;; .venv in project root
     ((and (fboundp 'projectile-project-root)
           (file-executable-p (expand-file-name ".venv/bin/python" (projectile-project-root))))
      (expand-file-name ".venv/bin/python" (projectile-project-root)))
     ;; fallback
     (t "python3")))

  (let ((debugpy-config (alist-get 'debugpy dape-configs))
        (debugpy-module-config (alist-get 'debugpy-module dape-configs)))
    (when debugpy-config
      (plist-put debugpy-config 'command #'my/dape-python-command))
    (when debugpy-module-config
      (plist-put debugpy-module-config 'command #'my/dape-python-command)))

  ;; --- Go (delve) ---
  ;; Built-in dape config `dlv` handles go-mode / go-ts-mode.
  ;; Uses `dlv dap --listen 127.0.0.1:<autoport>` — same adapter as before.
  ;; Integration build tags are handled by gopls (LSP), not the debugger.
  ;; For test debugging, use: M-x dape → dlv :mode "test" :program "."

  ;; --- TypeScript / Node.js (js-debug) ---
  ;; Built-in configs: js-debug-node, js-debug-ts-node, js-debug-tsx,
  ;;                   js-debug-node-attach, js-debug-chrome
  ;; Add tsx-ts-mode to all JS/TS debug configs so they appear in .tsx buffers
  (dolist (config-name '(js-debug-ts-node js-debug-tsx js-debug-node-attach js-debug-chrome))
    (when-let* ((config (alist-get config-name dape-configs)))
      (let ((modes (plist-get config 'modes)))
        (unless (memq 'tsx-ts-mode modes)
          (plist-put config 'modes (append modes '(tsx-ts-mode)))))))

  ;; --- Rust / C / C++ (lldb-dap) ---
  ;; Uses lldb-dap-20 from LLVM. Has proper expression REPL (like debugpy).
  ;; Override the built-in lldb-dap config to use the versioned binary name.
  (when-let* ((lldb-config (alist-get 'lldb-dap dape-configs)))
    (plist-put lldb-config 'command "lldb-dap-20"))

  ;; Helper: read the package name from Cargo.toml
  (defun my/cargo-project-name ()
    "Read the package name from Cargo.toml in the project root."
    (let ((cargo-toml (expand-file-name "Cargo.toml" (projectile-project-root))))
      (when (file-exists-p cargo-toml)
        (with-temp-buffer
          (insert-file-contents cargo-toml)
          (when (re-search-forward "^name *= *\"\\([^\"]+\\)\"" nil t)
            (match-string 1))))))

  ;; Add a Rust-specific dape config that auto-builds and auto-detects binary
  ;; Usage: SPC d d → rust-lldb (just hit Enter, no :program needed)
  (add-to-list 'dape-configs
               `(rust-lldb
                 modes (rustic-mode rust-mode rust-ts-mode)
                 ensure dape-ensure-command
                 command "lldb-dap-20"
                 command-cwd dape-command-cwd
                 compile "cargo build"
                 :cwd "."
                 :program ,(lambda ()
                             (let ((name (my/cargo-project-name)))
                               (if name
                                   (concat "target/debug/" (replace-regexp-in-string "-" "_" name))
                                 "target/debug/a.out")))
                 :args []))

  ;; Also add a test variant
  (add-to-list 'dape-configs
               `(rust-lldb-test
                 modes (rustic-mode rust-mode rust-ts-mode)
                 ensure dape-ensure-command
                 command "lldb-dap-20"
                 command-cwd dape-command-cwd
                 compile "cargo test --no-run"
                 :cwd "."
                 :program ,(lambda ()
                             (let* ((root (projectile-project-root))
                                    (deps-dir (concat root "target/debug/deps/"))
                                    (test-binaries
                                     (when (file-directory-p deps-dir)
                                       (seq-filter
                                        (lambda (f)
                                          (and (file-executable-p (concat deps-dir f))
                                               (not (string-match-p "\\." f))))
                                        (directory-files deps-dir nil nil t)))))
                               (if test-binaries
                                   (concat "target/debug/deps/"
                                           (completing-read "Test binary: " test-binaries nil t))
                                 "target/debug/deps/")))
                 :args ["--nocapture"]))
  )


;; -------------------------------
;; Enhanced IBuffer for Buffer Management
;; -------------------------------
(after! ibuffer
  ;; Group buffers by project
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Org" (mode . org-mode))
           ("Programming" (or
                          (mode . python-mode)
                          (mode . go-mode)
                          (mode . typescript-mode)
                          (mode . typescript-ts-mode)
                          (mode . tsx-ts-mode)
                          (mode . rust-mode)
                          (mode . c-mode)
                          (mode . c++-mode)))
           ("Dired" (mode . dired-mode))
           ("Magit" (name . "^magit"))
           ("Emacs" (or
                    (name . "^\\*scratch\\*$")
                    (name . "^\\*Messages\\*$")
                    (name . "^\\*dashboard\\*$")))
           ("Help" (or
                   (name . "^\\*Help\\*$")
                   (name . "^\\*info\\*$")
                   (name . "^\\*apropos\\*$")))
           ("Temp" (name . "^\\*.*\\*$")))))

  ;; Use human-readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  ;; Auto-update ibuffer
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; Keybindings for quick buffer cleanup
(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Kill other buffers" "O" #'doom/kill-other-buffers
       :desc "Kill buried buffers" "C" #'doom/kill-buried-buffers
       :desc "Kill project buffers" "P" #'doom/kill-project-buffers
       :desc "IBuffer" "I" #'ibuffer))




(use-package! repeat
  :config
  (repeat-mode 1))

;; Left and right side windows occupy full frame height
(use-package! emacs
  :custom
  (window-sides-vertical t))

;; -------------------------------
;; Quality of Life (Karthinks batteries)
;; -------------------------------

;; subword-mode — CamelCase-aware word navigation for Go/TS/Rust
;; Makes w/b/e/dw stop at CamelCase boundaries: getUserById → get|User|By|Id
(add-hook 'go-mode-hook #'subword-mode)
(add-hook 'go-ts-mode-hook #'subword-mode)
(add-hook 'typescript-mode-hook #'subword-mode)
(add-hook 'typescript-ts-mode-hook #'subword-mode)
(add-hook 'tsx-ts-mode-hook #'subword-mode)
(add-hook 'rustic-mode-hook #'subword-mode)
(add-hook 'rust-ts-mode-hook #'subword-mode)
(add-hook 'js-mode-hook #'subword-mode)
(add-hook 'js-ts-mode-hook #'subword-mode)

;; duplicate-dwim — duplicate line or region (like Ctrl+Shift+D in VSCode)
(map! :n "C-S-d" #'duplicate-dwim
      :v "C-S-d" #'duplicate-dwim)

;; view-read-only — pager-like bindings in read-only buffers
;; SPC to scroll, q to quit, s to search. Great for browsing library source.
(setq view-read-only t)

;; undelete-frame-mode — undo accidental frame close
(undelete-frame-mode 1)

;; find-function-on-key — jump from keybinding to source code in one step
(map! :leader
      :desc "Find function on key" "h M-k" #'find-function-on-key)


;; -------------------------------
;; TypeScript & React Configuration (Full-Stack)
;; -------------------------------

;; Prettier integration
(use-package! prettier
  :hook ((typescript-mode . prettier-mode)
         (typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

;; Use prettier by default for formatting
(setq-hook! 'typescript-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-ts-mode-hook +format-with 'prettier)
(setq-hook! 'tsx-ts-mode-hook +format-with 'prettier)

;; Auto-organize imports on save (like Python)
(add-hook 'typescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; -------------------------------
;; React & Next.js Snippets (TypeScript)
;; -------------------------------
;; Enhanced React/Next.js snippets for TypeScript
(after! yasnippet
(yas-define-snippets 'tsx-ts-mode
                     '(("rfc"
                        "import React from 'react';\n\nexport default function ${1:Component}() {\n  return (\n    <div>$0</div>\n  );\n}"
                        "React functional component")

                       ("rfce"
                        "import React from 'react';\n\nexport default function ${1:Component}() {\n  return (\n    <>\n      $0\n    </>\n  );\n}"
                        "React functional component with fragment")

                       ("rfcp"
                        "import React from 'react';\n\ninterface ${1:Component}Props {\n  $2\n}\n\nexport default function ${1:Component}({ $3 }: ${1:Component}Props) {\n  return (\n    <div>$0</div>\n  );\n}"
                        "React functional component with props interface")

                       ("useState"
                        "const [${1:state}, set${1:$(capitalize yas-text)}] = useState$2($3);"
                        "useState hook")

                       ("useEffect"
                        "useEffect(() => {\n  $0\n}, [$1]);"
                        "useEffect hook")

                       ("nextpage"
                        "export default function ${1:Page}() {\n  return (\n    <div>\n      $0\n    </div>\n  );\n}"
                        "Next.js page component")

                       ("nextapi"
                        "import { NextRequest, NextResponse } from 'next/server';\n\nexport async function ${1:GET}(request: NextRequest) {\n  $0\n  return NextResponse.json({ data: null });\n}"
                        "Next.js API route")

                       ("nextlayout"
                        "import type { Metadata } from 'next';\n\nexport const metadata: Metadata = {\n  title: '${1:Title}',\n  description: '${2:Description}',\n};\n\nexport default function ${3:Layout}({\n  children,\n}: {\n  children: React.ReactNode;\n}) {\n  return (\n    <>\n      {children}\n    </>\n  );\n}"
                        "Next.js layout component")

                       ("hono"
                        "import { Hono } from 'hono';\n\nconst app = new Hono();\n\napp.get('/${1:path}', (c) => {\n  return c.json({ $0 });\n});\n\nexport default app;"
                        "Hono API route"))))

;; Optional: better syntax highlighting via tree-sitter
;; Tree-sitter highlighting for non-ts-mode buffers only.
;; *-ts-mode (typescript-ts-mode, tsx-ts-mode) already use built-in tree-sitter;
;; adding the external tree-sitter-mode on top is redundant and may conflict.
(after! tree-sitter
  (add-hook 'typescript-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)
  ;; Rust tree-sitter (rustic-mode is not a -ts-mode)
  (add-hook 'rustic-mode-hook #'tree-sitter-mode)
  (add-hook 'rustic-mode-hook #'tree-sitter-hl-mode))


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

;; -------------------------------
;; Native Compilation Settings
;; -------------------------------
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Add project eln-cache to native compilation load path (don't replace the list)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory))

  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Number of async compilation jobs
  (setq native-comp-async-jobs-number 4)

  ;; Optimization level (2 = max optimization)
  (setq native-comp-speed 2))

;; Tree-sitter grammar load path
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))

;; Register tsx/typescript/rust grammar sources so treesit-install-language-grammar works
(setq treesit-language-source-alist
      '((tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))))

;; -------------------------------
;; Proxy Configuration for VPN
;; -------------------------------
(defun my/setup-proxy-from-env ()
  "Configure Emacs to use proxy from environment variables."
  (let ((http-proxy (or (getenv "http_proxy") (getenv "HTTP_PROXY")))
        (https-proxy (or (getenv "https_proxy") (getenv "HTTPS_PROXY"))))
    (if (and http-proxy https-proxy)
        (progn
          ;; Parse proxy URL
          (when (string-match "http://\\([^:]+\\):\\([0-9]+\\)" http-proxy)
            (let ((proxy-host (match-string 1 http-proxy))
                  (proxy-port (match-string 2 http-proxy)))
              (setq url-proxy-services
                    `(("http" . ,(concat proxy-host ":" proxy-port))
                      ("https" . ,(concat proxy-host ":" proxy-port))))
              (message "Emacs proxy configured: %s:%s" proxy-host proxy-port))))
      ;; No proxy set - disable proxy
      (setq url-proxy-services nil)
      (message "Emacs proxy disabled"))))

;; Apply proxy settings on startup
(my/setup-proxy-from-env)

;; Optional: Command to refresh proxy settings without restarting Emacs
(defun my/toggle-proxy ()
  "Refresh proxy settings from environment."
  (interactive)
  (my/setup-proxy-from-env))

;; -------------------------------
;; Agent Shell - AI Assistant
;; -------------------------------

;; Helper function to read Claude Code settings
(defun my/load-claude-settings ()
  "Load Claude Code settings from ~/.claude/settings.json"
  (let* ((settings-file (expand-file-name "~/.claude/settings.json"))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (when (file-exists-p settings-file)
      (with-temp-buffer
        (insert-file-contents settings-file)
        (json-read)))))

;; Load settings and set environment variables
(let ((settings (my/load-claude-settings)))
  (when settings
    (let ((env (gethash "env" settings)))
      (when env
        (setenv "ANTHROPIC_AUTH_TOKEN" (gethash "ANTHROPIC_AUTH_TOKEN" env))
        (setenv "ANTHROPIC_BASE_URL" (gethash "ANTHROPIC_BASE_URL" env))
        (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL" (gethash "ANTHROPIC_DEFAULT_SONNET_MODEL" env))))))

(use-package! acp
  :after shell-maker
  :config
  ;; Read from environment variables (loaded from Claude Code settings)
  (setq acp-api-key (getenv "ANTHROPIC_AUTH_TOKEN"))
  (setq acp-anthropic-api-url (getenv "ANTHROPIC_BASE_URL")))

(use-package! agent-shell
  :after (shell-maker acp)
  :commands (agent-shell agent-shell-send-region)
  :config
  ;; Use the Claude model from environment or default
  (setq agent-shell-claude-model
        (or (getenv "ANTHROPIC_DEFAULT_SONNET_MODEL")
            "vertex_ai/claude-sonnet-4-5")))

;; Open agent-shell in a bottom side window (half height)
(set-popup-rule! "^\\*agent-shell\\*" :side 'bottom :size 0.4 :select t :quit nil)

;; Agent shell — no keybinding to avoid which-key conflicts
;; Use M-x agent-shell and M-x agent-shell-send-region
