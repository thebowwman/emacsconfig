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
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  ;; Also initialize in terminal
  (unless (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Ensure npm global bin is in PATH
(add-to-list 'exec-path (expand-file-name "~/.npm-global/bin"))

;; Ensure Go tools are in PATH
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

;; Ensure Cargo/Rust tools are in PATH
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setenv "PATH" (concat (expand-file-name "~/.cargo/bin") ":" (getenv "PATH")))

(use-package! pyvenv
  :config
  (pyvenv-mode 1)
  ;; Do NOT enable pyvenv-tracking-mode globally — it bleeds venv display
  ;; into non-Python buffers (Rust, Go, etc.). Per-hook activation is enough.
  ;; (pyvenv-tracking-mode 1)

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

  ;; Auto-activate .venv when opening Python files
  (add-hook 'python-mode-hook #'my/auto-activate-venv)

  ;; Auto-activate .venv when switching projects (Projectile)
  ;; Only triggers when actually switching, not on every buffer change
  (add-hook 'projectile-after-switch-project-hook #'my/auto-activate-venv)

  ;; Removed: emacs-startup-hook — caused venv to activate based on launch
  ;; directory rather than actual Python context, polluting all buffers.
  )

(setq lsp-pyright-python-executable-cmd "python")
(setq python-shell-interpreter "python")

;; -------------------------------
;; Pyright auto-import support
;; -------------------------------
(setq lsp-disabled-clients '(pylsp))   ;; disable pylsp

(add-hook 'python-mode-hook #'lsp!)     ;; force start LSP with pyright

(after! lsp-pyright
  ;; Enable auto-import suggestions in completion popups
  (setq lsp-pyright-auto-import-completions t)

  ;; Use library code for better type information
  (setq lsp-pyright-use-library-code-for-types t))

;; Optional: auto-organize imports when saving
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; -------------------------------
;; LSP Configuration (consolidated)
;; -------------------------------
(after! lsp-mode
  ;; Session Management
  (setq lsp-session-file nil)  ; Don't persist workspace folders
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 500)
  (setq lsp-auto-guess-root t)
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

  ;; Auto-import completions
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t)

  ;; Cargo features awareness
  (setq lsp-rust-analyzer-cargo-all-features t)

  ;; Use library code for better type information
  (setq lsp-rust-analyzer-use-client-watching t)

  ;; Enable experimental features
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

;; Auto-format on save using rustfmt
(add-hook 'rustic-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; -------------------------------
;; Rust Clippy / Flycheck Integration
;; -------------------------------
(after! rustic
  ;; Use clippy for flycheck
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
;; DAP (Debug Adapter Protocol)
;; -------------------------------

(use-package! dap-mode
  :config
  (setq dap-print-io t)
  (setq dap-auto-configure-mode t)
  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions repl controls tooltip))

  ;; Load codelldb FIRST before other adapters
  (require 'dap-codelldb)
  (setq dap-codelldb-debug-path
        (expand-file-name "~/.emacs.d/.local/cache/.extension/vscode/codelldb/extension"))

  ;; Other adapters
  (require 'dap-python)
  (require 'dap-dlv-go)
  (require 'dap-js)

  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3")

  (unless (and (boundp 'dap-js-path)
               (file-exists-p (expand-file-name "src/dapDebugServer.js" dap-js-path)))
    (dap-js-setup))

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-ui-locals)))

  ;; Python debug template
  (dap-register-debug-template "Python :: Run Current File"
    (list :type "python"
          :args ""
          :cwd nil
          :program nil
          :request "launch"
          :name "Python :: Run Current File"))

  ;; Go debug template
  (dap-register-debug-template "Go :: Run Current File"
    (list :type "go"
          :request "launch"
          :name "Go :: Run Current File"
          :mode "debug"
          :program nil
          :args nil))

  ;; Node.js / TypeScript debug templates
  (dap-register-debug-template "Node :: Attach (JS/TS)"
    (list :type "pwa-node"
          :request "attach"
          :name "Node :: Attach (JS/TS)"
          :address "localhost"
          :port 9229
          :restart t
          :sourceMaps t
          :skipFiles ["<node_internals>/**" "**/node_modules/**"]
          :resolveSourceMapLocations ["${workspaceFolder}/**" "!**/node_modules/**"]))

  (dap-register-debug-template "Node :: Launch TS File (ts-node)"
    (list :type "pwa-node"
          :request "launch"
          :name "Node :: Launch TS File (ts-node)"
          :program nil
          :cwd nil
          :address "localhost"
          :runtimeExecutable "node"
          :runtimeArgs ["--require" "ts-node/register" "--enable-source-maps"]
          :skipFiles ["<node_internals>/**" "**/node_modules/**"]
          :sourceMaps t
          :resolveSourceMapLocations ["${workspaceFolder}/**" "!**/node_modules/**"]))

  ;; Rust debug function — prompts for template then binary
  (defun my/dap-rust-debug ()
    "Launch Rust debugger with template selection."
    (interactive)
    (let* ((root (projectile-project-root))
           (template (completing-read "Template: "
                                      '("Rust :: Run Binary"
                                        "Rust :: Run Test")
                                      nil t)))
      (if (string= template "Rust :: Run Test")
          (progn
            (message "Compiling tests...")
            (shell-command-to-string
             (concat "cd " root " && cargo test --no-run 2>&1"))
            (let* ((deps-dir (concat root "target/debug/deps/"))
                   (test-binaries (seq-filter
                                   (lambda (f)
                                     (and (file-executable-p (concat deps-dir f))
                                          (not (string-match-p "\\." f))))
                                   (directory-files deps-dir nil nil t)))
                   (binary (completing-read "Test binary: " test-binaries nil t)))
              (dap-debug
               (list :type "lldb"
                     :request "launch"
                     :name template
                     :program (concat deps-dir binary)
                     :cwd root
                     :args (list "--nocapture")))))
        (progn
          (message "Building...")
          (shell-command-to-string
           (concat "cd " root " && cargo build 2>&1"))
          (dap-debug
           (list :type "lldb"
                 :request "launch"
                 :name template
                 :program (read-file-name "Binary: " (concat root "target/debug/"))
                 :cwd root))))))

  (map! :after dap-mode
        :map rustic-mode-map
        :localleader
        :desc "Debug Rust" "D" #'my/dap-rust-debug))


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

;; Auto-load dap-mode for programming modes
(add-hook 'go-mode-hook #'dap-mode)
(add-hook 'python-mode-hook #'dap-mode)
(add-hook 'typescript-mode-hook #'dap-mode)
(add-hook 'tsx-ts-mode-hook #'dap-mode)
(add-hook 'typescript-ts-mode-hook #'dap-mode)
(add-hook 'js-mode-hook #'dap-mode)
(add-hook 'rustic-mode-hook #'dap-mode)

;; DAP keybindings
(map! :after dap-mode
      :map dap-mode-map
      :localleader
      (:prefix ("d" . "debug")
       :desc "Start debugging" "d" #'dap-debug
       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
       :desc "Conditional breakpoint" "c" #'dap-breakpoint-condition
       :desc "Delete breakpoint" "x" #'dap-breakpoint-delete
       :desc "List breakpoints" "l" #'dap-breakpoint-list
       :desc "Continue" "r" #'dap-continue
       :desc "Next" "n" #'dap-next
       :desc "Step in" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Quit" "q" #'dap-disconnect
       :desc "Eval at point" "e" #'dap-eval-thing-at-point
       :desc "View locals" "v" #'dap-ui-locals))


(use-package! repeat
  :custom
  (repeat-mode +1))

;; Left and right side windows occupy full frame height
(use-package! emacs
  :custom
  (window-sides-vertical t))


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
(use-package! yasnippet
  :config
  (yas-global-mode 1))

;; Enhanced React/Next.js snippets for TypeScript
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
                        "Hono API route")))

;; Optional: better syntax highlighting via tree-sitter
(after! tree-sitter
  (add-hook 'typescript-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-ts-mode-hook #'tree-sitter-mode)
  (add-hook 'tsx-ts-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'typescript-ts-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'tsx-ts-mode-hook #'tree-sitter-hl-mode)
  ;; Rust tree-sitter
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
  ;; Set the directory for native compilation cache
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" user-emacs-directory)))

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

;; Add npm global bin to exec-path so Emacs can find claude-agent-acp
(add-to-list 'exec-path (expand-file-name "~/.npm-global/bin"))

(use-package! agent-shell
  :after (shell-maker acp)
  :commands (agent-shell agent-shell-send-region)
  :config
  ;; Use the Claude model from environment or default
  (setq agent-shell-claude-model
        (or (getenv "ANTHROPIC_DEFAULT_SONNET_MODEL")
            "vertex_ai/claude-sonnet-4-5")))

;; Set up keybindings for agent-shell
(map! :leader
      :prefix ("g" . "agent-shell")
      :desc "Start agent shell" "s" #'agent-shell
      :desc "Agent shell send region" "r" #'agent-shell-send-region)
