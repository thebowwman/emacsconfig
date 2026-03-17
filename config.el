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

(use-package! pyvenv
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1)

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
  (add-hook 'projectile-after-switch-project-hook #'my/auto-activate-venv)

  ;; Auto-activate .venv on Emacs startup if in a Python project
  (add-hook 'emacs-startup-hook #'my/auto-activate-venv))

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
;; DAP (Debug Adapter Protocol)
;; -------------------------------
(use-package! dap-mode
  :config
  ;; Enable debug logging
  (setq dap-print-io t)
  (setq dap-auto-configure-mode t)

  ;; Enable UI features
  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions repl controls tooltip))

  ;; Load language-specific adapters
  (require 'dap-python)
  (require 'dap-dlv-go)
  (require 'dap-js)  ; Node.js/TypeScript debugging (modern vscode-js-debug)

  ;; Python configuration
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3")

  ;; FIX: Only install js-debug adapter if not already present
  ;; Previously ran dap-js-setup unconditionally every startup, which
  ;; can cause race conditions and adapter corruption.
  (unless (and (boundp 'dap-js-path)
               (file-exists-p (expand-file-name "src/dapDebugServer.js" dap-js-path)))
    (dap-js-setup))

  ;; Show locals when stopped
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

  ;; ---------------------------------------------------------------
  ;; Node.js / TypeScript debug templates
  ;; FIX: corrected attach template:
  ;;   - added :address "localhost" (required by pwa-node attach)
  ;;   - added :restart t (prevents ECONNRESET on brief disconnects)
  ;;   - fixed :resolveSourceMapLocations to use workspaceFolder glob
  ;;     instead of individual file extension patterns, which is what
  ;;     vscode-js-debug actually expects
  ;; ---------------------------------------------------------------

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

  ;; Launch template using ts-node
  ;; FIX: added :address and corrected :resolveSourceMapLocations
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
          :resolveSourceMapLocations ["${workspaceFolder}/**" "!**/node_modules/**"])))

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
                          (mode . typescript-tsx-mode)
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
(add-hook 'typescript-tsx-mode-hook #'dap-mode)
(add-hook 'js-mode-hook #'dap-mode)

;; DAP keybindings for Python and Go
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
         (typescript-tsx-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

;; Use prettier by default for formatting
(setq-hook! 'typescript-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)

;; Auto-organize imports on save (like Python)
(add-hook 'typescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
(add-hook 'typescript-tsx-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; Enable ESLint for linting
(after! flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode))

;; Company settings are in modules/company.el

;; -------------------------------
;; React & Next.js Snippets (TypeScript)
;; -------------------------------
(use-package! yasnippet
  :config
  (yas-global-mode 1))

;; Enhanced React/Next.js snippets for TypeScript
(yas-define-snippets 'typescript-tsx-mode
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

;; Tree-sitter settings
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))
