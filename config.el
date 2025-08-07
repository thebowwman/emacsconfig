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




(defvar my/pyvenv--debounce-timer nil)

(defun my/python-buffers ()
  "Real Python buffers (files or REPL), ignore internals/fontifiers."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (and (or (derived-mode-p 'python-mode 'inferior-python-mode))
            (not (string-prefix-p " " (buffer-name buf))) ; internal buffers
            (not (string-match-p "^ \\*markdown-code-fontification:" (buffer-name buf)))
            (or (buffer-file-name)
                (eq major-mode 'inferior-python-mode)))))
   (buffer-list)))

(defun my/auto-deactivate-pyvenv--coalesced ()
  "Deactivate pyvenv when there are no Python buffers."
  (when my/pyvenv--debounce-timer
    (cancel-timer my/pyvenv--debounce-timer))
  (setq my/pyvenv--debounce-timer
        (run-at-time
         0.2 nil
         (lambda ()
           (let ((pybufs (my/python-buffers)))
             (message "[pyvenv-debug] remaining python buffers: %S"
                      (mapcar #'buffer-name pybufs))
             (when (and (null pybufs) pyvenv-virtual-env)
               (message "[pyvenv] No Python buffers left. Deactivating venv.")
               (pyvenv-deactivate)
               (force-mode-line-update t)))))))

(add-hook 'kill-buffer-hook #'my/auto-deactivate-pyvenv--coalesced)

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


(defun my/debug-python-config ()
  (interactive)
  (message "python-shell-interpreter: %s" python-shell-interpreter)
  (message "lsp-pyright-python-executable-cmd: %s" lsp-pyright-python-executable-cmd)
  (message "pyvenv-virtual-env: %s" pyvenv-virtual-env))



;; Modelline setup from @joshblais
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)


;; Blink cursor
(blink-cursor-mode 1)


(global-visual-line-mode t)


;; Send files to trash instead of fully deleting
(setq delete-by-moving-to-trash t)
;; Save automatically
(setq auto-save-default t)

;; Performance optimizations
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 8)

;; Garbage collector optimization
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 1024 1024 1024))

;; Version control optimization
(setq vc-handled-backends '(Git))

;; Fix x11 issues
(setq x-no-window-manager t)
(setq frame-inhibit-implied-resize t)
(setq focus-follows-mouse nil)


;; Speed of which-key popup
(setq which-key-idle-delay 0.2)

;; Completion mechanisms
(setq completing-read-function #'completing-read-default)
(setq read-file-name-function #'read-file-name-default)
;; Makes path completion more like find-file everywhere
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
;; Use the familiar C-x C-f interface for directory completion
(map! :map minibuffer-mode-map
      :when (featurep! :completion vertico)
      "C-x C-f" #'find-file)

;; Save minibuffer history - enables command history in M-x
(use-package! savehist
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (savehist-mode 1))

(after! vertico
  ;; Add file preview
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  ;; Make vertico use a more minimal display
  (setq vertico-count 17
        vertico-cycle t
        vertico-resize t)
  ;; Enable alternative filter methods
  (setq vertico-sort-function #'vertico-sort-history-alpha)
  ;; Quick actions keybindings
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)

  ;; History navigation
  (define-key vertico-map (kbd "M-p") #'vertico-previous-history)
  (define-key vertico-map (kbd "M-n") #'vertico-next-history)
  (define-key vertico-map (kbd "C-r") #'consult-history)

  ;; Configure orderless for better filtering
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion orderless))))

  ;; Customize orderless behavior
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal
                                    orderless-prefixes
                                    orderless-initialism
                                    orderless-regexp)))

;; Quick command repetition
(use-package! vertico-repeat
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :leader
        (:prefix "r"
         :desc "Repeat completion" "v" #'vertico-repeat)))

;; TODO Not currently working
;; Enhanced sorting and filtering with prescient
;; (use-package! vertico-prescient
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode 1)
;;   (prescient-persist-mode 1)
;;   (setq prescient-sort-length-enable nil
;;         prescient-filter-method '(literal regexp initialism fuzzy)))

;; Enhanced marginalia annotations
(after! marginalia
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Show more details in marginalia
  (setq marginalia-max-relative-age 0
        marginalia-align 'right))

;; Corrected Embark configuration
(map! :leader
      (:prefix ("k" . "embark")  ;; Using 'k' prefix instead of 'e' which conflicts with elfeed
       :desc "Embark act" "a" #'embark-act
       :desc "Embark dwim" "d" #'embark-dwim
       :desc "Embark collect" "c" #'embark-collect))

;; Configure consult for better previews
(after! consult
  (setq consult-preview-key "M-."
        consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  ;; More useful previews for different commands
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; Enhanced directory navigation
(use-package! consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

;; Add additional useful shortcuts
(map! :leader
      (:prefix "s"
       :desc "Command history" "h" #'consult-history
       :desc "Recent directories" "d" #'consult-dir))

(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-quick-access t
        company-tooltip-limit 20
        company-tooltip-align-annotations t)

  ;; Make company-files a higher priority backend
  (setq company-backends (cons 'company-files (delete 'company-files company-backends)))

  ;; Better file path completion settings
  (setq company-files-exclusions nil)
  (setq company-files-chop-trailing-slash t)

  ;; Enable completion at point for file paths
  (defun my/enable-path-completion ()
    "Enable file path completion using company."
    (setq-local company-backends
                (cons 'company-files company-backends)))

  ;; Enable for all major modes
  (add-hook 'after-change-major-mode-hook #'my/enable-path-completion)

  ;; Custom file path trigger
  (defun my/looks-like-path-p (input)
    "Check if INPUT looks like a file path."
    (or (string-match-p "^/" input)         ;; Absolute path
        (string-match-p "^~/" input)        ;; Home directory
        (string-match-p "^\\.\\{1,2\\}/" input))) ;; Relative path

  (defun my/company-path-trigger (command &optional arg &rest ignored)
    "Company backend that triggers file completion for path-like input."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-files))
      (prefix (when (my/looks-like-path-p (or (company-grab-line "\\([^ ]*\\)" 1) ""))
                (company-files 'prefix)))
      (t (apply 'company-files command arg ignored))))

  ;; Add the custom path trigger to backends
  (add-to-list 'company-backends 'my/company-path-trigger))



;; org mode setup

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(use-package org
  :ensure nil
  :custom (org-modules '(org-habit)))

(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote)
  )

;; Auto-clock in when state changes to STRT
(defun my/org-clock-in-if-starting ()
  "Clock in when the task state changes to STRT"
  (when (and (string= org-state "STRT")
             (not (org-clock-is-active)))
    (org-clock-in)))


(after! org
  ;; Make sure the daily dir is on the agenda
  (add-to-list 'org-agenda-files "~/org/daily/")

  ;; Logseq-style daily note capture: one file per day with a title, add a TODO
  (setq org-capture-templates
        '(("d" "Daily TODO" entry
           (file+head "~/org/daily/%<%Y-%m-%d>.org"
                      "#+title: %<%B %e, %Y>\n")
           "* TODO %?\n"
           :empty-lines 1))))
