;;; init-keys.el --- Evil, leader key, and bindings -*- lexical-binding: t; -*-

(use-package which-key
  :init
  (which-key-mode 1))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  :init
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer my/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (my/leader
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "."  '(find-file :which-key "find file here")
    "pg" '(project-find-regexp :which-key "grep")
    "pn" '(next-buffer :which-key "next buffer")
    "pN" '(previous-buffer :which-key "previous buffer")
    "ps" '(project-shell :which-key "project shell")
    "t"  '(:ignore t :which-key "toggle")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bi" '(ibuffer :which-key "ibuffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "bx" '(kill-buffer-and-window :which-key "kill buffer and window")
    "br" '(revert-buffer :which-key "revert buffer")
    "bs" '(save-buffer :which-key "save buffer")
    "bS" '(save-some-buffers :which-key "save all buffers")
    "bz" '(bury-buffer :which-key "bury buffer")
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search in buffer")
    "sp" '(consult-ripgrep :which-key "search in project")
    "w"  '(:ignore t :which-key "windows")
    "wh" '(evil-window-left :which-key "window left")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wl" '(evil-window-right :which-key "window right")
    "ww" '(other-window :which-key "other window")
    "ws" '(evil-window-split :which-key "split horizontal")
    "wv" '(evil-window-vsplit :which-key "split vertical")
    "wd" '(evil-window-delete :which-key "delete window")
    "wm" '(delete-other-windows :which-key "maximize window")
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open :which-key "recent files")))


(use-package avy)

(my/leader
  "j"  '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char-timer :which-key "jump to text")
  "jl" '(avy-goto-line :which-key "jump to line"))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(provide 'init-keys)
;;; init-keys.el ends here
