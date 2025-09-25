;;; evil-mode.el -*- lexical-binding: t; -*-


;; Evil-escape sequence
(setq-default evil-escape-delay 0.1)
(setq evil-move-cursor-back nil)
;; granular undo with evil mode
(setq evil-want-fine-undo t)
;; Enable paste from system clipboard with C-v in insert mode
(evil-define-key 'insert global-map (kbd "C-v") 'clipboard-yank)
