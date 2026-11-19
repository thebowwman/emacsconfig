;;; init.el --- entry point -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-packages)
(require 'init-editor)
(require 'init-ui)
(require 'init-completion)
(require 'init-keys)
(require 'init-project)
(require 'init-debug)
(require 'init-lsp)
(require 'init-git)
(require 'init-snippets)
(require 'init-ai)
(require 'init-rust)
(require 'init-python)

;; Keep Emacs-written settings out of this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
