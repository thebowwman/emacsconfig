;;; init-ai.el --- LLM chat with gptel -*- lexical-binding: t; -*-

(require 'init-keys)

(use-package gptel
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (lambda ()
                 (auth-source-pick-first-password :host "api.anthropic.com"))
          :models '(claude-sonnet-5-5 claude-haiku-5-5 claude-opus-5-5))
        gptel-model 'claude-sonnet-5-5))

(my/leader
  "a"  '(:ignore t :which-key "ai")
  "ac" '(gptel :which-key "open chat")
  "as" '(gptel-send :which-key "send to model")
  "am" '(gptel-menu :which-key "gptel menu"))

(provide 'init-ai)
;;; init-ai.el ends here
