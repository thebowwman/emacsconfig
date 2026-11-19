;;; init-debug.el --- debugging with dape -*- lexical-binding: t; -*-

(require 'init-keys)

(use-package dape)

(my/leader
  "d"  '(:ignore t :which-key "debug")
  "dd" '(dape :which-key "start debugging")
  "db" '(dape-breakpoint-toggle :which-key "toggle breakpoint")
  "dc" '(dape-continue :which-key "continue")
  "dn" '(dape-next :which-key "step over")
  "di" '(dape-step-in :which-key "step into")
  "do" '(dape-step-out :which-key "step out")
  "dq" '(dape-quit :which-key "stop debugging"))

(provide 'init-debug)
;;; init-debug.el ends here
