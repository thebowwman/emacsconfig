;;; vertico.el -*- lexical-binding: t; -*-


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
