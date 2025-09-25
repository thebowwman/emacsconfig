;;; marginalina.el -*- lexical-binding: t; -*-

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
