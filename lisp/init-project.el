;;; init-project.el --- projects and bottom terminal -*- lexical-binding: t; -*-

(require 'init-keys)

(setq project-vc-extra-root-markers '("pyproject.toml" "uv.lock")
      project-switch-use-entire-map t
      project-switch-commands '((project-find-file "Find file")
                                (project-find-regexp "Grep")
                                (project-dired "Dired")))

;; Bottom terminal panel: match terminal buffers, whatever they're named
(add-to-list 'display-buffer-alist
             '("terminal"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.3)))

(defun my/toggle-terminal ()
  "Show the terminal at the bottom in the project root, or hide it if visible."
  (interactive)
  (let ((win (seq-some (lambda (w)
                         (and (string-match-p "terminal" (buffer-name (window-buffer w)))
                              w))
                       (window-list)))
        (buf (seq-find (lambda (b) (string-match-p "terminal" (buffer-name b)))
                       (buffer-list))))
    (cond
     (win (delete-window win))
     (buf (display-buffer buf))
     (t
     (let ((default-directory (if-let* ((proj (project-current)))
                                   (project-root proj)
                                 default-directory)))
        (term (or (executable-find "fish") "/bin/bash")))))))

(my/leader
  "to" '(my/toggle-terminal :which-key "terminal"))

(provide 'init-project)
;;; init-project.el ends here
