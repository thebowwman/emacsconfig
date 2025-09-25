;;; org.el -*- lexical-binding: t; -*-


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
;; Auto-clock out when leaving STRT state
(defun my/org-clock-out-if-not-starting ()
  "Clock out when leaving STRT state"
  (when (and (org-clock-is-active)
             (not (string= org-state "STRT")))
    (org-clock-out)))

;; Add these functions to org-after-todo-state-change-hook
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-in-if-starting)
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-out-if-not-starting)

;; Show habits in agenda
(setq org-habit-show-all-today t)
(setq org-habit-graph-column 1)
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines 1)))

;; (after! org
;;   (use-package! org-fancy-priorities
;;     :hook
;;     (org-mode . org-fancy-priorities-mode)
;;     :config
;;     (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "FUTURE"))))

;; Prevent clock from stopping when marking subtasks as done
(setq org-clock-out-when-done nil)


;; Org Agenda
;; Set days viewed to 3, set start day to today, create seperator, and Dashboard view
(setq org-agenda-remove-tags t)
(setq org-agenda-block-separator 32)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "\n HIGHEST PRIORITY")
                 (org-agenda-prefix-format "   %i %?-2 t%s")
                 )
                )
          (agenda ""
                  (
                   (org-agenda-start-day "+0d")
                   (org-agenda-span 1)
                   (org-agenda-time)
                   (org-agenda-remove-tags t)
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈ NOW")
                   (org-agenda-overriding-header "\n TODAY'S SCHEDULE")
                   (org-agenda-prefix-format "   %i %?-2 t%s")
                   )
                  )
          (tags-todo  "-STYLE=\"habit\""
                      (
                       (org-agenda-overriding-header "\n ALL TODO")
                       (org-agenda-sorting-strategy '(priority-down))
                       (org-agenda-remove-tags t)
                       (org-agenda-prefix-format "   %i %?-2 t%s")
                       )
                      )))))

;; Remove Scheduled tag
(setq org-agenda-scheduled-leaders '("" ""))
;; Remove holidays from agenda
(setq org-agenda-include-diary nil)


;; =====================================================================================
(defun my/org-capture-daily-file ()
  "Generate the daily capture file path in ~/org/daily/."
  (expand-file-name (format "~/org/daily/%s.org"
                            (format-time-string "%Y-%m-%d"))))
(after! org
  ;; Make sure the daily dir is on the agenda
  (add-to-list 'org-agenda-files "~/org/daily/")

  ;; Function to generate the file path dynamically
  (defun capture-report-date-file (path)
    "Generate a dynamic file path based on today's date."
    (expand-file-name (format "%s.org" (format-time-string "%Y-%m-%d")) path))

  (set-locale-environment "en_US.UTF-8")


  ;; Set the dynamic file path for today’s capture
  (setq my/daily-file (capture-report-date-file "~/org/daily/"))

  ;; Logseq-style daily note capture: one file per day with a title, add a TODO
  (setq org-capture-templates
        '(("d" "Daily TODO" entry
           (file my/daily-file)  ;; Use the dynamically evaluated file path
           "* TODO %?\n#+title: %<%B %e, %Y>\n"
           :empty-lines 1))))
;;=========================================================================================
;;
;;
;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/contacts.org\")\n:END:\n%?")

        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("p" "Project" entry
         (file+headline "~/org/projects.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")

        ("i" "Idea" entry
         (file+headline "~/org/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("b" "Bookmark" entry
         (file+headline "~/org/bookmarks.org" "Inbox")
         "** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:CREATED: %U\n:TAGS: %(org-capture-bookmark-tags)\n:END:\n\n"
         :empty-lines 0)

        ("c" "Contact" entry
         (file+headline "~/org/contacts.org" "Inbox")
         "* %^{Name}

:PROPERTIES:
:CREATED: %U
:CAPTURED: %a
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:BIRTHDAY: %^{Birthday +1y}u
:LOCATION: %^{Address}
:LAST_CONTACTED: %U
:END:
\\ *** Communications
\\ *** Notes
%?")

        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))

(defun org-capture-bookmark-tags ()
  "Get tags from existing bookmarks and prompt for tags with completion."
  (save-window-excursion
    (let ((tags-list '()))
      ;; Collect existing tags
      (with-current-buffer (find-file-noselect "~/org/bookmarks.org")
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:TAGS:\\s-*\\(.+\\)$" nil t)
            (let ((tag-string (match-string 1)))
              (dolist (tag (split-string tag-string "[,;]" t "[[:space:]]"))
                (push (string-trim tag) tags-list))))))
      ;; Remove duplicates and sort
      (setq tags-list (sort (delete-dups tags-list) 'string<))
      ;; Prompt user with completion
      (let ((selected-tags (completing-read-multiple "Tags (comma-separated): " tags-list)))
        ;; Return as a comma-separated string
        (mapconcat 'identity selected-tags ", ")))))

;; Helper function to select and link a contact
(defun org-capture-ref-link (file)
  "Create a link to a contact in contacts.org"
  (let* ((headlines (org-map-entries
                     (lambda ()
                       (cons (org-get-heading t t t t)
                             (org-id-get-create)))
                     t
                     (list file)))
         (contact (completing-read "Contact: "
                                   (mapcar #'car headlines)))
         (id (cdr (assoc contact headlines))))
    (format "[[id:%s][%s]]" id contact)))

;; Set archive location to done.org under current date
;; (defun my/archive-done-task ()
;;   "Archive current task to done.org under today's date"
;;   (interactive)
;;   (let* ((date-header (format-time-string "%Y-%m-%d %A"))
;;          (archive-file (expand-file-name "~/org/done.org"))
;;          (location (format "%s::* %s" archive-file date-header)))
;;     ;; Only archive if not a habit
;;     (unless (org-is-habit-p)
;;       ;; Add COMPLETED property if it doesn't exist
;;       (org-set-property "COMPLETED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
;;       ;; Set archive location and archive
;;       (setq org-archive-location location)
;;       (org-archive-subtree))))

;; Automatically archive when marked DONE, except for habits
;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (when (and (string= org-state "DONE")
;;                        (not (org-is-habit-p)))
;;               (my/archive-done-task))))

;; Optional key binding if you ever need to archive manually
(define-key org-mode-map (kbd "C-c C-x C-a") 'my/archive-done-task)
