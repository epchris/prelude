;;; Org-mode settings
(cond
 ((eq 'darwin system-type)
  (progn (setq btsync_dir "~/BTSync"))
  )
 ((eq 'gnu/linux system-type)
  (progn (setq btsync_dir "~/btsync"))
  )
 )
(require 'org)
(setq org-directory (concat btsync_dir "/OrgMode"))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "ASSIGNED(a)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
(setq org-use-fast-todo-selection t)
(setq org-default-notes-file (concat org-directory "notes.org"))
(add-to-list 'org-agenda-files org-directory)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/tasks.org") "Tasks")
         "* TODO %?\n%U\n%a\n")
        ("j" "Journal" entry (file+datetree (concat org-directory "/notes.org"))
         "* %?\nEntered on %U\n%i\n%a")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-completion-use-ido t)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key prelude-mode-map (kbd "C-c o") nil)
(global-set-key (kbd "C-c o f") 'org-switchb)

(provide 'epchris-org)
