;;; Personal settings
(require 'compile)
(setq prelude-flyspell nil)

;; To support DASH integration, unset the prelude key bindings we want to use
(define-key prelude-mode-map (kbd "C-c d") nil)
(define-key prelude-mode-map (kbd "C-c e") nil)

;;
;; Platform-specific setups
;;
(cond
 ((eq 'darwin system-type)
  (progn
    (setq btsync_dir "~/BTSync")
    (global-set-key (kbd "C-c d") 'dash-at-point)
    (global-set-key (kbd "C-c e") 'dash-at-point-with-docset)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    )
  )
 ((eq 'gnu/linux system-type)
  (progn
    (setq btsync_dir "~/btsync")
    (global-set-key (kbd "C-c d") 'zeal-at-point)
    (global-set-key (kbd "C-c e") 'dash-at-point-with-docset)
    )
  )
 )

;;
;; HTML/Javascript/Web mode things
;;
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;
;; ROBE integration
;;
(global-set-key (kbd "C-c r j") 'robe-jump)
(global-set-key (kbd "C-c r a") 'robe-ask)

;;
;; Project Explorer
;;
(global-set-key [f8] 'project-explorer-toggle)

;;
;; Org Capture
;;
(global-set-key (kbd "C-c c") 'org-capture)

;;
;; Line numbers and indentation
;;
(global-linum-mode t)
(setq linum-format "%d ")
;; Use spaces for tabs
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq json-reformat:indent-width 2)

;;
;; Kill without confirmation
;;
(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key "\C-xk" 'kill-current-buffer)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key "\M-/" 'comment-or-uncomment-region-or-line)

;;
;; XML Formatting method
;;
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
    ))

;;
;; Iedit is for multiple-occurrence editing, similar to
;; multiple-cur
;;
(require 'iedit)

;;
;; Projectile settings
;;
(persp-mode)
(require 'persp-projectile)
(setq projectile-switch-project-action 'projectile-dired)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;;
;; Ruby settings
;;
(defun my-ruby-mode-hook ()
  (setq standard-indent 2)
  (linum-on)
  (subword-mode +1)
  (local-set-key (kbd "C-M-f") 'ruby-forward-sexp)
  (local-set-key (kbd "C-M-b") 'ruby-backward-sexp)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq-local imenu-create-index-function 'ruby-imenu-create-index)
  (setq dash-at-point-docset "ruby"))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(push 'company-robe company-backends)
(add-hook 'robe-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
(setq ruby-deep-indent-paren nil)

;;
;; JS/Coffee mode
;;
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-process-output nil)))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;;
;; YAML Mode
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)))

;; GPG Things?
(require 'epa-file)
(epa-file-enable)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'origami)
(define-key origami-mode-map (kbd "C-c C-o o") 'origami-open-node)
(define-key origami-mode-map (kbd "C-c C-o c") 'origami-close-node)
(define-key origami-mode-map (kbd "C-c C-o t") 'origami-recursively-toggle-node)
(define-key origami-mode-map (kbd "C-c C-o r") 'origami-reset)

(provide 'epchris-settings)
;;;
