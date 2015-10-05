;;; Window settings
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'automatic)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; Themes
;;(load-theme 'molokai t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(enable-theme 'ample)
;;(setq prelude-theme 'zenburn)
;; Fix current-line highlighting problems
;; (set-face-foreground 'highlight nil)

;; Font
(add-to-list 'default-frame-alist '(font . "Hack-11"))

(setq-default truncate-lines t)

(setq ns-use-srgb-colorspace t)
(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)


(provide 'epchris-window)
