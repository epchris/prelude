;;; Window settings
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'automatic)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; Themes
(load-theme 'molokai t)

;; Font
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-11"))

(setq-default truncate-lines t)

(setq ns-use-srgb-colorspace t)
(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)


(provide 'epchris-window)
