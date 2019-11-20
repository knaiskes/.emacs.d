(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)

(setq x-select-enable-cliboard t)

(setq frame-title-format "emacs")

(load-theme 'wombat)

;; do not beep
(setq ring-bell-function(lambda()))

;; cursor stops blinking
(when(functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; highlight current line
(global-hl-line-mode t)

(set-default-font "Sans 18")

(require 'ido)
(ido-mode t)

(global-linum-mode t)

;; store all backup files in a single directory
(setq backup-directory-alist `(("." . "~/.saves")))
