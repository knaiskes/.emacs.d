;; init packages

(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)

(setq x-select-enable-cliboard t)

(setq frame-title-format "emacs")

(setq-default truncate-lines t) ;; stop wrapping long lines in display

;; do not beep
(setq ring-bell-function(lambda()))

;; cursor stops blinking
(when(functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; highlight current line
;; (global-hl-line-mode t)

(set-default-font "Ubuntu Mono-18")

;; (require 'ido)
;; (ido-mode t)

;; (global-linum-mode t)

;;; yasnippet
(require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; store all backup files in a single directory
;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Make kill and yank work with the X clipboaard
(setq x-select-enable-clipboard t)

(setq user-full-name "Kiriakos Naiskes"
      user-mail-address "kiriakosnaiskes@gmail.com")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(display-line-numbers-type (quote relative))
 '(package-selected-packages
   (quote
    (scala-mode yaml-mode yasnippet haskell-mode js2-mode markdown-mode web-mode php-mode go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


 (require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

;; Confirm when killing only on graphical session
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))


;;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))
