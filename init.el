;; init packages

(require 'package)
;;(package-initialize)

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

(set-frame-font "Ubuntu Mono-18")

;; (require 'ido)
;; (ido-mode t)

;; (global-linum-mode t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Delete selected region on typing
(delete-selection-mode t)

;; Ignore case in file name completion
(setq read-file-name-completion-ignore-case t)

;; y or n should suffice
(defalias 'yes-or-no-p 'y-or-n-p)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; store all backup files in a single directory
;;(setq backup-directory-alist `(("." . "~/.saves")))

;; Don't create lock files as it breaks React's development server
(setq create-lockfiles nil)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Make kill and yank work with the X clipboaard
(setq x-select-enable-clipboard t)

(setq user-full-name "Kiriakos Naiskes"
      user-mail-address "kiriakosnaiskes@gmail.com")

;; Config for the modus themes
(setq modus-themes-mode-line '(accented borderless padded))
(setq modus-themes-region '(bg-only))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense underline))
;(setq modus-themes-syntax '(faint))
(setq modus-themes-syntax '(alt-syntax))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(display-line-numbers-type 'relative)
 '(package-selected-packages
   '(rust-mode lsp-mode company json-mode magit flycheck typescript-mode dockerfile-mode yaml-mode yasnippet markdown-mode web-mode go-mode)))

;; Set cursor color
(set-cursor-color "#F35336")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

 (require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

;; Confirm when killing only on graphical session
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;;; yasnippet
(require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;;; js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

;;JSX
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . js-jsx-mode))


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;;tsx
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'after-init-hook #'global-flycheck-mode)


;; Enable company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; lsp-mode
(require 'lsp-mode)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(setq lsp-keymap-prefix "C-c l")
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-auto-revert-mode t) ;; auto refresh buffers
