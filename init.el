;; init packages
(require 'package)

;;(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))


;; Set my fullname and email address
(setq user-full-name "Kiriakos Naiskes"
      user-mail-address "kiriakosnaiskes@gmail.com")

;; Configure the startup screen and remove helper bars
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)

(setq x-select-enable-cliboard t)

(set-frame-font "Ubuntu Mono-18")

;; Config for the modus themes
(setq modus-themes-mode-line '(accented borderless padded))
(setq modus-themes-region '(bg-only))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense underline))
(setq modus-themes-syntax '(alt-syntax))

;; Stop wrapping long lines in display
(setq-default truncate-lines t)

;; Stop cursor from blinking
(when(functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq pixel-scroll-precision-mode 1)

;; Delete selected region on typing
(delete-selection-mode t)

;; Ignore case in file name completion
(setq read-file-name-completion-ignore-case t)

;; y or n should suffice
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show relative line number
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Don't create lock files as it breaks React's development server
(setq create-lockfiles nil)

;; Enable pair mode
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; Save all the backup files to .emacs.d/backups directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

;; Use UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Confirm when killing graphical session
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; Disable custom comments
(setq custom-file (make-temp-file "emacs-custom"))
(setq-default custom-file nil)

;; Enable search counter
(setq isearch-lazy-count t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(display-line-numbers-type 'relative)
 '(package-selected-packages
   '(python-mode yaml-mode web-mode typescript-mode terraform-mode rjsx-mode markdown-mode magit json-mode go-mode dockerfile-mode company)))

;; Define the package configurations
(use-package rjsx-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package json-mode :ensure t)
(use-package magit :ensure t)
(use-package typescript-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package web-mode :ensure t)
(use-package go-mode :ensure t)
(use-package python-mode :ensure t)


;; Magit config
(use-package magit
  :ensure t
  :commands magit-status)

;; Eglot
(use-package eglot
  :ensure t
  :hook ((js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
	 (go-mode . eglot-ensure)))

;; JavaScript and TypeScript modes
(use-package js
  :mode ("\\.js\\'" . js-mode)
  :init
  (add-to-list 'interpreter-mode-alist '("node" . js-mode))
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

;; C++ mode for .ino files
(use-package cc-mode
  :mode ("\\.ino\\'" . c++-mode))

;; Company mode
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Set cursor color
(set-cursor-color "#F35336")

;; Auto refresh buffers on change
(global-auto-revert-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
