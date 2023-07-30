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

;; Save all the backup files to .emacs.d/backups directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

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

(custom-set-variables
 '(custom-enabled-themes '(modus-vivendi))
 '(display-line-numbers-type 'relative)
 '(package-selected-packages '(magit)))


;; Magit config
(use-package magit
  :ensure t
  :commands magit-status)
