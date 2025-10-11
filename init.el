;; Package sources
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

;; Set font
;;(set-frame-font "Ubuntu Mono-18")

;; Stop cursor from blinking
(when(functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; Stop wrapping long lines in display
(setq-default truncate-lines t)

;; Config for the modus themes
(setq modus-themes-mode-line '(accented borderless padded))
(setq modus-themes-region '(bg-only))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense underline))
(setq modus-themes-syntax '(alt-syntax))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(display-line-numbers-type 'relative)
 '(js-indent-level 2)
 '(package-selected-packages
   '(terraform-mode yaml-mode markdown-mode company magit json-mode)))

;; Set cursor color
(set-cursor-color "#F35336")

;; Enable cliboard for cutting and pasting
(setq x-select-enable-cliboard t)

;; Ignore case in file name completion
(setq read-file-name-completion-ignore-case t)

;; Delete selected region on typing
(delete-selection-mode t)

;; y or n should suffice
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable pair mode
(electric-pair-mode 1)
;;(setq electric-pair-preserve-balance nil)

;; Disable custom comments
(setq custom-file (make-temp-file "emacs-custom"))
(setq-default custom-file nil)

;; Enable search counter
(setq isearch-lazy-count t)

;; Show line number
(global-display-line-numbers-mode)

;; Save all the backup files to .emacs.d/backups directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

;; Confirm when killing graphical session
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; Use UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Disable custom comments
(setq custom-file (make-temp-file "emacs-custom"))
(setq-default custom-file nil)

;; Auto refresh buffers on change
(global-auto-revert-mode t)

;; Enable winner-mode
(winner-mode 1)

(global-set-key (kbd "<C-s-right>") 'winner-redo)
(global-set-key (kbd "<C-s-left>") 'winner-undo)


;; Settings for treesit
(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
	'((go          "https://github.com/tree-sitter/tree-sitter-go")
          (gomod       "https://github.com/camdencheek/tree-sitter-go-mod")
          (javascript  "https://github.com/tree-sitter/tree-sitter-javascript")
          (json        "https://github.com/tree-sitter/tree-sitter-json")
          (python      "https://github.com/tree-sitter/tree-sitter-python")
          (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
          (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")
          (sql         "https://github.com/m-novikov/tree-sitter-sql")))

  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

  (dolist  (mapping '((js-mode . js-ts-mode)
		      (javascript-mode . js-ts-mode)
		      (typescript-mode . tsx-ts-mode)
		      (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

;; Use C++ mode for .ino files
(use-package cc-mode
  :mode ("\\.ino\\'" . c++-mode))

;; Configure JSON mode
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (setq js-indent-level 2))

;; Magit config
(use-package magit
  :ensure t
  :hook (git-commit-mode . company-mode)
  :commands magit-status
  :config
  (setq magit-diff-refine-hunk (quote all)))

;; Company mode config
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

;; JavaScript mode (treesitter)
(use-package javascript-ts-mode
  :hook ((js-ts-mode . eglot-ensure)
	 (js-ts-mode . company-mode))
  :mode (("\\.js\\'" . js-ts-mode))
  :config
  (setq-default js-indent-level 2))

(use-package typescript-ts-mode
  :hook ((typescript-ts-mode . eglot-ensure)
	 (typescript-ts-mode . company-mode))
  :mode (("\\.ts\\'" . typescript-ts-mode)))

;; TSX mode (treesitter)
;; Configure JSON mode
(use-package json-mode
  :ensure t
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook ((tsx-ts-mode . eglot-ensure)
	 (tsx-ts-mode . company-mode))
  :config)

(use-package js-mode
  :mode ("\\.mjs\\'" . js-mode))

;; GoLang mode (treesitter)
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . company-mode)
         (go-ts-mode . (lambda ()
                         (setq-local tab-width 8)  ;; Set correct indentation
                         (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  :mode ("\\.go\\'" . go-ts-mode))

;; Configure Eglot mode
(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc)
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename)))

;; Configure Markdown mode
(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . company-mode))
  :mode ("\\.md\\'" . markdown-mode))

;; Install yaml-mode
(use-package yaml-mode
  :ensure t)

(use-package python
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . company-mode))
  :mode (("\\.py\\'" . python-ts-mode)))

;; Enable pixel scroll
(setq pixel-scroll-precision-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq tramp-histfile-override nil)
(setq explicit-shell-file-name "/bin/bash")

(which-key-mode 1)
