(setq custom-file (make-temp-file "emacs-custom"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default truncate-lines t)
(setq read-file-name-completion-ignore-case t)
(setq isearch-lazy-count t)
(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(electric-pair-mode 1)
(global-auto-revert-mode t)

(setq explicit-shell-file-name "/bin/bash"
      tramp-histfile-override nil)

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying t
      version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)

;;; Kill confirmation for graphical sessions
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(setq x-select-enable-clipboard t)

(use-package winner
  :ensure nil
  :config
  (winner-mode 1)
  :bind (("<C-s-right>" . winner-redo)
         ("<C-s-left>"  . winner-undo)))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(provide 'core)
