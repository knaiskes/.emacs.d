(use-package magit
  :ensure t
  :commands magit-status
  :hook (git-commit-mode . company-mode)
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (defun my/magit-confirm-push-to-main (orig-fun &rest args)
    "Ask for confirmation before pushing to main or master."
    (let ((branch (magit-get-current-branch)))
      (if (and branch (member branch '("master" "main")))
          (when (yes-or-no-p (format "Pushing to %s. Continue? " branch))
            (apply orig-fun args))
        (apply orig-fun args))))

  (dolist (fn '(magit-push-current-to-pushremote
                magit-push-current
                magit-push))
    (advice-add fn :around #'my/magit-confirm-push-to-main)))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :hook (markdown-mode . company-mode))

(use-package yaml-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(provide 'tools)
