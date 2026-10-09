;; -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (defvar my/magit-protected-branches '("main" "master")
    "Branches that require confirmation before pushing.")

  (defun my/magit-confirm-push-to-main (orig-fun &rest args)
    "Ask for confirmation before pushing from a protected branch."
    (let ((branch (magit-get-current-branch)))
      (if (or (not (member branch my/magit-protected-branches))
              (yes-or-no-p (format "Pushing to %s. Continue? " branch)))
          (apply orig-fun args)
        (user-error "Push aborted"))))

  (dolist (fn '(magit-push-current-to-pushremote
                magit-push-current-to-upstream
                magit-push-current
                magit-push-other))
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
