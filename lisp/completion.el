(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1))

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)))

(use-package icomplete
  :ensure nil
  :init
  (fido-vertical-mode 1)
  :config
  (setq icomplete-delay-completions-threshold 0
	icomplete-max-delay-chars 0
	icomplete-scrol t))

(provide 'completion)
