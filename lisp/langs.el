(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((go         "https://github.com/tree-sitter/tree-sitter-go")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
          (sql        "https://github.com/m-novikov/tree-sitter-sql")))

  ;; Remap legacy modes to their tree-sitter equivalents
  (dolist (mapping '((js-mode          . js-ts-mode)
                     (javascript-mode  . js-ts-mode)
                     (typescript-mode  . typescript-ts-mode)
                     (js-json-mode     . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  (defun my/treesit-install-all-languages ()
    "Install all grammars listed in `treesit-language-source-alist'."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (message "Installing `%s' parser..." lang)
      (treesit-install-language-grammar lang)
      (sit-for 0.75))
    (message "All tree-sitter parsers installed.")))

(use-package js-ts-mode
  :ensure nil
  :mode "\\.js\\'"
  :hook ((js-ts-mode . eglot-ensure)
         (js-ts-mode . company-mode))
  :custom (js-indent-level 2))

(use-package js-mode
  :ensure nil
  :mode "\\.mjs\\'")

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure)
         (typescript-ts-mode . company-mode)))

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'"
  :hook ((tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . company-mode)))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :custom (js-indent-level 2))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook ((go-ts-mode . company-mode)
         (go-ts-mode . (lambda ()
                         (setq-local tab-width 8)
                         (add-hook 'before-save-hook #'eglot-format-buffer nil t)
                         (when (locate-dominating-file default-directory "go.mod")
                           (eglot-ensure))))))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . company-mode)))

(use-package cc-mode
  :ensure nil
  :mode ("\\.ino\\'" . c++-mode))

(provide 'langs)
