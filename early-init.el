(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-deferred-compilation t))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      )
