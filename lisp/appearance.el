(setq modus-themes-mode-line    '(accented borderless padded)
      modus-themes-region       '(bg-only)
      modus-themes-bold-constructs   t
      modus-themes-italic-constructs t
      modus-themes-paren-match  '(bold intense underline)
      modus-themes-syntax       '(alt-syntax))

(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :height 200)
(set-cursor-color "#F35336")

(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(use-package display-line-numbers
  :ensure nil                            ; built-in
  :custom (display-line-numbers-type 'relative)
  :config (global-display-line-numbers-mode 1))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(provide 'appearance)
