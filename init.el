(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq bookmark-default-file "~/.emacs.d/emacs-bookmarks/bookmarks")

(require 'core)
(require 'appearance)
(require 'completion)
(require 'langs)
(require 'tools)
