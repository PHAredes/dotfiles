;;; modules/tools/packages.el -*- lexical-binding: t; -*-

(use-package magit)
(use-package eat
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :hook (eshell-mode . eat-eshell-mode))

(use-package igist
  :custom (igist-auth-marker 'igist))

(use-package 0x0
  :config (defalias 'upload-0x0 'ee-0x0-upload-region))

(use-package mise
  :config (global-mise-mode))

(use-package eev
  :config
  (require 'eev-load)
  (eev-mode 1)
  (evil-make-overriding-map eev-mode-map 'normal))
