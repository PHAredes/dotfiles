;;; modules/editor/config.el -*- lexical-binding: t; -*-

;; Unbind evil's default "f" and remap it to avy-goto-char
(after! evil
  (define-key evil-motion-state-map (kbd "f") 'avy-goto-char))
