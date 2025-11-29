;;; modules/tools.el -*- lexical-binding: t; -*-

(use-package magit)

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
  (evil-make-overriding-map eev-mode-map 'normal)
  
  (defun find-angg (fname &rest rest)
    (apply 'find-wgeta (format "http://anggtwu.net/%s" fname) rest))
  
  (defun find-es (fname &rest rest)
    (apply 'find-wgeta (format "http://anggtwu.net/e/%s.e" fname) rest))
  
  (defun find-chrome (url)
    (find-bgprocess `("google-chrome-stable" ,url)))
  
  (defun find-eww2 (url &rest comments) 
    (find-2a nil `(find-eww ,url)))
  
  (code-brurl 'find-eww2 :remote 'breww2 :local 'breww2l :dired 'breww2d))

(provide 'tools)
