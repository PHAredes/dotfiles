;;; modules/lang/agda/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\.lagda.md\'" . agda2-mode))

(with-eval-after-load 'agda2-mode
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (require 'agda-input)
  (evil-define-key 'normal agda2-mode-map 
    (kbd "<leader>d") 'agda2-goto-definition-keyboard))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))

