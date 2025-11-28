;;; modules/core/config.el -*- lexical-binding: t; -*-

;; ============= Core Defaults =============
(setq-default
  indent-tabs-mode nil
  tab-width 2
  fill-column 80
  truncate-lines t
  cursor-in-non-selected-windows nil
  inhibit-startup-screen t
  initial-scratch-message ""
  use-short-answers t
  frame-inhibit-implied-resize t
  make-backup-files nil
  auto-save-default nil
  create-lockfiles nil
  ring-bell-function 'ignore)

;; Essential modes
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 25)

(fset 'yes-or-no-p 'y-or-n-p)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(cd "~/")

;; ============= Keybindings =============
(after! evil
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (setq consult-fd-args "fd --color=never --full-path --hidden --exclude .git")

  (evil-define-key 'normal 'global
    (kbd "<leader>SPC") 'consult-find
    (kbd "<leader>,")   'consult-buffer
    (kbd "<leader>.")   'find-file
    (kbd "<leader>:")   'execute-extended-command
    
    (kbd "<leader>ff")  (lambda () (interactive) (consult-fd "~/"))
    (kbd "<leader>fr")  'consult-recent-file
    (kbd "<leader>fR")  'crux-rename-file-and-buffer
    (kbd "<leader>fd")  'crux-delete-file-and-buffer
    (kbd "<leader>fs")  'save-buffer
    
    (kbd "<leader>bn")  'next-buffer
    (kbd "<leader>bp")  'previous-buffer
    (kbd "<leader>bb")  'consult-buffer
    (kbd "<leader>bk")  'kill-buffer
    (kbd "<leader>bl")  'eval-buffer
    
    (kbd "<leader>ss")  'consult-line
    (kbd "<leader>sp")  'consult-ripgrep
    
    (kbd "<leader>t")   'eat
    (kbd "<leader>g")   'magit-status
    (kbd "<leader>d")   'xref-find-definitions
    (kbd "<leader>h")   'symbol-overlay-remove-all
    
    (kbd "<leader>su")  'upload-0x0
    (kbd "<leader>sg")  'igist-dispatch
    
    (kbd "<leader>ob")  'breww2
    (kbd "<leader>ow")  'breww2)

  (evil-define-key '(normal visual) 'global
    (kbd "<leader>/") 'evil-commentary-line))
