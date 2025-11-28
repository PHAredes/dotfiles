;;; modules/email/packages.el -*- lexical-binding: t; -*-

(use-package mu4e
  :straight nil
  :defer t
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  :init
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (global-set-key (kbd "C-c m") 'mu4e)
  (global-set-key (kbd "C-c M") 'mu4e-compose-new))

(use-package consult-mu
  :straight (consult-mu :type git :host github :repo "armindarvish/consult-mu" :branch "main")
  :after (consult mu4e))