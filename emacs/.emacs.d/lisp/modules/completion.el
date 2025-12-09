;;; modules/completion.el -*- lexical-binding: t; -*-

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preview-current nil)
  :init (global-corfu-mode))

(use-package corfu-candidate-overlay
  :straight (:type git
             :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
             :files (:defaults "*.el"))
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1)
  (global-set-key (kbd "TAB")
    (lambda ()
      (interactive)
      (or (corfu-candidate-overlay-complete-at-point)
          (indent-for-tab-command)))))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-track-known-projects t)
  (projectile-switch-project-action 'projectile-dired)
  :init
  (projectile-mode +1)
  :bind
  (("C-c p" . projectile-command-map)))

(use-package consult-projectile
  :after (consult projectile)
  :config
  (define-key projectile-command-map (kbd "f") 'consult-projectile))

(provide 'completion)
