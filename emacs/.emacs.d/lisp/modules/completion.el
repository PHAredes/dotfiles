;;; modules/completion.el -*- lexical-binding: t; -*-

(use-package savehist
  :init
  (savehist-mode 1))

(use-package recentf
  :init
  (recentf-mode 1))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  :bind (:map corfu-map
              ("TAB" . +completion-tab)
              ([tab] . +completion-tab)))

(defun +completion-ghost-available-p ()
  (and (fboundp 'corfu-candidate-overlay-complete-at-point)
       (boundp 'corfu-candidate-overlay--overlay)
       (overlayp corfu-candidate-overlay--overlay)
       (let ((ghost (overlay-get corfu-candidate-overlay--overlay 'after-string)))
         (and (stringp ghost)
              (not (string-empty-p ghost))))))

(defun +completion-menu-visible-p ()
  (and (boundp 'corfu--frame)
       (frame-live-p corfu--frame)
       (frame-visible-p corfu--frame)))

(defun +completion-tab ()
  (interactive)
  (let ((orig-point (point))
        (orig-tick (buffer-chars-modified-tick)))
    (if (+completion-ghost-available-p)
        (corfu-candidate-overlay-complete-at-point)
      (completion-at-point))
    (unless (or (/= orig-point (point))
                (/= orig-tick (buffer-chars-modified-tick))
                (+completion-menu-visible-p))
      (indent-for-tab-command))))

(global-set-key (kbd "TAB") #'+completion-tab)
(global-set-key (kbd "<tab>") #'+completion-tab)

(use-package corfu-candidate-overlay
  :straight (:type git
             :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
             :files (:defaults "*.el"))
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1)
  (set-face-attribute 'corfu-candidate-overlay-face nil
                      :inherit 'shadow
                      :foreground 'unspecified
                      :weight 'normal)
  (set-face-attribute 'corfu-candidate-overlay-face-exact-match nil
                      :inherit 'corfu-candidate-overlay-face
                      :underline t))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-count-format (cons "%-6s " "%s/%s")))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  (orderless-affix-dispatch-alist
   '((?! . orderless-without-literal)
     (?& . orderless-annotation)
     (?% . char-fold-to-regexp)
     (?` . orderless-initialism)
     (?= . orderless-literal)
     (?^ . orderless-literal-prefix)
     (?~ . orderless-flex))))

(use-package marginalia
  :init
  (marginalia-mode))

(autoload 'consult-mode-command "consult" nil t)
(defalias 'consult-M-x #'consult-mode-command)

(use-package project
  :init
  (require 'project)
  :custom
  (project-switch-commands 'project-prefix-or-any-command)
  (project-vc-extra-root-markers
   '(".project"
     ".projectile"
     ".agda-lib"
     "package.json"
     "go.mod"
     "cabal.project"
     "pyproject.toml"
     "Cargo.toml"))
  :config
  (define-key global-map (kbd "C-c p") project-prefix-map))

(defun +project-root-or-default ()
  (if-let ((pr (project-current nil)))
      (project-root pr)
    default-directory))

(defun +consult-project-fd ()
  (interactive)
  (consult-fd (+project-root-or-default)))

(defun +consult-project-rg ()
  (interactive)
  (consult-ripgrep (+project-root-or-default)))

(use-package consult
  :bind (([remap execute-extended-command] . consult-mode-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ("C-s" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-s d" . consult-find)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         ("M-s" . consult-history)
         :map project-prefix-map
         ("b" . consult-project-buffer)
         ("f" . +consult-project-fd)
         ("s" . +consult-project-rg))
  :custom
  (consult-narrow-key "<")
  (consult-project-function #'consult--default-project-function)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-preview-key '(:debounce 0.2 any))
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult-theme consult-xref
   :preview-key '(:debounce 0.2 any)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completion)
