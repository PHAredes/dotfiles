;;; init.el

;; ============= Performance Boost =============
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Silencia mensagens de loading
(setq inhibit-message t)
(add-hook 'after-init-hook (lambda () (setq inhibit-message nil)))
(setq load-prefer-newer t)
(setq native-comp-async-report-warnings-errors nil)

(straight-use-package 'use-package)

(setq-default
 ad-redefinition-action 'accept
 auto-window-vscroll nil
 confirm-kill-emacs 'yes-or-no-p
 cursor-in-non-selected-windows nil
 delete-by-moving-to-trash t
 display-time-default-load-average nil
 system-time-locale "C"
 display-time-format "%d %b %I:%M %p"
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message ""
 mouse-yank-at-point t
 select-enable-clipboard t
 sentence-end-double-space nil
 show-trailing-whitespace t
 tab-width 2
 uniquify-buffer-name-style 'forward
 window-combination-resize t
 x-stretch-cursor t
 delete-old-versions -1
 version-control t
 make-backup-files nil
 backup-directory-alist '((".*" . "~/.Trash"))
 vc-follow-symlinks t
 frame-inhibit-implied-resize nil)

(cd "~/")
(global-display-line-numbers-mode t)
(delete-selection-mode 1)
(display-time-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode t)
(electric-indent-mode t)
(global-subword-mode 1)
(show-paren-mode 1)
(mouse-avoidance-mode 'jump)
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'custom-theme-load-path (expand-file-name "etc/themes/" user-emacs-directory))

(use-package autothemer :straight t :demand t)

(use-package nerd-icons :straight t)

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-flycheck-icon nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-bar-width 0)
  (doom-modeline-height 20)
  (doom-modeline-highlight-modified-buffer-name t)
  (doom-modeline-position-line-format '("%l:%c"))
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun pedro-config/gui-setup ()
  (interactive)
  (set-face-attribute 'default nil :family "Cascadia Code PL" :height 120)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
  (load-theme 'gruvbones t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame
                (pedro-config/gui-setup)))))

(when (display-graphic-p)
  (pedro-config/gui-setup))

;; ============= Evil Mode =============
(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-normal-state-map (kbd "RET") nil)
  (define-key evil-insert-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd " ") nil)
  (define-key evil-visual-state-map (kbd " ") nil)
  (define-key evil-insert-state-map (kbd " ") nil))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config (evil-commentary-mode 1))

;;; ============= Completion (Corfu + Cape) =============
(use-package corfu
  :straight t
  :custom
  (corfu-auto nil)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))

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
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t))

(use-package vertico
  :straight t
  :demand t
  :config
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package orderless
  :straight t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-y" . consult-yank-pop)))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package crux
  :straight t)

;; ============= Session Management =============
(setq desktop-dirname (expand-file-name "desktop/" user-emacs-directory)
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-load-locked-desktop t
      desktop-auto-save-timeout 30
      desktop-restore-eager 8)

(unless (file-directory-p desktop-dirname)
  (make-directory desktop-dirname t))

(desktop-save-mode 1)

(setq savehist-file (expand-file-name "savehist" user-emacs-directory)
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
      history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history t)

(savehist-mode 1)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(save-place-mode 1)

(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 200
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)

(recentf-mode 1)

(add-hook 'kill-emacs-hook #'desktop-save-in-desktop-dir)

;; ============= Keybindings =============
(setq globals--leader-key "<SPC>")
(evil-set-leader 'normal (kbd globals--leader-key))
(evil-set-leader 'visual (kbd globals--leader-key))

(setq consult-fd-args "fd --color=never --full-path --hidden --exclude .git")

(evil-define-key 'normal 'global
  (kbd "<leader> SPC") 'consult-find
  (kbd "<leader> ,") 'consult-buffer
  (kbd "<leader> .") 'find-file
  (kbd "<leader> :") 'execute-extended-command
  (kbd "<leader> f f") (lambda () (interactive) (consult-fd "~/"))
  (kbd "<leader> f r") 'consult-recent-file
  (kbd "<leader> f R") 'crux-rename-file-and-buffer
  (kbd "<leader> f d") 'crux-delete-file-and-buffer
  (kbd "<leader> t")   'eat
  (kbd "<leader> f s") 'save-buffer
  (kbd "<leader> b n") 'next-buffer
  (kbd "<leader> b p") 'previous-buffer
  (kbd "<leader> b b") 'consult-buffer
  (kbd "<leader> b k") 'kill-buffer
  (kbd "<leader> b l") 'eval-buffer
  (kbd "<leader> s s") 'consult-line
  (kbd "<leader> s p") 'consult-ripgrep
  (kbd "<leader> d") 'xref-find-definitions
  (kbd "<leader> h") 'symbol-overlay-remove-all
  (kbd "<leader> q s") 'desktop-save
  (kbd "<leader> q l") 'desktop-read
  (kbd "<leader> s u") 'upload-0x0
  (kbd "<leader> s g") 'igist-dispatch
  (kbd "<leader> o b") 'breww2
  (kbd "<leader> o w") 'breww2
  (kbd "<leader> g") 'magit-status)

(evil-define-key '(normal visual) 'global
  (kbd "<leader> /") 'evil-commentary-line)

(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)

;; ============= Git =============
(use-package magit
  :straight t
  :custom
  (magit-auto-revert-mode t)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ============= Tools =============
(use-package symbol-overlay
  :straight t
  :demand t
  :config
  (symbol-overlay-mode 1)
  (set-face-attribute 'symbol-overlay-default-face nil :inherit 'highlight :underline t))

(use-package igist :straight t :custom (igist-auth-marker 'igist))
(use-package 0x0 :straight t :config (defalias 'upload-0x0 'ee-0x0-upload-region))
(use-package mise :straight t :demand t :config (global-mise-mode))

;; ============= Terminal (Eat) =============
(use-package eat
  :straight t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :config
  (add-hook 'eshell-mode-hook #'eat-eshell-mode))

;; ============= LSP =============
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((typescript-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp))
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (setq lsp-haskell-server-args '("--lsp"))
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-completion-provider :none))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

;; ============= Languages =============
(use-package haskell-mode
  :straight t
  :hook
  ((haskell-mode . interactive-haskell-mode)
   (haskell-mode . turn-on-haskell-doc-mode)
   (haskell-mode . turn-on-haskell-indentation)))

(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package tree-sitter
  :straight t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package prettier-js
  :straight t
  :hook ((typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))

;; ============= Agda =============
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

;; Carrega agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; Ativa agda-input globalmente
(require 'agda-input)

;; Ativa em todos os modos de texto e programação
(add-hook 'text-mode-hook (lambda () (set-input-method "Agda")))
(add-hook 'prog-mode-hook (lambda () (set-input-method "Agda")))

;; Keybindings
(global-set-key (kbd "C-c \\") (lambda () (interactive) (set-input-method "Agda")))
(global-set-key (kbd "C-c C-\\") (lambda () (interactive) (deactivate-input-method)))

(with-eval-after-load 'agda-input
  (setq agda-input-translations
        (delete '("Glb" . ("⨅"))
                agda-input-translations))
  (agda-input-setup))


;; Customizações de highlight
(require 'agda2-highlight)

(cl-loop for (_ . face) in agda2-highlight-faces
         do (when (and (string-prefix-p "agda2-" (symbol-name face))
                       (not (equal face 'agda2-highlight-incomplete-pattern-face)))
              (set-face-attribute face nil
                                  :box (face-attribute face :background)
                                  :background 'unspecified)))

(set-face-attribute 'agda2-highlight-coverage-problem-face nil
                    :underline (face-attribute 'agda2-highlight-coverage-problem-face :box)
                    :box 'unspecified)

(set-face-attribute 'agda2-highlight-deadcode-face nil
                    :strike-through (face-attribute 'agda2-highlight-deadcode-face :box)
                    :box 'unspecified)

;; Keybinding para goto-definition no agda-mode
(evil-define-key 'normal agda2-mode-map (kbd "<leader> d") 'agda2-goto-definition-keyboard)

;; ============= Eev configurations =============
(use-package eev
  :straight t
  :demand t
  :config
  (require 'eev-load)
  (require 'eev-aliases)
  (eev-mode 1)
  (evil-make-overriding-map eev-mode-map 'normal)
  (add-hook 'eev-mode-hook #'evil-normalize-keymaps))

(defun find-angg (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/%s" fname) rest))

(defun find-anggfile (fname &rest rest)
  (apply 'find-wget  (format "http://anggtwu.net/%s" fname) rest))

(defun find-es (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/e/%s.e" fname) rest))

(defvar ee-chrome-program "google-chrome-stable")
(defun find-chrome (url) (find-bgprocess `(,ee-chrome-program ,url)))

(global-set-key (kbd "C-c f") 'chrome-find)
(defun chrome-find ()
  (interactive)
  (let ((texto (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert "(find-chrome \"" texto "\")")))

(defun ee-copy-this-line-to-kill-ring (&optional arg)
  (interactive "P")
  (if arg (ee-copy-preceding-tag-to-kill-ring)
    (let* ((start (ee-bol-skip-invisible))
           (end   (ee-eol-skip-invisible))
           (str   (buffer-substring start end))
           (msg   "Copied the current line to the kill ring - use C-y to paste"))
      (eeflash+ start end eeflash-copy)
      (kill-new str)
      (message msg))))

(defun eejump-211 () (eek "yy   M-3 M-1 M-j   M-> p RET   C-x o"))
(defun eejump-221 () (eek "yy   M-3 M-1 M-j   M-> p RET   C-x o"))
(defun eejump-311 () (eek "yy   M-3 M-1 M-j   M-> p RET"))
(defun eejump-331 () (eek "yy   M-3 M-1 M-j   M-> p RET"))

(defun find-eww2 (url &rest comments) (find-2a nil `(find-eww ,url)))
(code-brurl 'find-eww2 :remote 'breww2 :local 'breww2l :dired 'breww2d)

;; ============= Environment =============
(let ((custom-paths '("/home/pedro/.cabal/bin"
                       "/home/pedro/.local/bin"
                       "/home/pedro/.local/share/mise/shims")))
  (setenv "PATH" (concat (mapconcat 'identity custom-paths ":") ":" (getenv "PATH")))
  (setq exec-path (append custom-paths exec-path)))

(custom-set-variables
 '(custom-safe-themes
   '("59cf109707de43c2d055e6ec4ce1fc37b40d87b5fdedd9d3e26d9c451116fa3a"
     "ce8f04cf77435c027c696397378136d8dba4865104d6cb781fe6c2ac2aed2dea"
     "1d5e26b375169adb28df8b658ee4f40b330e21a9e05c962f3c53d83ca060f2bb"
     "1b32892ed4e7afb8fe276f68b8e249c79d03534b06cb68f3fef9404cb2b28894"
     "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64"
     "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563"
     "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f"
     "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe"
     "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7"
     "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" default))
 '(haskell-process-show-debug-tips nil)
 '(package-vc-selected-packages '())
 '(safe-local-variable-values '((eval turn-off-auto-fill))))
(custom-set-faces)

;;; init.el ends here
