;; ==================== Emacs configurations =======================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" default))
 '(package-selected-packages
   '(symbol-overlay 0x0 vterm igist show-conses autothemer gptel markdown-mode cape doom-modeline which-key-posframe which-key nerd-icons evil-commentary mini-frame bind-key eglot eldoc erc faceup idlwave jsonrpc org project soap-client tramp use-package verilog-mode xref eev evil))
 '(package-vc-selected-packages
   '((show-conses :vc-backend Git :url "https://github.com/edrx/show-conses")))
 '(tool-bar-mode nil))

(setq
 globals--email        (getenv "EMAIL") ; Email for GPG encryption
 globals--theme        'gruvbones                 ; Theme variable
 globals--leader-key   "<SPC>"                    ; Leader prefix key used for most bindings
 )

(when window-system
  (blink-cursor-mode 0) ; Disable the cursor blinking
  (scroll-bar-mode 0)   ; Disable the scroll bar
  (tool-bar-mode 0)     ; Disable the tool bar
  (tooltip-mode 0))     ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Use tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-help-function nil                           ; Disable help messages
 show-trailing-whitespace t                       ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 2                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 delete-old-versions -1                           ; Delete excess backup versions silently
 version-control t                                ; Use version control
 ring-bell-function 'ignore                       ; Silent bell when you make a mistake
 inhibit-compacting-font-caches t                 ; Faster navigation point (costs more memory)
 make-backup-files nil                            ; Stop creating backup files
 vc-follow-symlinks t                             ; When the symlink points to a version-controlled file
 use-default-font-for-symbols nil                 ; Do not use the frame font when rendering emojis
 frame-inhibit-implied-resize nil)                ; Don't ask for confirmation when opening symlinked file
(cd "~/")                                         ; Move to the user directory
(global-display-line-numbers-mode t)              ; Enable line numbers globally
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(global-auto-revert-mode 1)                       ; Automatically revert a buffer when it changes on disk
(fringe-mode '(8 . 0))                            ; Enable fringe on the left for git-gutter-fringe+
(electric-pair-mode t)                            ; Enable Matching delimeters
(electric-indent-mode t)                          ; Auto indentation
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'jump)                      ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(show-paren-mode 1)

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq backup-directory-alist            '((".*" . "~/.Trash")))

(add-to-list 'custom-theme-load-path (expand-file-name "etc/themes/" user-emacs-directory))

;; ================== Theme configurations ========================

;; Line number
(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code" :foundry "SAJA" :slant normal :weight regular :height 120 :width normal))))
 '(font-lock-comment-delimiter-face ((t (:slant italic))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(symbol-overlay-default-face ((t (:inherit highlight :underline t)))))

;; Autothemer setup
(unless (package-installed-p 'autothemer)
  (package-install 'autothemer))
(require 'autothemer)
(load-theme 'gruvbones t)

;; ================= Eev configurations ======================

;; See: (find-eev-levels-intro)
(require 'eev-load)               ; (find-eev "eev-load.el")
(require 'eev-aliases)            ; (find-eev "eev-aliases.el")
(eev-mode 1)                      ; (find-eev "eev-mode.el")

;; From: (find-angg-es-links)
(defun find-angg (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/%s" fname) rest))
(defun find-anggfile (fname &rest rest)
  (apply 'find-wget  (format "http://anggtwu.net/%s" fname) rest))
(defun find-es (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/e/%s.e" fname) rest))

;; From: (find-melpa-links)
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

;; See: (find-lean4-intro)
(add-to-list 'load-path "~/.emacs.d/elpa/lean4-mode")
(defun fli () (interactive) (find-lean4-intro))
(defun el4 () (interactive) (find-eev "eev-lean4.el"))
(require 'eev-lean4)      ; (find-eev "eev-lean4.el")

;; From: (find-windows-beginner-intro "8. Test Maxima with find-wget")
(code-c-d "maxima" "/usr/share/maxima/5.47.0/" "maxima")

;; From: (find-mpv-links)
;;  See: http://anggtwu.net/eev-videos.html#smaller-fullscreen
;;
(defun mf ()
  "Make mpv use (real) full screen."
  (interactive)
  (setq ee-mpv-video-options '("--fs" "--osd-level=2")))
;;
(defun ms ()
  "Make mpv use a \"smaller full screen\"."
  (interactive)
  (setq ee-mpv-video-options
	'("--fs" "--osd-level=2"
	  "--video-margin-ratio-bottom=0.15"
	  "--sub-font-size=35")))

;; ================= Motion Extension ===================
;; Evil mode

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Set leader key
(evil-set-leader 'normal (kbd "SPC"))

;; Set C-r to redo
(evil-set-undo-system 'undo-redo)

;; Make the eev keymap take precedence over Evil's keymap
(evil-make-overriding-map eev-mode-map 'normal)

;; Re-enable the eev keymap every time eev-mode is activated
(add-hook 'eev-mode-hook #'evil-normalize-keymaps)

(unless (package-installed-p 'symbol-overlay)
  (package-install 'symbol-overlay))

(require 'symbol-overlay)
(symbol-overlay-mode 1)

;; ================== Evil-commentary Setup =====================

;; Install and enable evil-commentary
(unless (package-installed-p 'evil-commentary)
  (package-install 'evil-commentary))

(evil-commentary-mode)

;; ============== Which-Key and keybinds =========================

(use-package nerd-icons
  :ensure t)

(require 'which-key)
(which-key-mode)
(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-state-maps t)

;; allow space to act as <leader> on visual mode
(define-key evil-motion-state-map " " nil)

(global-set-key (kbd "C-<tab>") 'dabbrev-completion)
(global-set-key (kbd "C-c C-l") 'eval-buffer)

;; symbol-overlay
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<leader> h") 'symbol-overlay-remove-all)

;; files
(evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader> f r") 'rename-file-and-buffer)
(evil-define-key 'normal 'global (kbd "<leader> f d") 'delete-file-and-buffer)

;; term
(evil-define-key 'normal 'global (kbd "<leader> t") 'vterm)

;; buffers
(evil-define-key 'normal 'global (kbd "<leader> w")   'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader> b n") 'switch-to-next-buffer)
(evil-define-key 'normal 'global (kbd "<leader> b p") 'switch-to-prev-buffer)
(evil-define-key 'normal 'global (kbd "<leader> b c") 'kill-buffer)

;; web macros
(evil-define-key 'normal 'global (kbd "<leader> o b") 'breww2) ;; this window
(evil-define-key 'normal 'global (kbd "<leader> o w") 'breww2) ;; that window

;; agda go-to-definition
(evil-define-key 'normal agda2-mode-map (kbd "<leader> d") 'agda2-goto-definition-keyboard)

;; commentary
(evil-define-key '(normal visual) 'global (kbd "<leader> /") 'evil-commentary-line)

;; gptel
(evil-define-key '(normal visual) 'global (kbd "S-SPC") 'gptel-send)
(evil-define-key '(normal visual) 'global (kbd "<leader> g") 'gptel-menu)

;; 0x0
(evil-define-key '(normal visual) 'global (kbd "<leader> s u") 'upload-0x0)


;; igist
(evil-define-key '(normal visual) 'global (kbd "<leader> s g") 'igist-dispatch)

;; Helper functions
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (when (yes-or-no-p (format "Are you sure you want to delete '%s'?" filename))
        (delete-file filename)
        (kill-buffer)))))


;; ====================== Macros ==========================

;; This appends the line to TODO file

;; Overwrite `ee-copy-this-line-to-kill-ring' with a fixed version:
(defun ee-copy-this-line-to-kill-ring (&optional arg)
"Copy the current line to the kill ring and highlight (\"flash\") it.
With a prefix argument run `ee-copy-preceding-tag-to-kill-ring' instead."
  (interactive "P")
  (if arg (ee-copy-preceding-tag-to-kill-ring)
    (let* ((start (ee-bol-skip-invisible))
	   (end   (ee-eol-skip-invisible))
	   (str   (buffer-substring start end))
	   (msg   "Copied the current line to the kill ring - use C-y to paste"))
      (eeflash+ start end eeflash-copy)
      (kill-new str)
      (message msg))))

;; Copy the current line to the end of the ~/-TODO.
;;   M-211j and M-221j work similarly to M-21j, and
;;   M-311j and M-331j work similarly to M-21j.
;; For more info in M-21j and M-31j, see:
;;   (find-eejumps "M-2 M-1 M-j")
;;   (find-eejumps "M-3 M-1 M-j")
;;
(defun eejump-211 () (eek "yy   M-3 M-1 M-j   M-> p RET   C-x o"))
(defun eejump-221 () (eek "yy   M-3 M-1 M-j   M-> p RET   C-x o"))
(defun eejump-311 () (eek "yy   M-3 M-1 M-j   M-> p RET"))
(defun eejump-331 () (eek "yy   M-3 M-1 M-j   M-> p RET"))

;; Test:
;; (eval-buffer)
;; (eek "M-2 M-2 M-1 M-j")

(defun find-eww2 (url &rest comments) (find-2a nil `(find-eww ,url)))
(code-brurl 'find-eww2 :remote 'breww2 :local 'breww2l :dired 'breww2d)

;; (find-esetkey-links   (kbd "C-c 2") 'breww2)

;; Tests:
;; (find-eww "https://www.lua.org/")
;; (find-eww2 "https://www.lua.org/")
;;
;; See:
;; (find-brxxx-intro)
;; (find-multiwindow-intro "3. High-level words")
;; (find-eev "eev-blinks.el" "find-eww")
;; (find-eev "eev-brxxx.el" "code-brxxxs" "find-eww")

;; =========== Agda cfgs ==================

;; load file (added by running `agda-mode setup` on terminal)
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; add literate markdown Agda file to filetypes
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

(require 'agda2-highlight)

;; Change backgrounds to boxes.
(cl-loop for (_ . face) in agda2-highlight-faces
      do (if (string-prefix-p "agda2-" (symbol-name face)) ;; Some non-Agda faces are in the list; don't change them
             (unless (equal face 'agda2-highlight-incomplete-pattern-face) ;; Workaround; this face is not defined in recent versions?
             (set-face-attribute face nil
               :box (face-attribute face :background)
               :background 'unspecified))))

;; Coverage warnings highlight the whole function;
;; change the box to an underline to be less intrusive.
(set-face-attribute 'agda2-highlight-coverage-problem-face nil
  :underline (face-attribute 'agda2-highlight-coverage-problem-face :box)
  :box 'unspecified)

;; Deadcode warnings highlight the whole line;
;; change the box to a strikethrough to be less intrusive,
;; as well as thematically appropriate.
(set-face-attribute 'agda2-highlight-deadcode-face nil
  :strike-through (face-attribute 'agda2-highlight-deadcode-face :box)
  :box 'unspecified)

(set-face-attribute 'agda2-highlight-level 'non-interactive)

;; ================= Auto Complete ==================

(setq dabbrev-case-fold-search nil
      dabbrev-case-replace nil
      dabbrev-check-other-buffers t)

;; ================ Emacs Session =============

;; ;; Session management
;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; ================== UI Stuff ===================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-modal-modern-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-flycheck-icon nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-bar-width 0)
  (doom-modeline-hud nil)
  (doom-modeline-height 20))

;; =================== AI stuff ==================

;; GPTel cfgs
(setq gptel-backend
      (gptel-make-openai "Groq"
        :host "api.groq.com"
        :endpoint "/openai/v1/chat/completions"
        :stream t
        :key (getenv "GROQ_API_KEY")
        :models '(llama-3.1-70b-versatile
                  llama-3.1-8b-instant
                  llama-3.2-3b-preview
                  llama-3.2-90b-vision-preview
                  mixtral-8x7b-32768
                  gemma-7b-it)))

;; ============== Coding Sharing ==================

;; igist setup
(setq igist-auth-marker 'igist)

;; 0x0 setup
(require 'dired)
(defalias 'upload-0x0 'ee-0x0-upload-region)
