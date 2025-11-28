;;; modules/ui/config.el -*- lexical-binding: t; -*-

;; Clean UI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode 
                tooltip-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(fringe-mode 10)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode 1)


;; ============= Custom Modeline =============
(defface mode-line-evil-normal
  '((t (:background "#b8bb26" :foreground "#282828" :weight bold)))
  "Evil normal state indicator.")

(defface mode-line-evil-insert
  '((t (:background "#fb4934" :foreground "#282828" :weight bold)))
  "Evil insert state indicator.")

(defface mode-line-evil-visual
  '((t (:background "#fe8019" :foreground "#282828" :weight bold)))
  "Evil visual state indicator.")

(defface mode-line-evil-replace
  '((t (:background "#83a598" :foreground "#282828" :weight bold)))
  "Evil replace state indicator.")

(defun mode-line-evil-indicator ()
  "Return Evil mode indicator with color."
  (cond
   ((bound-and-true-p evil-normal-state-p) 
    (propertize " N " 'face 'mode-line-evil-normal))
   ((bound-and-true-p evil-insert-state-p) 
    (propertize " I " 'face 'mode-line-evil-insert))
   ((bound-and-true-p evil-visual-state-p) 
    (propertize " V " 'face 'mode-line-evil-visual))
   ((bound-and-true-p evil-replace-state-p) 
    (propertize " R " 'face 'mode-line-evil-replace))
   (t " - ")))

(defun mode-line-render (left right)
  "Render LEFT and RIGHT aligned modeline."
  (let ((available-width
         (- (window-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
  '((:eval (mode-line-render
            (list (mode-line-evil-indicator)
                  (propertize " %b" 'face '(:slant italic))
                  (if (and buffer-file-name (buffer-modified-p))
                      (propertize " *" 'face '(:foreground "#fb4934" :weight bold)))
                  (if (buffer-narrowed-p)
                      (propertize " -" 'face '(:foreground "#fabd2f" :weight bold))))
            '(" %m | %p | %l:%c ")))))

;; ============= Theme & Fonts =============
(add-to-list 'custom-theme-load-path (expand-file-name "etc/themes/" user-emacs-directory))

(after! rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun setup-fonts ()
  "Configure fonts for GUI."
  (when (display-graphic-p)
    (set-face-attribute 'default nil 
                        :family "CaskaydiaCove Nerd Font Mono" 
                        :height 110
                        :weight 'regular)
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (setq x-underline-t-descent-line t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (setup-fonts))))
  (setup-fonts))

(load-theme 'gruvbones t)

(after! good-scroll
  (good-scroll-mode 1))
