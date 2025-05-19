;;; zenwritten-light-theme.el --- A zenbones variant with zero hue and saturation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Pharedes
;;
;; Author: Pharedes <pedro.aredes@hotmail.com>
;; Maintainer: Pharedes <pedro.aredes@hotmail.com>
;; Created: May 19, 2025
;; Modified: May 19, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/pharedes/dotfiles/blob/main/dotfiles/.emacs.d/etc/themes/
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A light theme based on the Zenwritten palette, using grey shades for text and colors for UI elements.
;;
;;; Code:

(require 'autothemer)

(autothemer-deftheme
 zenwritten-light "A zenbones variant with zero hue and saturation for text."

 ((((class color) (min-colors #xffffff)))

  ;; Define the Zenwritten color palette
  (fg                  "#353535")       ; Default text color
  (bg                  "#EEEEEE")       ; Background
  (fg-comment          "#8B8B8B")       ; Comments
  (fg-string           "#636363")       ; Strings and constants
  (fg-variable         "#505050")       ; Variables
  (fg-type             "#5C5C5C")       ; Types
  (bg-mode-line        "#DDDDDD")       ; Mode-line background
  (bg-highlight        "#D7D7D7")       ; Highlight background
  (color-error         "#A8334C")       ; Red for errors
  (color-warning       "#944927")       ; Yellow for warnings
  (color-success       "#4F6C31")       ; Green for success
  (color-link          "#286486")       ; Blue for links
  (color-info          "#88507D")       ; Magenta for info
  (color-diff-added    "#CBE5B8")       ; Light green for added lines
  (color-diff-removed  "#EBD8DA")       ; Light red for removed lines
  (color-diff-modified "#D4DEE7")       ; Light blue for modified lines
  (gray                "#989898")       ; Additional grey shade
  (ngrey-light         "#C6C3C3"))      ; Light grey shade

 ;; Customize faces
 (
  ;; Basic faces
  (default                             (:foreground fg :background bg))
  (button                              (:foreground color-link))
  (child-frame                         (:background bg-mode-line :foreground bg-mode-line))
  (child-frame-border                  (:background bg-mode-line :foreground bg-mode-line))
  (cursor                              (:background fg))
  (error                               (:foreground color-error :bold t))
  (success                             (:foreground color-success))
  (warning                             (:foreground color-warning))
  (fringe                              (:background bg))
  (glyph-face                          (:background bg-highlight))
  (glyphless-char                      (:foreground bg-highlight))
  (header-line                         (:background bg-mode-line))
  (highlight                           (:background bg-highlight))
  (hl-line                             (:background bg-highlight))
  (homoglyph                           (:foreground fg))
  (internal-border                     (:background bg-mode-line))
  (line-number                         (:foreground fg-comment))
  (line-number-current-line            (:foreground fg :bold t))
  (lv-separator                        (:background bg-mode-line :foreground fg))
  (match                               (:background bg-highlight :foreground fg))
  (menu                                (:background bg :foreground fg))
  (separator-line                      (:background bg-mode-line))
  (link                                (:foreground color-link :underline t))
  (shadow                              (:foreground fg-comment))
  (vertical-border                     (:foreground bg-highlight))
  (border                              (:background bg-mode-line))
  (trailing-whitespace                 (:background nil :foreground nil))
  (region                              (:background bg-highlight))
  (escape-glyph                        (:foreground fg))
  (minibuffer-prompt                   (:background bg-mode-line :foreground fg))

  ;; Fixed pitch and window dividers
  (fixed-pitch                         (:inherit 'unspecified))
  (window-border                       (:background bg-highlight :foreground bg-highlight))
  (window-divider                      (:background bg-highlight))
  (window-divider-first-pixel          (:foreground bg-highlight))
  (window-divider-last-pixel           (:background bg-highlight))

  ;; Mode line
  (mode-line                           (:background bg-mode-line :foreground fg))
  (mode-line-inactive                  (:background bg :foreground fg-comment))
  (mode-line-active                    (:background bg-mode-line :foreground fg))
  (mode-line-highlight                 (:foreground color-info))
  (mode-line-buffer-id                 (:foreground fg :bold t))

  ;; Doom mode line
  (doom-modeline-buffer-file           (:foreground fg :bold t))
  (doom-modeline-buffer-major-mode     (:foreground fg :bold t))
  (doom-modeline-buffer-modified       (:foreground color-error :bold t))
  (doom-modeline-buffer-path           (:foreground fg :bold t))
  (doom-modeline-error                 (:background color-error))
  (doom-modeline-info                  (:foreground color-info :bold t))
  (doom-modeline-project-dir           (:foreground fg :bold t))
  (doom-modeline-bar                   (:background color-info))

  ;; Doom mode line visual states
  (doom-modeline-evil-motion-state     (:foreground fg))
  (doom-modeline-evil-normal-state     (:foreground fg))
  (doom-modeline-evil-insert-state     (:foreground fg))
  (doom-modeline-evil-visual-state     (:foreground fg))
  (doom-modeline-evil-emacs-state      (:foreground fg))
  (doom-modeline-evil-replace-state    (:foreground fg))
  (doom-modeline-evil-operator-state   (:foreground fg))

  ;; Font lock (syntax highlighting)
  (font-lock-type-face                 (:foreground fg-type :italic t))
  (font-lock-warning-face              (:foreground color-warning :bold t))
  (font-lock-delimiters-face           (:foreground fg))
  (font-lock-punctuation-face          (:foreground fg))
  (font-lock-builtin-face              (:foreground fg))
  (font-lock-constant-face             (:foreground fg-string))
  (font-lock-comment-face              (:foreground fg-comment :italic t))
  (font-lock-comment-delimiter-face    (:foreground fg-comment))
  (font-lock-doc-markup-face           (:foreground fg-comment))
  (font-lock-string-face               (:foreground fg-string :italic t))
  (font-lock-keyword-face              (:foreground fg :bold t))
  (font-lock-function-name-face        (:foreground fg :weight 'semi-bold))
  (font-lock-variable-name-face        (:foreground fg-variable))
  (font-lock-regexp-grouping-backslash (:foreground fg))
  (font-lock-negation-char-face        (:foreground fg))

  ;; LSP-UI
  (lsp-ui-sideline-current-symbol      (:foreground fg :bold t))
  (lsp-ui-sideline-symbol              (:foreground fg-comment :box (:line-width -1 :color bg-highlight)))
  (lsp-ui-sideline-symbol-info         (:italic t))

  ;; Flycheck
  (flycheck-posframe-warning-face      (:foreground color-warning))
  (flycheck-fringe-warning             (:foreground color-warning))
  (flycheck-fringe-error               (:foreground color-error))

  ;; Org-mode
  (org-block                           (:background bg-mode-line))
  (org-link                            (:foreground color-link :underline t))
  (org-block-begin-line                (:foreground fg-comment :height 0.9))
  (org-block-end-line                  (:inherit 'org-block-begin-line))
  (org-date                            (:foreground fg :underline t))
  (org-level-1                         (:foreground fg :bold t))
  (org-level-2                         (:foreground fg :bold t))

  ;; Ivy/Ido
  (ivy-current-match                   (:foreground fg :bold t :underline t))
  (ivy-minibuffer-match-face-1         (:foreground fg))
  (ivy-minibuffer-match-face-2         (:foreground fg))
  (ivy-minibuffer-match-face-3         (:foreground fg))
  (ivy-minibuffer-match-face-4         (:foreground fg))
  (ido-only-match                      (:foreground fg))
  (ido-first-match                     (:foreground fg))
  (ido-subdir                          (:foreground fg))

  ;; Diff-mode
  (diff-changed                        (:foreground fg))
  (diff-added                          (:foreground color-success))
  (diff-removed                        (:foreground color-error))
  (diff-indicator-changed              (:inherit 'diff-changed))
  (diff-indicator-added                (:inherit 'diff-added))
  (diff-indicator-removed              (:inherit 'diff-removed))

  ;; JS2-mode
  (js2-warning                         (:underline (:color color-warning :style 'wave)))
  (js2-error                           (:underline (:color color-error :style 'wave)))
  (js2-external-variable               (:underline (:color color-success :style 'wave)))
  (js2-jsdoc-tag                       (:foreground fg-comment))
  (js2-jsdoc-type                      (:foreground fg-type))
  (js2-jsdoc-value                     (:foreground fg))
  (js2-function-param                  (:foreground fg-variable :italic t))
  (js2-function-call                   (:foreground fg))
  (js2-instance-member                 (:foreground fg))
  (js2-private-member                  (:foreground fg))
  (js2-private-function-call           (:foreground fg))
  (js2-jsdoc-html-tag-name             (:foreground fg-type))
  (js2-jsdoc-html-tag-delimiter        (:foreground fg))

  ;; Agda2
  (agda2-highlight-keyword-face               (:foreground fg :bold t))
  (agda2-highlight-string-face                (:foreground fg-string :italic t))
  (agda2-highlight-generalizable-variable-face (:foreground fg))
  (agda2-highlight-bound-variable-face        (:foreground fg :italic t))
  (agda2-highlight-number-face                (:foreground fg))
  (agda2-highlight-symbol-face                (:foreground fg))
  (agda2-highlight-datatype-face              (:foreground fg-type))
  (agda2-highlight-function-face              (:foreground fg))
  (agda2-highlight-inductive-constructor-face (:foreground fg :weight 'semi-bold))
  (agda2-highlight-coinductive-constructor-face (:foreground fg :bold t))
  (agda2-highlight-module-face                (:foreground fg :italic t))
  (agda2-highlight-postulate-face             (:foreground fg))
  (agda2-highlight-primitive-face             (:foreground fg :italic t))
  (agda2-highlight-record-face                (:foreground fg :italic t))

  ;; Symbol-mode
  (symbol-overlay-default-face         (:underline t))
  (symbol-overlay-face-1               (:underline (:color color-success)))
  (symbol-overlay-face-2               (:underline (:color color-warning)))
  (symbol-overlay-face-3               (:underline (:color color-error)))
  (symbol-overlay-face-4               (:underline (:color color-link)))
  (symbol-overlay-face-5               (:underline (:color color-info)))
  (symbol-overlay-face-6               (:underline (:color color-diff-added)))
  (symbol-overlay-face-7               (:underline (:color color-diff-removed)))
  (symbol-overlay-face-8               (:underline (:color color-diff-modified)))

  ;; HVM-mode
  (hvm-variable-face                   (:foreground fg-variable :italic t))
  (hvm-function-face                   (:foreground fg :weight 'semi-bold))
  (hvm-constructor-face                (:foreground fg))
  (hvm-number-face                     (:foreground fg))
  (hvm-char-face                       (:foreground fg-string))
  (hvm-operator-face                   (:foreground fg))
  (hvm-symbols-face                    (:foreground fg))
  (hvm-supdup-face                     (:foreground fg))
  (hvm-super-face                      (:foreground fg :italic t))
  (hvm-datatype-face                   (:foreground fg-type))
  (hvm-comment-face                    (:foreground fg-comment :italic t))
  (hvm-delimiters-face                 (:foreground fg))
  (hvm-declaration-face                (:foreground fg :bold t))
  (hvm-definition-face                 (:foreground fg))
  (hvm-control-face                    (:foreground fg))

  ;; Rainbow delimiters
  (rainbow-delimiters-depth-1-face     (:foreground "#353535"))
  (rainbow-delimiters-depth-2-face     (:foreground "#505050"))
  (rainbow-delimiters-depth-3-face     (:foreground "#5C5C5C"))
  (rainbow-delimiters-depth-4-face     (:foreground "#636363"))
  (rainbow-delimiters-depth-5-face     (:foreground "#8B8B8B"))
  (rainbow-delimiters-depth-6-face     (:foreground "#989898"))
  (rainbow-delimiters-depth-7-face     (:foreground "#C6C3C3"))
  (rainbow-delimiters-depth-8-face     (:foreground "#D7D7D7"))
  (rainbow-delimiters-depth-9-face     (:foreground "#DDDDDD"))
  (rainbow-delimiters-unmatched-face   (:foreground color-error :bold t))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenwritten-light)
;;; zenwritten-light-theme.el ends here
