;;; gruvbones-theme.el --- A pleasant theme with earthy shades, inspired by Gruvbox -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 PHAredes
;;
;; Author: PHAredes <pedro.aredes@hotmail.com>
;; Maintainer: PHAredes <pedro.aredes@hotmail.com>
;; Created: october 30, 2025
;; Modified: abril 17, 2025
;; Version: 0.0.6
;; Homepage: https://github.com/PHAredes/dotfiles/blob/main/dotfiles/.emacs.d/etc/themes/
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'autothemer)

(autothemer-deftheme
 gruvbones "Inspired by Gruvbox tones with shades of neutral grey"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (bg              "#282828")
  (fg              "#ebdbb2")
  (rose            "#fb4934")
  (leaf            "#b8bb26")
  (wood            "#fabd2f")
  (water           "#83a598")
  (blossom         "#d3869b")
  (sky             "#83c07c")

  ;; Additional colors (derived or complementary)
  (bg-light        "#3c3836")
  (bg-dark         "#1d2021")
  (fg-dark         "#a89984")
  (ngrey-mid       "#727272")
  (ngrey-light     "#a8a8a8")
  (fg-mid          "#bdae93")
  (fg-light        "#fbf1c7")
  (gray            "#928374")
  (orange          "#fe8019")
  (yellow          "#fabd2f")
  (aqua            "#8ec07c"))

 ;; Customize faces
 (
  (default                             (:foreground fg :background bg))
  (button                              (:foreground water))
  (child-frame                         (:background bg-dark :foreground bg-dark))
  (child-frame-border                  (:background bg-dark :foreground bg-dark))
  (cursor                              (:background fg))
  (error                               (:foreground rose :bold t))
  (success                             (:foreground leaf))
  (warning                             (:foreground wood))
  (fringe                              (:background nil))
  (glyph-face                          (:background bg-light))
  (glyphless-char                      (:foreground bg-light))
  (header-line                         (:background bg-dark))
  (highlight                           (:background gray :foreground fg-light))
  (hl-line                             (:background bg-light))
  (homoglyph                           (:foreground leaf))
  (internal-border                     (:background bg-dark))
  (line-number                         (:foreground ngrey-mid))
  (line-number-current-line            (:foreground rose :bold t))
  (lv-separator                        (:background bg-dark :foreground water))
  (match                               (:background water :foreground bg-dark))
  (menu                                (:background bg :foreground fg))
  (separator-line                      (:background bg-dark))
  (link                                (:foreground water :underline t))
  (shadow                              (:foreground gray))
  (vertical-border                     (:foreground bg-light))
  (border                              (:background water :foreground fg-light))
  (trailing-whitespace                 (:background nil :foreground nil))
  (region                              (:background bg-light))
  (escape-glyph                        (:background leaf))
  (minibuffer-prompt                   (:background bg-dark :foreground leaf))

  (fixed-pitch                         (:inherit 'unspecified))
  (window-border                       (:background bg-light :foreground bg-light))
  (window-divider                      (:background bg-light))
  (window-divider-first-pixel          (:foreground bg-light))
  (window-divider-last-pixel           (:background bg-light))


  ;; Mode line
  (mode-line                           (:background bg-light :foreground fg))
  (mode-line-inactive                  (:background nil :foreground gray :bold nil))
  (mode-line-active                    (:background bg-light :foreground fg-dark :bold nil))
  (mode-line-highlight                 (:foreground wood))
  (mode-line-buffer-id                 (:foreground blossom :bold t))

  ;; Doom mode line
  (doom-modeline-buffer-file           (:inherit 'bold :foreground wood))
  (doom-modeline-buffer-major-mode     (:foreground wood :bold t))
  (doom-modeline-buffer-modified       (:foreground rose :inherit 'bold))
  (doom-modeline-buffer-path           (:foreground wood :inherit 'bold))
  (doom-modeline-error                 (:background rose))
  (doom-modeline-info                  (:foreground wood :bold t))
  (doom-modeline-project-dir           (:foreground water :bold t))
  (doom-modeline-bar                   (:background orange))

  ;; Doom mode line visual state
  (doom-modeline-evil-motion-state     (:foreground wood))
  (doom-modeline-evil-normal-state     (:foreground wood))
  (doom-modeline-evil-insert-state     (:foreground water))
  (doom-modeline-evil-visual-state     (:foreground fg-light))
  (doom-modeline-evil-emacs-state      (:foreground leaf))
  (doom-modeline-evil-replace-state    (:foreground sky))
  (doom-modeline-evil-operator-state   (:foreground leaf))

  ;; Font lock
  (font-lock-type-face                 (:foreground blossom))
  (font-lock-warning-face              (:foreground rose :bold t))
  (font-lock-delimiters-face           (:foreground ngrey-mid))
  (font-lock-punctuation-face          (:foreground ngrey-mid))
  (font-lock-builtin-face              (:foreground orange))
  (font-lock-constant-face             (:foreground blossom))
  (font-lock-comment-face              (:foreground ngrey-mid))
  (font-lock-comment-delimiter-face    (:foreground ngrey-mid))
  (font-lock-doc-markup-face           (:foreground ngrey-mid))
  (font-lock-string-face               (:foreground fg-mid :italic t))
  (font-lock-keyword-face              (:foreground rose :bold t))
  (font-lock-function-name-face        (:foreground fg-dark :weight 'semi-bold))
  (font-lock-variable-name-face        (:foreground water))
  (font-lock-regexp-grouping-backslash (:foreground water))
  (font-lock-negation-char-face        (:foreground rose))

  ;; which-key
  (which-key-key-face                  (:foreground water :bold t))
  (which-key-separator-face            (:foreground gray))
  (which-key-note-face                 (:foreground wood))
  (which-key-command-description-face  (:foreground fg))
  (which-key-local-map-description-face (:foreground leaf))
  (which-key-group-description-face    (:foreground blossom :italic t))
  (which-key-highlighted-command-face  (:foreground wood :bold t :underline t))

  ;; vertico
  (vertico-current                     (:background bg-light :bold t))
  (vertico-group-title                 (:foreground blossom :italic t))
  (vertico-group-separator             (:foreground gray :strike-through t))
  (vertico-multiline                   (:foreground wood))

  ;; orderless
  (orderless-match-face-0              (:foreground water :bold t))
  (orderless-match-face-1              (:foreground wood :bold t))
  (orderless-match-face-2              (:foreground leaf :bold t))
  (orderless-match-face-3              (:foreground blossom :bold t))

  ;; marginalia
  (marginalia-key                      (:foreground water))
  (marginalia-documentation            (:foreground fg-mid :italic t))
  (marginalia-variable                 (:foreground leaf))
  (marginalia-command                  (:foreground wood))
  (marginalia-mode                     (:foreground blossom))
  (marginalia-function                 (:foreground sky))
  (marginalia-file-name                (:foreground fg))
  (marginalia-file-owner               (:foreground gray))
  (marginalia-file-priv-dir            (:foreground water :bold t))
  (marginalia-file-priv-read           (:foreground leaf))
  (marginalia-file-priv-write          (:foreground wood))
  (marginalia-file-priv-exec           (:foreground rose))

  ;; consult
  (consult-file                        (:foreground fg))
  (consult-preview-line                (:background bg-light))
  (consult-line-number                 (:foreground ngrey-mid))
  (consult-line-number-wrapped         (:foreground ngrey-mid :italic t))
  (consult-line-number-prefix          (:foreground gray))
  (consult-separator                   (:foreground gray))
  (consult-bookmark                    (:foreground blossom))
  (consult-buffer                      (:foreground fg))
  (consult-imenu-prefix                (:foreground gray))

  ;; embark
  (embark-keybinding                   (:foreground water :bold t))
  (embark-collect-marked               (:background bg-light :foreground wood))
  (embark-collect-group-title          (:foreground blossom :italic t))

  ;; company
  (company-tooltip                     (:background bg-light :foreground fg))
  (company-tooltip-selection           (:background bg-dark :foreground fg-light :bold t))
  (company-tooltip-common              (:foreground water :bold t))
  (company-tooltip-common-selection    (:foreground water :bold t))
  (company-tooltip-annotation          (:foreground gray :italic t))
  (company-tooltip-annotation-selection (:foreground gray :italic t))
  (company-scrollbar-fg                (:background gray))
  (company-scrollbar-bg                (:background bg-dark))
  (company-preview                     (:foreground gray))
  (company-preview-common              (:foreground water :bold t))

  ;; lsp-mode
  (lsp-face-highlight-textual          (:background bg-light :bold t))
  (lsp-face-highlight-read             (:background bg-light :underline t))
  (lsp-face-highlight-write            (:background bg-light :bold t :underline t))
  (lsp-headerline-breadcrumb-separator-face (:foreground gray))
  (lsp-headerline-breadcrumb-symbols-face (:foreground fg))
  (lsp-headerline-breadcrumb-path-face (:foreground fg-dark))

  ;; lsp-ui
  (lsp-ui-sideline-current-symbol      (:inherit 'border :bold t))
  (lsp-ui-sideline-symbol              (:foreground gray :box (:line-width -1 :color bg-light)))
  (lsp-ui-sideline-symbol-info         (:italic t))
  (lsp-ui-doc-background               (:background bg-dark))
  (lsp-ui-doc-header                   (:foreground wood :bold t))
  (lsp-ui-peek-header                  (:background bg-light :foreground fg))
  (lsp-ui-peek-list                    (:background bg-dark))
  (lsp-ui-peek-peek                    (:background bg-dark))
  (lsp-ui-peek-selection               (:background bg-light :bold t))
  (lsp-ui-peek-line-number             (:foreground ngrey-mid))
  (lsp-ui-peek-highlight               (:foreground water :bold t :underline t))

  ;; flycheck
  (flycheck-posframe-warning-face      (:foreground leaf))
  (flycheck-fringe-warning             (:foreground leaf))
  (flycheck-fringe-error               (:foreground rose))

  ;; magit
  (magit-section-heading               (:foreground wood :bold t))
  (magit-section-highlight             (:background bg-light))
  (magit-section-heading-selection     (:foreground wood :bold t))
  (magit-diff-file-heading             (:foreground fg :bold t))
  (magit-diff-file-heading-highlight   (:background bg-light :bold t))
  (magit-diff-file-heading-selection   (:foreground water :bold t))
  (magit-diff-hunk-heading             (:background bg-dark :foreground fg-dark))
  (magit-diff-hunk-heading-highlight   (:background bg-light :foreground fg))
  (magit-diff-hunk-heading-selection   (:background bg-light :foreground water :bold t))
  (magit-diff-context                  (:foreground fg-dark))
  (magit-diff-context-highlight        (:background bg-light :foreground fg))
  (magit-diff-added                    (:foreground leaf))
  (magit-diff-added-highlight          (:background bg-dark :foreground leaf :bold t))
  (magit-diff-removed                  (:foreground rose))
  (magit-diff-removed-highlight        (:background bg-dark :foreground rose :bold t))
  (magit-diffstat-added                (:foreground leaf))
  (magit-diffstat-removed              (:foreground rose))
  (magit-hash                          (:foreground gray))
  (magit-branch-local                  (:foreground water :bold t))
  (magit-branch-remote                 (:foreground leaf :bold t))
  (magit-branch-current                (:foreground water :bold t :underline t))
  (magit-tag                           (:foreground wood :bold t))
  (magit-log-author                    (:foreground blossom))
  (magit-log-date                      (:foreground gray))
  (magit-log-graph                     (:foreground gray))
  (magit-process-ok                    (:foreground leaf :bold t))
  (magit-process-ng                    (:foreground rose :bold t))
  (magit-reflog-commit                 (:foreground leaf))
  (magit-reflog-amend                  (:foreground blossom))
  (magit-reflog-merge                  (:foreground leaf))
  (magit-reflog-checkout               (:foreground water))
  (magit-reflog-reset                  (:foreground rose))
  (magit-reflog-rebase                 (:foreground blossom))
  (magit-reflog-cherry-pick            (:foreground leaf))
  (magit-reflog-remote                 (:foreground sky))
  (magit-reflog-other                  (:foreground sky))

  ;; evil
  (evil-ex-info                        (:foreground rose :italic t))
  (evil-ex-search                      (:background wood :foreground bg-dark))
  (evil-ex-substitute-matches          (:background rose :foreground bg-dark))
  (evil-ex-substitute-replacement      (:background leaf :foreground bg-dark))

  ;; org-mode
  (org-block                           (:background bg-dark))
  (org-link                            (:foreground water :bold t :underline t))
  (org-block-begin-line                (:inherit 'shadow :height 0.9))
  (org-block-end-line	               (:inherit 'org-block-begin-line))
  (org-date                            (:foreground water :underline t :bold t))
  (org-level-1                         (:foreground rose :bold t))
  (org-level-2                         (:foreground water :bold t))

  ;; ivy/ido (mantido para compatibilidade)
  (ivy-current-match                   (:foreground fg-light :bold t :underline t))
  (ivy-minibuffer-match-face-1         (:foreground orange))
  (ivy-minibuffer-match-face-2         (:foreground wood))
  (ivy-minibuffer-match-face-3         (:foreground rose))
  (ivy-minibuffer-match-face-4         (:foreground sky))
  (ido-only-match                      (:foreground wood))
  (ido-first-match                     (:foreground wood))
  (ido-subdir                          (:foreground rose))

  ;; diff-mode
  (diff-changed                        (:foreground fg))
  (diff-added                          (:foreground leaf))
  (diff-removed                        (:foreground rose))
  (diff-indicator-changed              (:inherit 'diff-changed))
  (diff-indicator-added                (:inherit 'diff-added))
  (diff-indicator-removed              (:inherit 'diff-removed))

  ;; js2-mode
  (js2-warning                         (:underline (:color wood :style 'wave)))
  (js2-error                           (:underline (:color rose :style 'wave)))
  (js2-external-variable               (:underline (:color leaf :style 'wave)))
  (js2-jsdoc-tag                       (:foreground gray))
  (js2-jsdoc-type                      (:foreground fg-dark))
  (js2-jsdoc-value                     (:foreground fg))
  (js2-function-param                  (:foreground water :italic t))
  (js2-function-call                   (:foreground water))
  (js2-instance-member                 (:foreground orange))
  (js2-private-member                  (:foreground sky))
  (js2-private-function-call           (:foreground water))
  (js2-jsdoc-html-tag-name             (:foreground fg-dark))
  (js2-jsdoc-html-tag-delimiter        (:foreground fg))

  ;; haskell-mode
  (haskell-keyword-face                (:foreground rose :bold t))
  (haskell-type-face                   (:foreground blossom))
  (haskell-constructor-face            (:foreground fg :weight 'semi-bold))
  (haskell-definition-face             (:foreground fg-dark :weight 'semi-bold))
  (haskell-operator-face               (:foreground ngrey-mid))
  (haskell-pragma-face                 (:foreground gray :italic t))
  (haskell-literate-comment-face       (:foreground ngrey-mid :italic t))

  ;; agda2-mode
  (agda2-highlight-keyword-face               (:foreground rose))
  (agda2-highlight-string-face                (:foreground fg-mid :italic t))
  (agda2-highlight-generalizable-variable-face  (:foreground fg))
  (agda2-highlight-bound-variable-face        (:foreground fg :italic))
  (agda2-highlight-number-face                (:foreground orange))
  (agda2-highlight-symbol-face                (:foreground ngrey-mid))
  (agda2-highlight-datatype-face              (:foreground water))
  (agda2-highlight-function-face              (:foreground fg))
  (agda2-highlight-inductive-constructor-face (:foreground fg :weight 'semi-bold))
  (agda2-highlight-coinductive-constructor-face (:foreground fg :bold t))
  (agda2-highlight-module-face                (:foreground fg-light :italic t))
  (agda2-highlight-postulate-face             (:foreground orange))
  (agda2-highlight-primitive-face             (:foreground sky :italic t))
  (agda2-highlight-record-face                (:foreground wood :italic t))

  ;; symbol-overlay
  (symbol-overlay-default-face (:underline t))
  (symbol-overlay-face-1 (:underline (:color leaf)))
  (symbol-overlay-face-2 (:underline (:color wood)))
  (symbol-overlay-face-3 (:underline (:color rose)))
  (symbol-overlay-face-4 (:underline (:color sky)))
  (symbol-overlay-face-5 (:underline (:color orange)))
  (symbol-overlay-face-6 (:underline (:color blossom)))
  (symbol-overlay-face-7 (:underline (:color aqua)))
  (symbol-overlay-face-8 (:underline (:color yellow)))

  ;; hvm-mode
  (hvm-variable-face (:foreground fg :italic t))
  (hvm-function-face (:foreground fg-light :weight 'semi-bold))
  (hvm-constructor-face (:foreground sky))
  (hvm-number-face (:foreground orange))
  (hvm-char-face (:foreground blossom))
  (hvm-operator-face (:foreground rose))
  (hvm-symbols-face (:foreground ngrey-light))
  (hvm-supdup-face (:foreground fg))
  (hvm-super-face (:foreground wood :italic t))
  (hvm-datatype-face (:foreground water))
  (hvm-comment-face (:foreground fg-mid :italic t))
  (hvm-delimiters-face (:foreground ngrey-mid))
  (hvm-declaration-face (:foreground rose :weight 'bold))
  (hvm-definition-face (:foreground yellow))
  (hvm-control-face (:foreground aqua))

  ;; Rainbow delimiters
  (rainbow-delimiters-depth-1-face     (:foreground "#d5c4a1"))
  (rainbow-delimiters-depth-2-face     (:foreground "#bdae93"))
  (rainbow-delimiters-depth-3-face     (:foreground "#a89984"))
  (rainbow-delimiters-depth-4-face     (:foreground "#928374"))
  (rainbow-delimiters-depth-5-face     (:foreground "#7c6f64"))
  (rainbow-delimiters-depth-6-face     (:foreground "#665c54"))
  (rainbow-delimiters-depth-7-face     (:foreground "#504945"))
  (rainbow-delimiters-depth-8-face     (:foreground "#3c3836"))
  (rainbow-delimiters-depth-9-face     (:foreground "#32302f"))
  (rainbow-delimiters-unmatched-face   (:foreground "#fb4934" :bold t))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbones)
;;; gruvbones-theme.el ends here
