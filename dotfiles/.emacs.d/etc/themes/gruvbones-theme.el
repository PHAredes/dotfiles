;;; package: --- A pleasant theme with earthy shades, inspired by Gruvbox.
;;; Commentary:
;;; Code:
(require 'autothemer)

(autothemer-deftheme
 gruvbones "Inspired by Gruvbox tones with shades of neutral grey"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs, although it is fine for most modern temu

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
  (purple          "#b16286")
  (aqua            "#8ec07c")
  )

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
  (font-lock-builtin-face              (:foreground orange))
  (font-lock-constant-face             (:foreground blossom))
  (font-lock-comment-face              (:foreground ngrey-mid))
  (font-lock-comment-delimiter-face    (:foreground ngrey-mid))
  (font-lock-doc-markup-face           (:foreground ngrey-mid))
  (font-lock-string-face               (:foreground fg-mid :italic t))
  (font-lock-keyword-face              (:foreground rose :bold t))
  (font-lock-function-name-face        (:foreground fg-dark))
  (font-lock-variable-name-face        (:foreground water))
  (font-lock-regexp-grouping-backslash (:foreground water))
  (font-lock-negation-char-face        (:foreground rose))

  ;; lsp-ui
  (lsp-ui-sideline-current-symbol      (:inherit 'border :bold t))
  (lsp-ui-sideline-symbol              (:foreground gray :box (:line-width -1 :color bg-light)))
  (lsp-ui-sideline-symbol-info         (:italic t))

  ;; flycheck
  (flycheck-posframe-warning-face      (:foreground leaf))
  (flycheck-fringe-warning             (:foreground leaf))
  (flycheck-fringe-error               (:foreground rose))

  ;; org-mode
  (org-block                           (:background bg-dark))
  (org-link                            (:foreground water :bold t :underline t))
  (org-block-begin-line                (:inherit 'shadow :height 0.9))
  (org-block-end-line	               (:inherit 'org-block-begin-line))
  (org-date                            (:foreground water :underline t :bold t))
  (org-level-1                         (:foreground rose :bold t))
  (org-level-2                         (:foreground water :bold t))

  ;; ivy/ido
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

  ;; symbol-mode
  (symbol-overlay-default-face (:underline t))
  (symbol-overlay-face-1 (:underline (:color leaf)))
  (symbol-overlay-face-2 (:underline (:color wood)))
  (symbol-overlay-face-3 (:underline (:color rose)))
  (symbol-overlay-face-4 (:underline (:color sky)))
  (symbol-overlay-face-5 (:underline (:color orange)))
  (symbol-overlay-face-6 (:underline (:color blossom)))
  (symbol-overlay-face-7 (:underline (:color aqua)))
  (symbol-overlay-face-8 (:underline (:color yellow)))
 ))
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbones)
;;; gruvbones.el ends here
