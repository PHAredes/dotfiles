;;; modules/lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-lens-enable nil)
  (lsp-completion-provider :none))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(provide 'lsp)
