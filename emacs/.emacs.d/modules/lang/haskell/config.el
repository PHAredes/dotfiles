;;; modules/lang/haskell/config.el -*- lexical-binding: t; -*-

(after! haskell-mode
  (add-hook 'haskell-mode-hook #'lsp))
