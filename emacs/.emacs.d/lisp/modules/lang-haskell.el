;;; modules/lang-haskell.el -*- lexical-binding: t; -*-

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-indentation)
         (haskell-mode . lsp)))

(provide 'lang-haskell)
