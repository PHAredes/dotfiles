;;; modules/lang/go/packages.el -*- lexical-binding: t; -*-

(use-package go-mode
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save))
  :custom
  (gofmt-command "goimports"))

(use-package gotest
  :after go-mode)
