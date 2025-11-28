;;; modules/lang/typescript/packages.el -*- lexical-binding: t; -*-

(use-package typescript-mode
  :mode "\.tsx?\'"
  :custom (typescript-indent-level 2))

(use-package prettier-js
  :hook ((typescript-mode js-mode) . prettier-js-mode))

