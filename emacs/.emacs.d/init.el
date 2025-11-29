;;; init.el --- Entry point for the Emacs configuration -*- lexical-binding: t; -*-

;; Add the `lisp` directory to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load core configuration and package setup
(require 'init-packages)
(require 'init-core)

;; Load all modules from the `lisp/modules` directory
(let ((modules-dir (expand-file-name "lisp/modules" user-emacs-directory)))
  (mapc #'load-file (directory-files-recursively modules-dir "\.el$")))

;; Anything in here will be executed last.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6d57b77f5406b26562bffc214f3a79f987b813fe5c010a08f02b2873be682e11"
     "9088b7fb8084ecd7bdd69c94a01bf336de611bd12dc44c5940ff6de0d941e3cd"
     "59cf109707de43c2d055e6ec4ce1fc37b40d87b5fdedd9d3e26d9c451116fa3a" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
