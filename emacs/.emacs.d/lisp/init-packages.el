;;; init-packages.el --- Package management setup -*- lexical-binding: t; -*-

;; ============= Package Manager (straight.el) =============
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; A helper macro for configurations to be run after a package is loaded.
(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE is loaded."
  `(with-eval-after-load ',package ,@body))

(provide 'init-packages)
;;; init-packages.el ends here
