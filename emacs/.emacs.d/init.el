;;; init.el --- Modular Emacs Config -*- lexical-binding: t; -*-

;; ============= Performance =============
(setq gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 1024 1024)
      gc-cons-percentage 0.6)

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

;; ============= Module Loading System =============

;; A helper macro for configurations to be run after a package is loaded.
(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE is loaded."
  `(with-eval-after-load ',package ,@body))

;; Function to load a module's packages.el and config.el
(defun load-module! (module-path)
  "Load a module from a given path."
  (let* ((module-dir (expand-file-name module-path user-emacs-directory))
         (packages-file (expand-file-name "packages.el" module-dir))
         (config-file (expand-file-name "config.el" module-dir)))
    (when (file-exists-p packages-file)
      (load packages-file nil 'nomessage))
    (when (file-exists-p config-file)
      (load config-file nil 'nomessage))))

;; ============= Core Defaults & UI =============
;; These are loaded before other modules to ensure a consistent base.
(load-module! "modules/core")
(load-module! "modules/ui")

;; ============= Module Definitions =============
;; List of all modules to load.
(defvar enabled-modules
  '(
    ;; Core
    "editor"
    "completion"
    
    ;; Languages
    "lang/agda"
    "lang/go"
    "lang/haskell"
    "lang/markdown"
    "lang/typescript"

    ;; Tools
    "tools"
    "email"
    "ai/gptel"
    ))

;; =ad-hoc module loader
(defun load-modules (modules)
  "Load a list of modules."
  (dolist (module modules)
    (load-module! (concat "modules/" module))))

(load-modules enabled-modules)

;; ============= Environment & Session =============
;; This should run last to ensure all paths and settings are established.

;; Set PATH from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Environment variables
(let ((paths '("/home/pedro/.cabal/bin"
               "/home/pedro/.local/bin"
               "/home/pedro/.local/share/mise/shims")))
  (setenv "PATH" (concat (mapconcat 'identity paths ":") ":" (getenv "PATH")))
  (setq exec-path (append paths exec-path)))

;; Session management
(setq desktop-dirname (expand-file-name "desktop/" user-emacs-directory)
      desktop-path (list desktop-dirname)
      desktop-restore-eager 8
      save-place-file (expand-file-name "saveplace" user-emacs-directory))

(desktop-save-mode 1)


;; ============= Final Customizations =============
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