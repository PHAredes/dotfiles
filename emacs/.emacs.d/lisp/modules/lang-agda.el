;;; modules/lang-agda.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '(".lagda.md\'" . agda2-mode))

(with-eval-after-load 'agda2-mode
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (require 'agda-input)
  (evil-define-key 'normal agda2-mode-map 
    (kbd "<leader>d") 'agda2-goto-definition-keyboard))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))

(require 'agda-input)

(defconst +agda-input-method "Agda")

(setq default-input-method +agda-input-method)

(defun +agda-enable-input-method (&rest _)
  (unless (equal current-input-method +agda-input-method)
    (activate-input-method +agda-input-method)))

(defun +agda-enable-input-method-in-all-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (+agda-enable-input-method))))

(add-hook 'after-change-major-mode-hook #'+agda-enable-input-method)
(add-hook 'minibuffer-setup-hook #'+agda-enable-input-method)
(add-hook 'server-after-make-frame-hook #'+agda-enable-input-method)

(add-hook 'after-init-hook #'+agda-enable-input-method-in-all-buffers)

(+agda-enable-input-method-in-all-buffers)

(+agda-enable-input-method)

(provide 'lang-agda)
