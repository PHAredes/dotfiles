;;; modules/lang-janet.el -*- lexical-binding: t; -*-

;;; modules/lang-janet.el -*- lexical-binding: t; -*-

;;; modules/lang-janet.el -*- lexical-binding: t; -*-

(use-package janet-mode
  :ensure t
  :mode "\\.janet\\'"
  :interpreter "janet"
  :bind (:map janet-mode-map
              ("C-c C-c" . janet-compile-file)
              ("C-c C-l" . janet-eval-buffer)
              ("C-c C-f" . janet-load-file)
              ("C-c C-r" . janet-run-repl)
              ("C-c C-e" . janet-eval-last-sexp)
              ("C-c C-z" . janet-switch-to-repl))
  :config
  (defvar janet-repl-buffer-name "*janet-repl*"
    "Name of the Janet REPL buffer.")

  (defun janet-compile-file ()
    "Compile current Janet file in compile-mode."
    (interactive)
    (save-buffer)
    (compile (format "janet %s" (shell-quote-argument (buffer-file-name)))))

  (defun janet-eval-buffer ()
    "Evaluate current Janet buffer and show output."
    (interactive)
    (save-buffer)
    (let ((output-buffer "*Janet Output*"))
      (shell-command 
       (format "janet %s" (shell-quote-argument (buffer-file-name)))
       output-buffer)
      (display-buffer output-buffer)))

  (defun janet-run-repl ()
    "Start Janet REPL."
    (interactive)
    (unless (comint-check-proc janet-repl-buffer-name)
      (make-comint-in-buffer "janet-repl" janet-repl-buffer-name "janet"))
    (pop-to-buffer janet-repl-buffer-name))

  (defun janet-load-file ()
    "Load current file into Janet REPL."
    (interactive)
    (save-buffer)
    (let ((filename (buffer-file-name)))
      (unless (comint-check-proc janet-repl-buffer-name)
        (janet-run-repl))
      (comint-send-string 
       janet-repl-buffer-name
       (format "(import \"%s\")\n" filename))
      (display-buffer janet-repl-buffer-name)))

  (defun janet-eval-last-sexp ()
    "Evaluate the expression before point in the REPL."
    (interactive)
    (let ((expr (buffer-substring-no-properties
                 (save-excursion (backward-sexp) (point))
                 (point))))
      (unless (comint-check-proc janet-repl-buffer-name)
        (janet-run-repl))
      (comint-send-string janet-repl-buffer-name (concat expr "\n"))
      (display-buffer janet-repl-buffer-name)))

  (defun janet-switch-to-repl ()
    "Switch to Janet REPL buffer."
    (interactive)
    (unless (comint-check-proc janet-repl-buffer-name)
      (janet-run-repl))
    (pop-to-buffer janet-repl-buffer-name))

  ;; Keybindings no REPL pra copiar resultado pro arquivo
  (defun janet-repl-copy-output ()
    "Copy last REPL output to kill ring."
    (interactive)
    (save-excursion
      (comint-previous-prompt 1)
      (let ((start (save-excursion (comint-next-prompt 1) (point)))
            (end (point-max)))
        (kill-new (string-trim (buffer-substring-no-properties start end)))
        (message "Output copied to kill ring!"))))

  (add-hook 'inferior-janet-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-y") 'janet-repl-copy-output))))

(provide 'lang-janet)
