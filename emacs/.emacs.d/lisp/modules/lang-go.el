;;; modules/lang-go.el -*- lexical-binding: t; -*-

(use-package go-mode
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save))
  :custom
  (gofmt-command "goimports")
  :config
  (add-hook 'go-mode-hook
    (lambda () 
      (setq-local compile-command "go build")))
  (evil-define-key 'normal go-mode-map
    (kbd "<leader>r")  'my/go-run-tui
    (kbd "<leader>cb") 'compile
    (kbd "<leader>cr") 'recompile
    (kbd "<leader>ct") (lambda () (interactive) 
                         (let ((compile-command "go test ./..."))
                           (compile compile-command)))
    (kbd "<leader>tt") 'go-test-current-test
    (kbd "<leader>tf") 'go-test-current-file
    (kbd "<leader>tp") 'go-test-current-project))

(use-package gotest
  :after go-mode)

(defun my/go-run-tui ()
  "Run Go TUI in eat terminal."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root))
    (if (get-buffer "*go-tui*")
        (kill-buffer "*go-tui*"))
    (let ((eat-buffer (eat-other-window)))
      (rename-buffer "*go-tui*")
      (eat-term-send-string eat-terminal "go run .\n"))))

(provide 'lang-go)
