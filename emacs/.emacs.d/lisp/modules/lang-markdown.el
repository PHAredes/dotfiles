;;; modules/lang-markdown.el -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ((".md\'" . markdown-mode)
         (".markdown\'" . markdown-mode)
         ("README\.\(?:md\|markdown\)\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . (lambda () 
                           (setq-local line-spacing 0.2)
                           (setq-local left-margin-width 2
                                      right-margin-width 2)
                           (set-window-buffer nil (current-buffer)))))
  :custom
  ;; Preview & rendering
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup nil)
  
  ;; Links & references
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  (markdown-footnote-location 'end)
  
  ;; Indentation & lists
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-list-indent-width 2)
  (markdown-unordered-list-item-prefix "- ")
  
  ;; Headers
  (markdown-asymmetric-header t)  ; # Header instead of ### Header ###
  (markdown-header-scaling t)     ; Scale header fonts
  (markdown-header-scaling-values '(1.4 1.3 1.2 1.1 1.0 1.0))
  
  ;; Code blocks
  (markdown-code-block-braces t)
  (markdown-gfm-use-electric-backquote t)
  
  ;; Behavior
  (markdown-split-window-direction 'right)
  (markdown-live-preview-delete-export 'delete-on-export)
  (markdown-display-remote-images t)
  
  
  ;; Better GFM (GitHub Flavored Markdown) support
  (add-to-list 'auto-mode-alist '("README\.md\'" . gfm-mode))
  
  ;; Optional: auto-pair for markdown emphasis
  (add-hook 'markdown-mode-hook
            (lambda () 
              (setq-local electric-pair-pairs 
                          (append electric-pair-pairs '((?* . ?*)
                                                       (?_ . ?_)
                                                       (?` . ?`))))))
  :config
  (define-key markdown-mode-map (kbd "C-c C-c w") 'markdown-preview-eww)
  )

(use-package markdown-toc
  :after markdown-mode
  :commands markdown-toc-generate-toc
)

(defun markdown-preview-eww ()
  "Preview markdown in eww browser."
  (interactive)
  (let* ((tmp-file (make-temp-file "markdown-preview" nil ".html"))
         (output-buffer (find-file-noselect tmp-file)))
    (shell-command-on-region (point-min) (point-max)
                            (format "pandoc -f markdown -t html5 -s --mathjax -o %s" tmp-file))
    (eww-open-file tmp-file)))

(provide 'lang-markdown)
