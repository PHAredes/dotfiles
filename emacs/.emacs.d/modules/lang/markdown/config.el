;;; modules/lang/markdown/config.el -*- lexical-binding: t; -*-

(defun markdown-preview-eww ()
  "Preview markdown in eww browser."
  (interactive)
  (let* ((tmp-file (make-temp-file "markdown-preview" nil ".html"))
         (output-buffer (find-file-noselect tmp-file)))
    (shell-command-on-region (point-min) (point-max)
                            (format "pandoc -f markdown -t html5 -s --mathjax -o %s" tmp-file))
    (eww-open-file tmp-file)))

(after! markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c w") 'markdown-preview-eww))
