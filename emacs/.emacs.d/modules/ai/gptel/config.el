;;; modules/ai/gptel/config.el -*- lexical-binding: t; -*-

;; ============= GPTel Fill (Auto-solve ?? holes) =============
(defun gptel-autosolve-at-point ()
  "Find '??' token, send surrounding context to LLM, and replace '??' with solution."
  (interactive)
  (let* (;; 1. Find the '??' token
         (target-bounds
          (save-excursion
            (unless (search-backward "??" (point-min) t)
              (user-error "No '??' token found in buffer"))
            (cons (point) (+ (point) 2))))
         (beg (car target-bounds))
         (end (cdr target-bounds))
         (start-marker (copy-marker beg))
         
         ;; 2. Determine context (region if active, otherwise buffer up to ??)
         (context-text
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) beg))) 
         
         ;; 3. Build the prompt with context embedded
         (prompt (format "The code context is below. Solve the hole (??) found in this snippet:\n\n%s\n\n??"
                        context-text)))
    
    (message "Autosolving...")
    
    ;; 4. Send request to gptel
    (gptel-request prompt
      :system (alist-get 'autosolve gptel-directives)
      :stream nil
      :callback
      (lambda (response info)
        (if (not response)
            (message "GPTel error: %s" (plist-get info :status))
          (with-current-buffer (marker-buffer start-marker)
            (save-excursion
              (goto-char start-marker)
              (when (looking-at "\\?\?") ; Note: escaped 
                (delete-region start-marker (+ start-marker 2))
                (insert (string-trim response))
                (message "Solved: %s" (string-trim response))))))))))

;; ============= Evil Keybindings =============
(after! evil
  (evil-define-key 'normal 'global
    (kbd "<leader>a RET") 'gptel-send
    (kbd "<leader>am")    'gptel-menu
    (kbd "<leader>ab")    'gptel
    (kbd "<leader>ar")    'gptel-rewrite
    (kbd "<leader>aa")    'gptel-add
    (kbd "<leader>aq")    'gptel-quick
    (kbd "<leader>af")    'gptel-autosolve-at-point))
