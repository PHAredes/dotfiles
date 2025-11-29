;;; modules/ai.el -*- lexical-binding: t; -*-

(use-package gptel
  :config
  (gptel-make-gh-copilot "Copilot"
    :models '(gpt-4.1
              gpt-5-mini
              gpt-4o))

  ;; Google Gemini (Generous Free Tier)
  (gptel-make-gemini "Gemini"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :host "generativelanguage.googleapis.com"
    :models '(gemini-3-pro-preview
              gemini-2.5-flash
              gemini-2.5-pro
              gemini-2.0-flash-exp
              gemini-2.5-flash-lite))

  ;; Ollama Cloud
  (defun my/ollama-cloud-auth-header ()
    "Return authorization header for Ollama Cloud."
    (when-let ((key (gptel--get-api-key)))
      `(("Authorization" . ,(concat "Bearer " key)))))

  (gptel-make-ollama "Ollama Cloud"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :host "ollama.com"
    :protocol "https"
    :endpoint "/api/chat"
    :header #'my/ollama-cloud-auth-header
    :models '(gpt-oss:20b
              gpt-oss:120b
              qwen3-coder:480b
              deepseek-v3.1:671b
              glm-4.6))
  
  (gptel-make-openai "GitHub"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :models '(phi-4
              phi-4-reasoning
              phi-4-mini-instruct
              phi-4-multimodal-instruct
              gpt-4o
              gpt-4o-mini
              meta-llama-3.1-405b-instruct
              mistral-large-2411))

  ;; defaults
  (setq gptel-default-mode 'markdown-mode
        gptel-model 'groq/compound
        gptel-include-reasoning nil
        gptel-backend  (gptel-make-openai "Groq"
                         :host "api.groq.com"
                         :endpoint "/openai/v1/chat/completions"
                         :stream t
                         :key 'gptel-api-key-from-auth-source
                         :models '(meta-llama/llama-4-maverick-17b-128e-instruct
                                   groq/compound-mini
                                   groq/compound
                                   openai/gpt-oss-120b
                                   openai/gpt-oss-20b
                                   llama-3.3-70b-versatile
                                   llama-3.1-8b-instant
                                   meta-llama/llama-4-scout-17b-16e-instruct)))


  ;; Custom directives
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (programming . "You are an expert programmer. Provide concise, efficient code with brief explanations.")
          (explain . "You are a teacher. Explain concepts clearly with examples.")
          (haskell . "You are a Haskell expert. Focus on functional programming, type safety, and idiomatic code.")
          (agda . "You are an expert in Agda and dependent types. Help with theorem proving and formal verification.")
          (emacs . "You are an Emacs Lisp expert. Provide idiomatic Elisp code following best practices.")
          (completion . "You provide only complete or refactored code without any commentary or explanation.")
          (concise . "You are a concise assistant. Give brief, direct answers without unnecessary explanation.")
          (autosolve . "You are a meta-language metavariable solver. Your logic follows these strict rules:

1. LANGUAGE INFERENCE: Detect the programming language based on syntax or explicit comments.
2. CONTEXT ANALYSIS: Analyze the provided code scope to identify available variables, functions, and types.
3. SOLVE: Replace the token '??' with the SMALLEST unit of code (variable name, literal, or function call) that satisfies the type checker and makes semantic sense in that scope.
4. OUTPUT FORMAT: You MUST wrap your answer in <S></S> tags. Output ONLY the replacement code inside these tags, nothing else. No explanations, no markdown, no context.

Example outputs:
<S>variableName</S>
<S>\"hello world\"</S>
<S>myFunction()</S>"))))

(use-package posframe)

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick")
  :after gptel
  :config
  (setq gptel-quick-use-context t))

;; ============= GPTel Fill (Auto-solve ?? holes) =============

(defun gptel--extract-solution (response)
  "Extract the solution from RESPONSE, handling various LLM output formats.
Looks for content between <S></S> tags, or falls back to heuristics."
  ;; Handle case where response is a cons cell like (reasoning . "text")
  (let* ((text (if (consp response)
                   (cdr response)
                 response))
         (cleaned (string-trim text)))
    (cond
     ;; Try to extract from <S>...</S> tags (find the LAST occurrence)
     ((string-match ".*<S>\\([^<]+\\)</S>" cleaned)
      (string-trim (match-string 1 cleaned)))
     
     ;; Fallback: remove markdown code blocks
     ((string-match "```[^\n]*\n\\(.*?\\)```" cleaned)
      (string-trim (match-string 1 cleaned)))
     
     ;; Fallback: if response is a single line without explanation, use it
     ((not (string-match-p "\n" cleaned))
      cleaned)
     
     ;; Fallback: take first line if it looks like code
     ((string-match "^[^.!?]*$" (car (split-string cleaned "\n")))
      (string-trim (car (split-string cleaned "\n"))))
     
     ;; Last resort: return as-is
     (t cleaned))))

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
              (when (looking-at "\\?\\?")
                (let ((solution (gptel--extract-solution response)))
                  (delete-region start-marker (+ start-marker 2))
                  (insert solution)
                  (message "Solved: %s" solution))))))))))

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

(provide 'ai)
