;;; modules/ai.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun my/gptel-api-key-from-env (var)
  "Return a thunk that reads API key from env VAR."
  (lambda ()
    (or (getenv var)
        (user-error "Missing environment variable: %s" var))))

(defun my/gptel-api-key-from-env-or-auth-source (var)
  "Return a thunk that prefers env VAR, then auth-source lookup."
  (lambda ()
    (or (getenv var)
        (let ((key (ignore-errors (gptel-api-key-from-auth-source))))
          (and (stringp key) (not (string-empty-p key)) key))
        (user-error "Missing %s and no auth-source key found" var))))

(defconst +gptel-holefill-marker "??")
(defconst +gptel-holefill-token "{:FILL_HERE:}")
(defconst +gptel-holefill-system
  (mapconcat
   #'identity
   (list
    "You fill exactly one placeholder inside a user-provided file."
    ""
    "Rules:"
    "- The user sends the complete file text containing a single {:FILL_HERE:} marker."
    "- Inspect the surrounding text to understand the context (code, prose, question, etc.) and produce content that fits seamlessly."
    "- Preserve indentation, spacing, and style so the replacement feels native to the file."
    "- Unless the user explicitly asks you to rewrite the entire file, output only the text that should replace the placeholder."
    "- When asked to rewrite the entire file, emit the full file contents while keeping everything else identical apart from the requested changes."
    "- Wrap the replacement in a single <COMPLETION>...</COMPLETION> block with no commentary before or after the tags."
    "- The text inside <COMPLETION> should be exactly what replaces the placeholder (no fences, no marker tokens)."
    "- Never include {:FILL_HERE:} in your response and never output more than one <COMPLETION> block.")
   "\n"))

(use-package gptel
  :config
  (gptel-make-gh-copilot "Copilot"
    :models '(gpt-4.1
              gpt-4o
              gpt-5-mini
              gpt-5.1
              gpt-5.1-codex
              claude-haiku-4.5
              raptor-mini))

  ;; Google Gemini
  (gptel-make-gemini "Gemini"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :host "generativelanguage.googleapis.com"
    :models '(gemini-3-pro-preview
              gemini-3-flash-preview
              gemini-3.1-pro-preview
              gemini-2.5-pro
              gemini-2.5-flash
              gemini-2.5-flash-lite))

  ;; OpenCode Zen
  (gptel-make-openai "OpenCode"
    :stream t
    :key (my/gptel-api-key-from-env "OPENCODE_API_KEY")
    :host "opencode.ai"
    :endpoint "/zen/v1/chat/completions"
    :models '(minimax-m2.5
              minimax-m2.5-free
              minimax-m2.1
              minimax-m2.1-free
              glm-5
              glm-5-free
              glm-4.7
              glm-4.6
              kimi-k2.5
              kimi-k2.5-free
              kimi-k2
              kimi-k2-thinking
              trinity-large-preview-free
              big-pickle)))

  ;; Ollama Cloud
  (defun my/ollama-cloud-auth-header ()
    "Return authorization header for Ollama Cloud."
    (when-let ((key (gptel--get-api-key)))
      `(("Authorization" . ,(concat "Bearer " key)))))

  (gptel-make-ollama "Ollama Cloud"
    :stream t
    :key (my/gptel-api-key-from-env-or-auth-source "OLLAMA_API_KEY")
    :host "ollama.com"
    :protocol "https"
    :endpoint "/api/chat"
    :header #'my/ollama-cloud-auth-header
    :models '(glm-5
              glm-4.7
              deepseek-v3.2
              deepseek-v3.1:671b
              qwen3.5:397b
              qwen3-coder:480b
              qwen3-coder-next
              qwen3-vl:235b-instruct
              kimi-k2.5
              kimi-k2-thinking
              minimax-m2.5
              gpt-oss:120b
              gpt-oss:20b
              llama3.3:70b
              devstral-2:123b
              devstral-small-2:24b
              ministral-3:14b
              ministral-3:8b
              rnj-1:8b
              nemotron-3-nano:30b))

  (gptel-make-openai "iFlow"
    :stream t
    :key (my/gptel-api-key-from-env "IFLOW_API_KEY")
    :host "apis.iflow.cn"
    :endpoint "/v1/chat/completions"
    :models '(iflow-rome-30ba3b
              qwen3-coder-plus
              qwen3-max
              qwen3-vl-plus
              kimi-k2-0905
              qwen3-max-preview
              glm-4.6
              kimi-k2
              deepseek-v3.2
              deepseek-r1
              deepseek-v3
              qwen3-32b
              qwen3-235b-a22b-thinking-2507
              qwen3-235b-a22b-instruct
              qwen3-235b))

  (gptel-make-openai "OpenAI"
    :stream t
    :key (my/gptel-api-key-from-env "OPENAI_API_KEY")
    :host "api.openai.com"
    :endpoint "/v1/chat/completions"
    :models '(gpt-5.3-codex
              gpt-5.2-codex
              gpt-5.2
              gpt-5.2-chat-latest
              gpt-5.2-pro
              gpt-5.1-codex
              gpt-5.1-codex-mini
              gpt-5.1-codex-max
              gpt-5.1
              gpt-5.1-chat-latest
              gpt-5-codex
              gpt-5
              gpt-5-chat-latest
              gpt-5-mini
              gpt-5-nano
              gpt-4.1
              gpt-4.1-mini
              gpt-4.1-nano
              gpt-4o
              gpt-4o-mini
              o3
              o4-mini
              o3-mini))

  ;; defaults
  (setq gptel-default-mode 'markdown-mode
        gptel-model 'qwen3-coder-plus
        gptel-include-reasoning nil
        gptel-backend  (gptel-get-backend "iFlow"))

  (gptel-make-openai "Groq"
                         :host "api.groq.com"
                         :endpoint "/openai/v1/chat/completions"
                         :stream t
                         :key (my/gptel-api-key-from-env-or-auth-source "GROQ_API_KEY")
                         :models '(groq/compound
                                   groq/compound-mini
                                   llama-3.3-70b-versatile
                                   llama-3.1-8b-instant
                                   openai/gpt-oss-120b
                                   openai/gpt-oss-20b
                                   meta-llama/llama-4-maverick-17b-128e-instruct
                                   meta-llama/llama-4-scout-17b-16e-instruct
                                   deepseek-r1-llama-70b-specular))


  ;; Custom directives
  ;; (setq gptel-directives
  ;;       '((default . "You are a helpful assistant.")))

  (setf (alist-get 'holefill gptel-directives) +gptel-holefill-system)

(use-package posframe)

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick")
  :after gptel
  :config
  (setq gptel-quick-use-context t))

(use-package eca
  :straight (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :commands (eca
             eca-stop
             eca-restart
             eca-chat-toggle-window
             eca-rewrite
             eca-complete
             eca-completion-mode)
  :custom
  (eca-completion-idle-delay nil))

;; ============= GPTel Holefill (??) =============

(defun gptel--count-substring (needle haystack)
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun gptel--find-hole-bounds ()
  (save-excursion
    (or (when (search-backward +gptel-holefill-marker nil t)
          (cons (point) (+ (point) (length +gptel-holefill-marker))))
        (when (search-forward +gptel-holefill-marker nil t)
          (cons (- (point) (length +gptel-holefill-marker)) (point))))))

(defun gptel--find-first-hole-bounds ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward +gptel-holefill-marker nil t)
      (cons (- (point) (length +gptel-holefill-marker)) (point)))))

(defun gptel--strip-completion-tags (text)
  (let ((case-fold-search t))
    (replace-regexp-in-string "</?\\s-*completion\\s-*>" "" text t t)))

(defun gptel--extract-solution (response)
  (let* ((text (if (consp response) (cdr response) response))
         (cleaned (string-trim (format "%s" text)))
         candidate)
    (cond
     ((let ((case-fold-search t))
        (string-match "<\\s-*completion\\s-*>\\(\\(?:.\\|\\n\\)*?\\)</\\s-*completion\\s-*>" cleaned))
      (setq candidate (match-string 1 cleaned)))
     ((string-match ".*<S>\\([^<]+\\)</S>" cleaned)
      (setq candidate (string-trim (match-string 1 cleaned))))
     ((string-match "```[^\n]*\n\\(.*?\\)```" cleaned)
      (setq candidate (string-trim (match-string 1 cleaned))))
     ((not (string-match-p "\n" cleaned))
      (setq candidate cleaned))
     ((string-match "^[^.!?]*$" (car (split-string cleaned "\n")))
      (setq candidate (string-trim (car (split-string cleaned "\n")))))
     (t (setq candidate cleaned)))
    (replace-regexp-in-string
     "\\`[\n\r]+\\|[\n\r]+\\'"
     ""
     (gptel--strip-completion-tags candidate))))

(defun gptel-holefill-at-point ()
  "Fill all ?? holes in current buffer using holefill prompt rules."
  (interactive)
  (let* ((buf (current-buffer))
         (source (buffer-substring-no-properties (point-min) (point-max)))
         (total (gptel--count-substring +gptel-holefill-marker source))
         (done 0))
    (unless (> total 0)
      (user-error "No '??' hole found in buffer"))
    (cl-labels
        ((step ()
           (with-current-buffer buf
             (let ((hole (gptel--find-first-hole-bounds)))
               (if (null hole)
                   (message "Holefill complete (%d/%d)." done total)
                 (let* ((start-marker (copy-marker (car hole)))
                        (current (buffer-substring-no-properties (point-min) (point-max)))
                        (prompt (replace-regexp-in-string
                                 (regexp-quote +gptel-holefill-marker)
                                 +gptel-holefill-token
                                 current
                                 t
                                 t
                                 1)))
                   (message "Holefilling %d/%d..." (1+ done) total)
                   (gptel-request
                    prompt
                    :system +gptel-holefill-system
                    :stream nil
                    :callback
                    (lambda (response info)
                      (if (not response)
                          (message "GPTel error on hole %d/%d: %s"
                                   (1+ done)
                                   total
                                   (plist-get info :status))
                        (with-current-buffer buf
                          (save-excursion
                            (goto-char start-marker)
                            (when (looking-at (regexp-quote +gptel-holefill-marker))
                              (let ((solution (gptel--extract-solution response)))
                                (delete-region start-marker
                                               (+ start-marker
                                                  (length +gptel-holefill-marker)))
                                (insert solution)
                                (setq done (1+ done))
                                (step))))))))))))))
      (step))))

(defalias 'gptel-autosolve-at-point #'gptel-holefill-at-point)

;; ============= Evil Keybindings =============
(after! evil
  (evil-define-key 'normal 'global
    (kbd "<leader>a RET") 'gptel-send
    (kbd "<leader>am")    'gptel-menu
    (kbd "<leader>ab")    'gptel
    (kbd "<leader>ar")    'gptel-rewrite
    (kbd "<leader>aa")    'gptel-add
    (kbd "<leader>aq")    'gptel-quick
    (kbd "<leader>af")    'gptel-holefill-at-point
    (kbd "<leader>aE")    'eca
    (kbd "<leader>aW")    'eca-chat-toggle-window
    (kbd "<leader>aS")    'eca-stop
    (kbd "<leader>aR")    'eca-restart
    (kbd "<leader>ac")    'eca-complete
    (kbd "<leader>aw")    'eca-rewrite))

(provide 'ai)
