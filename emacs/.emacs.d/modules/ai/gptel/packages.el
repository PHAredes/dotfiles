;;; modules/ai/gptel/packages.el -*- lexical-binding: t; -*-

(use-package gptel
  :config
  (gptel-make-gh-copilot "Copilot"
    :models '(gpt-4.1
              gpt-5-mini
              gpt-4o))

  ;; Google Gemini (Generous Free Tier)
  (gptel-make-gemini "Gemini"
    :stream t
    :key "AIzaSyA6r7JUoeSzYEC_UZ-Rm8hPt44eZMFRUJ8"
    :host "generativelanguage.googleapis.com"
    :models '(gemini-3-pro-preview
              gemini-2.5-flash
              gemini-2.5-pro
              gemini-2.0-flash-exp
              gemini-2.5-flash-lite))

  ;; Ollama Cloud
  (gptel-make-openai "OllamaCloud"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :host "api.ollama.com"
    :endpoint "/api/v1/chat/completions"
    :models '(gpt-oss:120b-cloud
              gpt-oss:20b-cloud
              qwen3-coder:480b-cloud
              deepseek-v3.1:671b-cloud
              glm-4.6:cloud))

  
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

1. LANGUAGE INFERENCE: Detect the programming language based on syntax or explicit comments (e.g., '/* LANGUAGE = Java 25 */').
2. CONTEXT ANALYSIS: Analyze the provided code scope to identify available variables, functions, and types.
3. SOLVE: Replace the token '??' with the SMALLEST unit of code (variable name, literal, or function call) that satisfies the type checker and makes semantic sense in that scope.
4. OUTPUT FORMAT: Output ONLY the replacement code text. Do not include markdown backticks, explanations, or the original surrounding code. If the solution is a string, output the string with quotes. If it is a variable, output just the variable name."))))

(use-package posframe)

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick")
  :after gptel
  :config
  (setq gptel-quick-use-context t))
