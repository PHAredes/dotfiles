;;; modules/email/config.el -*- lexical-binding: t; -*-
;; Add email-related configuration here.

(after! mu4e
  ;; Diretórios
  (setq mu4e-maildir "~/Mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-index-update-in-background t)
  
  ;; Visualização
  (setq mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-headers-date-format "%d/%m/%Y"
        mu4e-headers-time-format "%H:%M"
        mu4e-use-fancy-chars t
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:from-or-to . 25)
                              (:subject)))
  
  ;; Composição
  (setq mu4e-compose-format-flowed t
        mu4e-compose-dont-reply-to-self t
        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        mu4e-change-filenames-when-moving t
        mu4e-confirm-quit nil
        mu4e-headers-auto-update t
        mu4e-hide-index-messages t)
  
  ;; HTML rendering
  (setq mu4e-html2text-command "w3m -T text/html -dump")
  
  ;; Contextos - Zoho e Gmail separados
  (setq mu4e-contexts
        (list
         ;; Zoho (principal)
         (make-mu4e-context
          :name "Zoho"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Zoho" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "pedroaredes@zohomail.com")
                  (user-full-name . "Pedro Aredes")
                  (mu4e-sent-folder . "/Zoho/Sent")
                  (mu4e-drafts-folder . "/Zoho/Drafts")
                  (mu4e-trash-folder . "/Zoho/Trash")
                  (mu4e-refile-folder . "/Zoho/Archive")
                  (smtpmail-smtp-server . "smtp.zoho.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-user . "pedroaredes@zohomail.com")
                  (mu4e-compose-signature . "\n--\nPedro Aredes\npedroaredes@zohomail.com")))
         
         ;; Gmail (conta separada)
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "pedroaredes6@gmail.com")
                  (user-full-name . "Pedro Aredes")
                  (mu4e-sent-folder . "/Gmail/[Gmail]/E-mails enviados")
                  (mu4e-drafts-folder . "/Gmail/[Gmail]/Rascunhos")
                  (mu4e-trash-folder . "/Gmail/[Gmail]/Lixeira")
                  (mu4e-refile-folder . "/Gmail/[Gmail]/Todos os e-mails")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-user . "pedroaredes6@gmail.com")
                  (mu4e-compose-signature . "\n--\nPedro Aredes\npedroaredes6@gmail.com")))
         
         ;; Outlook (alias do Zoho)
         (make-mu4e-context
          :name "Outlook"
          :match-func (lambda (msg)
                        (when msg
                          (or (string-match-p "pedro.aredes@hotmail.com"
                                            (or (mu4e-message-field msg :to) ""))
                              (string-match-p "pedro.aredes@hotmail.com"
                                            (or (mu4e-message-field msg :from) "")))))
          :vars '((user-mail-address . "pedro.aredes@hotmail.com")
                  (user-full-name . "Pedro Aredes")
                  (mu4e-sent-folder . "/Zoho/Sent")
                  (mu4e-drafts-folder . "/Zoho/Drafts")
                  (mu4e-trash-folder . "/Zoho/Trash")
                  (mu4e-refile-folder . "/Zoho/Archive")
                  (smtpmail-smtp-server . "smtp.zoho.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-user . "pedroaredes@zohomail.com")
                  (mu4e-compose-signature . "\n--\nPedro Aredes\npedro.aredes@hotmail.com")))))
  
  ;; Atalhos rápidos (Maildirs)
  (setq mu4e-maildir-shortcuts
        '(("/Zoho/INBOX" . ?z)
          ("/Gmail/INBOX" . ?g)
          ("/Zoho/Sent" . ?s)
          ("/Gmail/[Gmail]/E-mails enviados" . ?S)
          ("/Gmail/[Gmail]/Todos os e-mails" . ?A)
          ("/Gmail/[Gmail]/Com estrela" . ?*)
          ("/Zoho/Archive" . ?a)
          ("/Zoho/Drafts" . ?d)))
  
  ;; Bookmarks (queries de busca)
  (setq mu4e-bookmarks
        '((:name "📬 Unread (All)" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "📨 Zoho Inbox" :query "maildir:/Zoho/INBOX" :key ?z)
          (:name "📧 Gmail Inbox" :query "maildir:/Gmail/INBOX" :key ?g)
          (:name "📤 Zoho Sent" :query "maildir:/Zoho/Sent" :key ?s)
          (:name "📮 Gmail Sent" :query "maildir:\"/Gmail/[Gmail]/E-mails enviados\"" :key ?S)
          (:name "📆 Today" :query "date:today..now" :key ?t)
          (:name "📅 Week" :query "date:7d..now" :hide-unread t :key ?w)
          (:name "⭐ Starred" :query "maildir:\"/Gmail/[Gmail]/Com estrela\"" :key ?*)
          (:name "🚩 Flagged" :query "flag:flagged" :key ?f)))
  
  ;; Keybindings estilo Emacs padrão
  (define-key mu4e-main-mode-map (kbd "C-c u") 'mu4e-update-mail-and-index)
  (define-key mu4e-main-mode-map (kbd "C-c c") 'mu4e-compose-new)
  (define-key mu4e-headers-mode-map (kbd "C-c c") 'mu4e-compose-new)
  (define-key mu4e-view-mode-map (kbd "C-c c") 'mu4e-compose-new))

(after! consult-mu
  ;;maximum number of results shown in minibuffer
  (setq consult-mu-maxnum 200)
  ;;show preview when pressing any keys
  (setq consult-mu-preview-key 'any)
  ;;do not mark email as read when previewed
  (setq consult-mu-mark-previewed-as-read nil)
  ;;do not mark email as read when selected. This is a good starting point to ensure you would not miss important emails marked as read by mistake especially when trying this package out. Later you can change this to t.
  (setq consult-mu-mark-viewed-as-read nil)
  ;; open the message in mu4e-view-buffer when selected.
  (setq consult-mu-action #'consult-mu--view-action))