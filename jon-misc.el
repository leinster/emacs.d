(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (menu-bar-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode '(7 . 0)))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(server-start)
(recentf-mode 1)
(show-paren-mode 1)
(ansi-color-for-comint-mode-on)
(global-auto-revert-mode t)
(line-number-mode t)
(column-number-mode t)
(windmove-default-keybindings)
;; (global-whitespace-mode t)
(winner-mode t)

(setq-default locale-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      inhibit-startup-message nil
      inhibit-startup-screen nil
      mouse-yank-at-point t
      require-final-newline t
      ido-use-filename-at-point nil
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

(setq-default resize-mini-windows nil
              truncate-lines t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

(add-hook 'write-file-hooks
          'delete-trailing-whitespace)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when (functionp 'ido-mode)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        confirm-nonexistent-file-or-buffer nil))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;;; http://obfuscatedcode.wordpress.com/2007/04/26/configuring-emacs-for-gmails-smtp/
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(require 'smtpmail)

;;; jabber settings
(setq jabber-history-enable-rotation t
      jabber-history-enabled t
      jabber-use-global-history nil
      jabber-account-list '(("jonathon.ramsey@gmail.com"
                             (:network-server . "talk.google.com")
                             (:connection-type . ssl))))

(provide 'jon-misc)
