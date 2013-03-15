(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'deft)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-co" 'occur)
(global-set-key "\C-cr" 'ruby-debug)
(global-set-key "\C-cs" 'jon-switch-to-shell)
(global-set-key "\C-ct" 'insert-timestamp)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cu" 'browse-url-at-point)
(global-set-key "\C-cw" 'jon-copy-filename-to-kill-ring)
(global-set-key `[(control meta tab)] 'indent-rigidly)

(if (fboundp 'mc/mark-next-like-this)
    (progn
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;;; from starter-kit

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; File finding
(global-set-key (kbd "C-c y") 'bury-buffer)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

(provide 'jon-bindings)
