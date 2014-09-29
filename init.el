;;; to open a new emacs with current init
;;;     $ open -n -a Emacs.app

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)

(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

(setq-default locale-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load "server")
(unless (server-running-p) (server-start))

(tooltip-mode -1)
(show-paren-mode t)

(setq
 inhibit-startup-screen t
 user-full-name "Jonathon Ramsey"
 user-mail-address "jonathon.ramsey@gmail.com"
 mail-default-reply-to "jonathon.ramsey@gmail.com"
 backup-directory-alist (quote (("." . "~/.emacs.d/backups")))

 mouse-yank-at-point t
 global-auto-revert-mode t)

(setq-default
 column-number-mode t
 indent-tabs-mode nil
 ;; truncate-lines t
 visible-bell t
 show-paren-style (quote parenthesis))

(setq-default
 ido-mode (quote both)
 ido-create-new-buffer (quote prompt)
 ido-enable-flex-matching t
 ido-show-dot-for-dired t)
(ido-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(add-hook 'write-file-hooks
          'delete-trailing-whitespace)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
(add-to-list 'hippie-expand-try-functions-list
             'try-complete-file-name-partially t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar jon-required-packages
  '(auctex
    csv-mode
    deft
    exec-path-from-shell
    fill-column-indicator
    flycheck
    haml-mode
    heroku-theme
    inf-ruby
    json-mode
    js2-mode
    js2-refactor
    less-css-mode
    magit
    markdown-mode
    markdown-mode+
    mustache-mode
    paredit
    php-mode
    rainbow-delimiters
    rainbow-mode
    ruby-end
    ruby-tools
    smex
    sublime-themes
    web-mode
    window-number
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "Required packages.")

(defun jon-install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun jon-install-packages ()
  "Install required packages."
  (interactive)
  (package-refresh-contents)
  (mapc 'jon-install-package jon-required-packages))

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun jon-add-to-auto-mode-alist (regex-list mode)
  (dolist (regex regex-list)
    (add-to-list 'auto-mode-alist (cons regex mode))))

(defvar jon-coding-hook nil "Common")
(add-hook 'jon-coding-hook 'subword-mode)
(add-hook 'jon-coding-hook 'electric-pair-mode)
(defun jon-run-coding-hook ()
  (run-hooks 'jon-coding-hook))

(after 'markdown-mode-autoloads
       (jon-add-to-auto-mode-alist '("[Tt][Oo][Dd][Oo]"
                                     "\\.markdown\\'"
                                     "\\.md\\'")
                                   'gfm-mode))
(after 'mustache-mode-autoloads
       (jon-add-to-auto-mode-alist '("\\.mustache$")
                                   'mustache-mode))
(after 'rainbow-delimiters-autoloads
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(after 'smex-autoloads
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))
(after 'window-number-autoloads
       (require 'window-number)
       (window-number-mode t)
       (window-number-meta-mode t))
(after 'yasnippet-autoloads
       (yas-global-mode 1))

;;; ----------------------------------------------------------------
;; auctex
(after 'auctex-autoloads
  (setq-default TeX-PDF-mode t
                TeX-engine 'luatex)
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))
        TeX-view-program-list '(("PDF Viewer" "open %o"))))

;;; ----------------------------------------------------------------
;; deft
(after 'deft-autoloads
  (setq deft-extension "md"
        deft-text-mode 'gfm-mode))

;;; ----------------------------------------------------------------
;; set $PATH, $MANPATH, and exec-path from shell, mac only
(after 'exec-path-from-shell-autoloads
  (when (memq window-system '(mac ns))
    (progn
      (require 'exec-path-from-shell)
      (dolist (var '("SSH_AUTH_SOCK"
                     "SSH_AGENT_PID"
                     "GPG_AGENT_INFO"
                     "LANG"
                     "LC_CTYPE"))
        (add-to-list 'exec-path-from-shell-variables var))
      (exec-path-from-shell-initialize))))

;;; ----------------------------------------------------------------
;; emacs-lisp
(after 'paredit-autoloads
  (defun jon-emacs-lisp-mode-paredit-hook ()
    (require 'paredit)
    (enable-paredit-mode))
  (add-hook 'emacs-lisp-mode-hook 'jon-emacs-lisp-mode-paredit-hook))

;;; ----------------------------------------------------------------
;;; flyspell
(defun jon-flyspell-hook ()
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast")
        ispell-list-command "list"))
(add-hook 'flyspell-mode-hook 'jon-flyspell-hook)

;;; ----------------------------------------------------------------
;; haml
(add-hook 'haml-mode-hook 'jon-run-coding-hook)
(add-hook 'haml-mode-hook 'jon-run-coding-hook)

;;; ----------------------------------------------------------------
;; json
(defun prettify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region
     b e "/Users/jon/bin/prettify-json" (current-buffer) t)))

(defun jon-json-hook ()
  (local-set-key "\C-xp" 'prettify-json))
(add-hook 'json-mode-hook 'jon-run-coding-hook)
(add-hook 'json-mode-hook 'jon-json-hook)

;;; ----------------------------------------------------------------
;; javascript
(after 'js2-mode-autoloads
       (jon-add-to-auto-mode-alist '("\\.js\\'") 'js2-mode)
       (setq-default js2-basic-offset 2
                     js2-concat-multiline-strings 'eol
                     js2-include-node-externs t
                     js2-skip-preprocessor-directives t))

(after 'js2-refactor-autoloads
  (require #'js2-refactor))
(after 'js2-refactor
       (js2r-add-keybindings-with-prefix "C-c C-r"))

(defun jon-js2-hook ()
  (set (make-local-variable 'compile-command)
       (concat "/Users/jon/dev/SE/scripts/hint "
               (file-name-nondirectory (buffer-file-name (current-buffer)))))
  (set (make-local-variable 'compilation-read-command) nil))
(add-hook 'js2-mode-hook 'jon-run-coding-hook)
(add-hook 'js2-mode-hook 'jon-js2-hook)

(after 'compile
       (add-to-list 'compilation-error-regexp-alist 'se-hint)
       (add-to-list 'compilation-error-regexp-alist-alist
                    '(se-hint "^\\(- \\)?\\([^:\n\" ]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
                              2              ; file path
                              3              ; line
                              4              ; column
                              )))

;;; ----------------------------------------------------------------
;; php
(after 'php-mode-autoloads
       (jon-add-to-auto-mode-alist '("\\.php\\'")
                                   'php-mode))
(defun jon-php-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'php-mode-hook 'jon-run-coding-hook)
(add-hook 'php-mode-hook 'jon-php-mode-hook)

;;; ----------------------------------------------------------------
;; ruby
(jon-add-to-auto-mode-alist '("\\.rake$"
                              "\\.gemspec$"
                              "\\.ru$"
                              "\\.pill$"
                              "\\.erb$"
                              "Rakefile$"
                              "Gemfile$")
                            'ruby-mode)
(add-hook 'ruby-mode-hook 'jon-run-coding-hook)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

;;; ----------------------------------------------------------------
;; shell mode
(add-hook 'shell-mode-hook 'jon-run-coding-hook)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'visual-line-mode)

;;; ----------------------------------------------------------------
;; shell scripts
(defun jon-sh-mode-hook ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'jon-run-coding-hook)
(add-hook 'sh-mode-hook 'jon-sh-mode-hook)

;;; ----------------------------------------------------------------
;; sql
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

;;; ----------------------------------------------------------------
;; web mode
(after 'web-mode-autoloads
       (jon-add-to-auto-mode-alist '("\\.html$"
                                     "\\.css$")
                                   'web-mode))
(defun jon-web-mode-hook ()
  (rainbow-mode))
(add-hook 'web-mode-hook 'jon-web-mode-hook)
(add-hook 'web-mode-hook 'jon-run-coding-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defuns
(defun jon-search-mdn (thing)
  "Search MDN for THING."
  (interactive "sSearch MDN for: ")
  (browse-url (concat "http:///www.google.com/search?ie=UTF-8&q="
                      (jon-urlencode (concat "mdn " thing)))))

(defun jon-search-mdn-for-thing-at-point ()
  "Search MDN for thing at point."
  (interactive)
  (let ((thing (thing-at-point 'word)))
    (jon-search-mdn thing)))

(defun insert-timestamp ()
  (interactive)
  (insert
   (format-time-string "%-e %b %Y")))

(defvar jon-shell-buffer nil
  "*Buffer to jump to with `jon-switch-to-shell'")

(defun jon-switch-to-shell ()
  "Switch to the shell buffer `jon-shell-buffer' or to `*shell*'.
With a prefix-argument jump to a shell buffer by name, creating a
new shell if required, and set `jon-shell-buffer'."
  (interactive)
  (if current-prefix-arg
      (setq jon-shell-buffer
            (let ((current-prefix-arg '(4)))
              (call-interactively 'shell)))
    (shell jon-shell-buffer)))

(defun jon-switch-to-vm-shell ()
  "Switch to shell buffer `#vm`."
  (interactive)
  (shell "#vm"))

(defun jon-copy-filename-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "%s" (buffer-file-name)))

(defun finder ()
  "Open current working directory in finder."
  (interactive)
  (let ((cmd (if (eq system-type 'darwin)
                 "open"
               "xdg-open")))
    (shell-command (concat cmd " "
                           (shell-quote-argument
                            (expand-file-name default-directory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'deft)
(global-set-key "\C-cf" 'finder)
(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-co" 'occur)
(global-set-key "\C-cs" 'jon-switch-to-shell)
(global-set-key "\C-ct" 'insert-timestamp)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cu" 'browse-url-at-point)
(global-set-key "\C-cv" 'jon-switch-to-vm-shell)
(global-set-key "\C-cw" 'jon-copy-filename-to-kill-ring)
(global-set-key "\C-c+" 'calculator)
(global-set-key `[(control meta tab)] 'indent-rigidly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; desktop
(defun my-desktop-save ()
  (interactive)
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(defun jon-use-desktop ()
  (add-hook 'auto-save-hook 'my-desktop-save)
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode 1))

(setq desktop-restore-eager 5
      desktop-dirname dotfiles-dir
      desktop-globals-to-save (append '((extended-command-history . 100)
                                        (file-name-history        . 100)
                                        (grep-history             . 100)
                                        (compile-history          . 30)
                                        (minibuffer-history       . 50)
                                        (query-replace-history    . 60)
                                        (read-expression-history  . 60)
                                        (regexp-history           . 60)
                                        (regexp-search-ring       . 20)
                                        (search-ring              . 20)
                                        (shell-command-history    . 500)
                                        tags-file-name
                                        register-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; system specific
(when window-system
  (progn
    (jon-use-desktop)
    (load-theme 'whiteboard)
    (setq ansi-color-names-vector
          ["black"
           "firebrick"
           "DarkGreen"
           "DarkGoldenrod3"
           "navy"
           "DarkOrchid"
           "SteelBlue"
           "white"])
    (setq ansi-color-map (ansi-color-make-color-map))))
