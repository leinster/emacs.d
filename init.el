;;; to open a new emacs with current init
;;;     $ open -n -a Emacs.app

(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode '(1 . 1))

(defconst *is-a-mac* (eq system-type 'darwin) "Is this a mac?")
(defconst jon-open (if *is-a-mac*
                       "open"
                     "xdg-open"))

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
 indicate-empty-lines t
 use-file-dialog nil
 use-dialog-box nil

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
 ido-show-dot-for-dired t
 ido-use-filename-at-point nil)
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

;;; isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'midnight)                     ; delete old buffers automatically

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
    bash-completion
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
    w3m
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

;;; pull in with-eval-after-load if not defined
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

;;; https://github.com/purcell/emacs.d/
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' for `MODE' for all `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defvar jon-coding-hook nil "Common")
(add-hook 'jon-coding-hook 'subword-mode)
(add-hook 'jon-coding-hook 'electric-pair-mode)
(defun jon-run-coding-hook ()
  (run-hooks 'jon-coding-hook))

(with-eval-after-load "markdown-mode-autoloads.el"
  (add-auto-mode 'gfm-mode
                 "[Tt][Oo][Dd][Oo]"
                 "\\.markdown\\'"
                 "\\.md\\'"))

(with-eval-after-load "mustache-mode-autoloads.el"
  (add-auto-mode 'mustache-mode "\\.mustache$"))

(with-eval-after-load "rainbow-delimiters-autoloads.el"
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(with-eval-after-load "smex-autoloads.el"
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(with-eval-after-load "window-number-autoloads.el"
  (require 'window-number)
  (window-number-mode t)
  (window-number-meta-mode t))

(with-eval-after-load "bash-completion-autoloads.el"
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete))

;; (with-eval-after-load "yasnippet-autoloads.el"
;;   (yas-global-mode 1))

;;; ----------------------------------------------------------------
;; auctex
(with-eval-after-load "auctex-autoloads.el"
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
  ;; luatex param is required for Skim integration
  (setq-default TeX-PDF-mode t
                TeX-engine 'luatex)
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-view-program-list
        `(("Skim"
           "/Applications/Skim.app/Contents/SharedSupport/displayline -background %n %o %b")))
  (setq TeX-engine-alist
        '((luatex "LuaTeX" "luatex" "lualatex --synctex=1 --jobname=%s" "luatex"))))

;;; ----------------------------------------------------------------
;; deft
(with-eval-after-load "deft-autoloads.el"
  (setq deft-extension "md"
        deft-text-mode 'gfm-mode
        deft-auto-save-interval 0))

;;; ----------------------------------------------------------------
;; set $PATH, $MANPATH, and exec-path from shell, mac only
(with-eval-after-load "exec-path-from-shell-autoloads.el"
  (when *is-a-mac*
    (progn
      (require 'exec-path-from-shell)
      (dolist (var '("SSH_AUTH_SOCK"
                     "SSH_AGENT_PID"
                     "GPG_AGENT_INFO"
                     "LANG"
                     "LANGUAGE"
                     "LC_ALL"
                     "CDPATH"
                     "HOSTNAME"))
        (add-to-list 'exec-path-from-shell-variables var))
      (exec-path-from-shell-initialize))))

;;; ----------------------------------------------------------------
;; emacs-lisp
(with-eval-after-load "paredit-autoloads.el"
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
(with-eval-after-load "js2-mode-autoloads.el"
  (add-auto-mode 'js2-mode "\\.js\\'")
  (setq-default js2-basic-offset 2
                js2-concat-multiline-strings 'eol
                js2-include-node-externs t
                js2-skip-preprocessor-directives t))

(with-eval-after-load "js2-refactor-autoloads.el"
  (require #'js2-refactor))
(with-eval-after-load 'js2-refactor
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(defun jon-js2-hook ()
  (set (make-local-variable 'compile-command)
       (concat "/Users/jon/dev/SE/scripts/hint "
               (file-name-nondirectory (buffer-file-name (current-buffer)))))
  (set (make-local-variable 'compilation-read-command) nil))
(add-hook 'js2-mode-hook 'jon-run-coding-hook)
(add-hook 'js2-mode-hook 'jon-js2-hook)

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'se-hint)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(se-hint "^\\(- \\)?\\([^:\n\" ]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
                         2              ; file path
                         3              ; line
                         4              ; column
                         )))

;;; ----------------------------------------------------------------
;; php
(with-eval-after-load "php-mode-autoloads.el"
  (add-auto-mode 'php-mode "\\.php\\'"))
(defun jon-php-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'php-mode-hook 'jon-run-coding-hook)
(add-hook 'php-mode-hook 'jon-php-mode-hook)

;;; ----------------------------------------------------------------
;; ruby
(add-auto-mode 'ruby-mode
               "\\.rake$"
               "\\.gemspec$"
               "\\.ru$"
               "\\.pill$"
               "\\.erb$"
               "Rakefile$"
               "Gemfile$")
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
(with-eval-after-load "web-mode-autoloads.el"
  (add-auto-mode 'web-mode
                 "\\.html$"
                 "\\.css$"))
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

(defvar jon-shell-buffer "*shell*"
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
    (progn
      (unless (buffer-name (get-buffer jon-shell-buffer))
        (setq jon-shell-buffer "*shell*"))
      (shell jon-shell-buffer))))

(defun jon-switch-to-vm-shell ()
  "Switch to shell buffer `#vm`."
  (interactive)
  (if (get-buffer "#vm")
      (switch-to-buffer "#vm")
    (progn
      (shell "#vm")
      (cd (expand-file-name "~/SE/"))
      (comint-simple-send (get-buffer "#vm") "cd ~/SE/")
      (comint-simple-send (get-buffer "#vm") "vm"))))

(defun jon-copy-filename-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "%s" (buffer-file-name)))

(defun jon-se-docs ()
  (interactive)
  (w3m-browse-url "http://se_www/doc"))

(defun jon-open-se-doc-src ()
  (interactive)
  (find-file-other-window
   (expand-file-name
    (concat
     "~/SE/www/content"
     (substring (cadr (split-string w3m-current-url "se_www")) 0 -1)
     ".markdown"))))

(defun jon-open-se-doc-page ()
  (interactive)
  (w3m-browse-url
   (concat
    "http://se_www/"
    (replace-regexp-in-string
     ".markdown"
     "/"
     (cadr (split-string (buffer-file-name) "/content/"))))))

(defun finder ()
  "Open current working directory in finder."
  (interactive)
  (shell-command
   (concat jon-open " "
           (shell-quote-argument
            (expand-file-name default-directory)))))

(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))
(global-set-key (kbd "C-z") 'maybe-suspend-frame)

;;; http://www.emacswiki.org/emacs/ModeLineDirtrack
(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification
               '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c d") 'deft)
(global-set-key (kbd "C-c f") 'finder)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c s") 'jon-switch-to-shell)
(global-set-key (kbd "C-c t") 'insert-timestamp)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'remember-notes)
(global-set-key (kbd "C-c u") 'browse-url-at-point)
(global-set-key (kbd "C-c v") 'jon-switch-to-vm-shell)
(global-set-key (kbd "C-c w") 'jon-copy-filename-to-kill-ring)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c +") 'calculator)
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
    (require 'ansi-color)
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

(when (not window-system)
  (menu-bar-mode -1))

(defun jon-font-inconsolata ()
  (interactive)
  (jon-set-font "Inconsolata" 135))

(defun jon-font-monaco ()
  (interactive)
  (jon-set-font "Monaco" 120))

(defun jon-font-source-code-pro ()
  (interactive)
  (jon-set-font "Source Code Pro" 120))

(defun jon-set-font (face height)
  (set-face-attribute
   'default
   nil
   :family face
   :height height
   :weight 'normal))

(when *is-a-mac*
  (progn
    (setq shell-file-name "/usr/local/bin/bash")
    (global-set-key (kbd "s-_") 'ns-do-hide-others)
    (when window-system
      (jon-font-monaco))
    (when (fboundp 'toggle-frame-fullscreen)
      (global-set-key (kbd "s-f") 'toggle-frame-fullscreen))))
