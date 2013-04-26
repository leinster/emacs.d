(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name mustache-mode
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))))
        (:name markdown-mode
               :after (progn
                        (setq markdown-command "~/SE/scripts/markitup")))
        (:name deft
               :after (progn
                        (setq deft-extension "md"
                              deft-text-mode 'markdown-mode)))
        (:name grep-edit
               :type http
               :url "http://emacswiki.org/emacs/download/grep-edit.el"
               :features "grep-edit")
        (:name multiple-cursors
               :type elpa
               :features "multiple-cursors")
        (:name less-css-mode
               :type elpa
               :features "less-css-mode")
        (:name csv-mode
               :type elpa
               :after (progn
                        (autoload 'csv-mode "csv-mode" "Major mode for editing CSV files" t)))
        ))

(el-get-install "csv-mode")
(el-get-install "autopair")
(el-get-install "idle-highlight-mode")
(el-get-install "rvm")
(el-get-install "ruby-mode")
(el-get-install "ruby-compilation")
(el-get-install "ruby-test-mode")
(el-get-install "ri-emacs")
(el-get-install "rinari")
(el-get-install "inf-ruby")
(el-get-install "haml-mode")
(el-get-install "sass-mode")
(el-get-install "yaml-mode")
(el-get-install "magit")
(el-get-install "magithub")
(el-get-install "markdown-mode")
(el-get-install "emacs-jabber")
(el-get-install "mustache-mode")
(el-get-install "coffee-mode")
(el-get-install "haskell-mode")
(el-get-install "deft")
(el-get-install "rainbow-mode")
(el-get-install "grep-edit")
(el-get-install "multiple-cursors")
(el-get-install "less-css-mode")
(el-get-install "paredit")
(el-get-install "php-mode")
(el-get-install "go-mode")

;; install is broken
;; (el-get-install "emacs-goodies-el")

(el-get 'sync)

;;; ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pill$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

(defun jon-ruby-hook ()
  (esk-run-coding-hook)
  (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
               (lambda (_ _) nil))
  (paredit-mode)
  (subword-mode))
(add-hook 'ruby-mode-hook 'jon-ruby-hook)

;;; javascript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; matches jshint output
(add-to-list 'compilation-error-regexp-alist
             '("^\\(- \\)?\\([^:\n\" ]+\\): line \\([0-9]+\\), col \\([0-9]+\\)" 2 3 4))

(defun jon-js-hook ()
  (esk-run-coding-hook)
  (autopair-mode)
  (subword-mode)
  (define-key js-mode-map (kbd ",") 'self-insert-command)
  (local-set-key "\C-xq" 'jon-search-mdc-for-thing-at-point)
  (local-set-key "\C-xj" 'jon-search-jquery-api-for-thing-at-point)
  (local-set-key "\C-xp" 'prettify-json)
  (setq js-indent-level 2)
  (set (make-local-variable 'compile-command)
       (concat "/Users/jon/dev/SE/scripts/hint "
               (file-name-nondirectory (buffer-file-name (current-buffer)))))
  (set (make-local-variable 'compilation-read-command) nil))
(add-hook 'js-mode-hook 'jon-js-hook)
(add-hook 'js-mode-hook 'jon-greek-lambda)

(eval-after-load 'js
  '(progn
     (defun js-insert-and-indent (key)
       (interactive (list (this-command-keys)))
       (call-interactively (lookup-key (current-global-map) key)))
     (defun prettify-json ()
       (interactive)
       (let ((b (if mark-active (min (point) (mark)) (point-min)))
             (e (if mark-active (max (point) (mark)) (point-max))))
         (shell-command-on-region
          b e "python -mjson.tool" (current-buffer) t)))))

;;; haml-mode
(defun jon-haml-mode-hook ()
  "haml-mode-hook"
  (autopair-mode)
  (visual-line-mode))
(add-hook 'haml-mode-hook 'jon-haml-mode-hook)

;;; coffee-mode
(defun jon-coffee-mode-hook ()
  "coffee-mode hook"
  (set (make-local-variable 'tab-width) 2)
  (coffee-cos-mode t)
  (setq coffee-js-mode 'js-mode)
  (autopair-mode))
(add-hook 'coffee-mode-hook 'jon-coffee-mode-hook)

;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(defun jon-php-hook ()
  (setq c-basic-offset 4)
  (esk-run-coding-hook)
  (autopair-mode)
  (subword-mode))
(add-hook 'php-mode-hook 'jon-php-hook)

;;; css, sass, less
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

(defun jon-css-hook ()
  (setq css-indent-offset 2)
  (esk-run-coding-hook)
  (autopair-mode)
  (subword-mode)
  (rainbow-mode))
(add-hook 'css-mode-hook 'jon-css-hook)

;;; shell
(defun jon-sh-hook ()
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2)
  (subword-mode))
(add-hook 'sh-mode-hook 'jon-sh-hook)

;;; haskell
(defun jon-haskell-hook ()
    (autopair-mode))
(add-hook 'haskell-mode-hook 'jon-haskell-hook)

;;; magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;;; markdown-mode for todos
(add-to-list 'auto-mode-alist '("[Tt][Oo][Dd][Oo]" . markdown-mode))

;;; ssh
(autoload 'ssh "ssh" "SSH" t)

;;; markdown-mode
(defun jon-markdown-hook ()
  (longlines-mode))
(add-hook 'markdown-mode-hook 'jon-markdown-hook)

;;; flyspell
(defun jon-flyspell-hook ()
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast")
        ispell-list-command "list"))
(add-hook 'flyspell-mode-hook 'jon-flyspell-hook)

;;; sql
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

;;; windmove (shift-arrow to switch windows)
(require 'windmove)
(windmove-default-keybindings)

;;; go-mode
(defun jon-go-mode-hook ()
  (setq tab-width 4))
(add-hook 'go-mode-hook 'jon-go-mode-hook)

(provide 'jon-packages)
