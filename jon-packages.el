(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; https://github.com/zenspider/package
(unless (package-installed-p 'package+)
  (package-install 'package+))

(package-manifest
 'csv-mode
 'deft
 'fill-column-indicator
 'haml-mode
 'heroku-theme
 'inf-ruby
 'jabber
 'less-css-mode
 'magit
 'markdown-mode
 'markdown-mode+
 'mustache-mode
 'package+
 'paredit
 'php-mode
 'ruby-compilation
 'ruby-end
 'ruby-test-mode
 'ruby-tools
 'sublime-themes
 'web-mode
 'window-number
 'yaml-mode
 'yasnippet
 'zenburn-theme
 )

(eval-after-load "markdown-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("[Tt][Oo][Dd][Oo]" . gfm-mode))
     (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
     (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))))

(eval-after-load "mustache-mode"
  '(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode)))

(eval-after-load "web-mode"
  '(add-to-list 'auto-mode-alist '("\\.html$" . web-mode)))

(require 'window-number)
(window-number-mode t)

;;; packages I might want to reinstall
;;; emacs-goodies - unavailable?
;;; coffee-mode
;;; go-mode
;;; haskell-mode
;;; idle-highlight-mode
;;; sass-mode
;;; rvm
;;; rainbow-mode
;;; (yas-global-mode 1) - yasnippet config

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
  (electric-pair-mode)
  (ruby-end-mode t)
  (ruby-tools-mode t)
  (subword-mode)
  (set (make-local-variable 'compile-command)
       (concat "rubocop -es "
               (file-name-nondirectory (buffer-file-name (current-buffer))))))
(add-hook 'ruby-mode-hook 'jon-ruby-hook)

;;; javascript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; matches jshint output
;; (add-to-list 'compilation-error-regexp-alist
;;              '("^\\(- \\)?\\([^:\n\" ]+\\): line \\([0-9]+\\), col \\([0-9]+\\)" 2 3 4))

(defun jon-js-hook ()
  (esk-run-coding-hook)
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
;; (remove-hook 'js-mode-hook 'jon-greek-lambda)

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
  (electric-pair-mode)
  (visual-line-mode))
(add-hook 'haml-mode-hook 'jon-haml-mode-hook)

;;; coffee-mode
(defun jon-coffee-mode-hook ()
  "coffee-mode hook"
  (set (make-local-variable 'tab-width) 2)
  (coffee-cos-mode t)
  (setq coffee-js-mode 'js-mode)
  (electric-pair-mode))
(add-hook 'coffee-mode-hook 'jon-coffee-mode-hook)

;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(defun jon-php-hook ()
  (setq c-basic-offset 4)
  (esk-run-coding-hook)
  (electric-pair-mode)
  (subword-mode))
(add-hook 'php-mode-hook 'jon-php-hook)

;;; web-mode
(defun jon-web-mode-hook ()
  (setq web-mode-code-indent-offset 4)
  (subword-mode)
  (electric-pair-mode -1))
(add-hook 'web-mode-hook 'jon-web-mode-hook)

;;; css, sass, less
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

(defun jon-css-hook ()
  (setq css-indent-offset 2)
  (esk-run-coding-hook)
  (electric-pair-mode)
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
    (electric-pair-mode)
    (turn-on-haskell-indentation))
(add-hook 'haskell-mode-hook 'jon-haskell-hook)
(push '("\\.hs$" . haskell-mode)  auto-mode-alist)
;; (assoc "\\.hs$" auto-mode-alist)

;;; ssh
(autoload 'ssh "ssh" "SSH" t)

;;; flyspell
(defun jon-flyspell-hook ()
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast")
        ispell-list-command "list"))
(add-hook 'flyspell-mode-hook 'jon-flyspell-hook)

;;; sql
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

;;; go-mode
(defun jon-go-mode-hook ()
  (setq tab-width 4))
(add-hook 'go-mode-hook 'jon-go-mode-hook)

(provide 'jon-packages)
