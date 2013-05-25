;;; tomorrow-night-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Theme
;;
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Ported to GNU Emacs 24's built-in theme system by Jim Myhrberg (@jimeh)

;;; Code:

(deftheme tomorrow-night-modeline
  "A Pastel Coloured Theme")

(let ((background "#1d1f21")
      (current-line "#282a2e")
      (selection "#373b41")
      (foreground "#c5c8c6")
      (comment "#969896")
      (cursor "#aeafad")
      (dark-grey "#202023")
      (red "#cc6666")
      (dark-red "#933")
      (orange "#de935f")
      (yellow "#f0c674")
      (green "#b5bd68")
      (dark-green "#393")
      (aqua "#8abeb7")
      (blue "#81a2be")
      (purple "#b294bb"))

  (custom-theme-set-faces
   'tomorrow-night-modeline

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,current-line))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   ;; `(mode-line ((t (:background ,dark-green :foreground ,background))))
   `(region ((t (:background ,selection))))

   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))

   ;; magit-mode
   `(magit-diff-add ((t (:foreground ,dark-green))))
   `(magit-diff-del ((t (:foreground ,dark-red))))
   `(magit-item-highlight ((t (:background ,dark-grey))))
   ;; see magit.el
   ;; magit-header
   ;; magit-section-title
   ;; magit-branch
   ;; magit-diff-file-header
   ;; magit-diff-hunk-header
   ;; magit-diff-none
   ;; magit-log-graph
   ;; magit-log-sha1
   ;; magit-log-message
   ;; magit-item-mark
   ;; magit-log-tag-label
   ;; magit-log-head-label-bisect-good
   ;; magit-log-head-label-bisect-bad
   ;; magit-log-head-label-remote
   ;; magit-log-head-label-tags
   ;; magit-log-head-label-local

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground)))))

  (custom-theme-set-variables
   'tomorrow-night-modeline

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'tomorrow-night-modeline)

;;; tomorrow-night-modeline-theme.el ends here
