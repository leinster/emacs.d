(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin:/usr/local/share/npm/bin:/Users/jon/bin"))
  (add-to-list 'exec-path "/Users/jon/bin")
  (add-to-list 'exec-path "/usr/local/share/npm/bin")
  (add-to-list 'exec-path "/usr/texbin")

  (setenv "LANG" "en_GB.UTF-8")
  (setenv "LC_ALL" "en_GB.UTF-8")
  (setenv "LC_CTYPE" "en_GB.UTF-8")

  (defun mac-run-ruby ()
    (interactive)
    (run-ruby))
  (global-set-key (kbd "s-r") 'mac-run-ruby)

  (if (functionp 'ns-toggle-fullscreen)
      (global-set-key (kbd "<C-S-return>") 'ns-toggle-fullscreen)))

(provide 'jon-osx)
