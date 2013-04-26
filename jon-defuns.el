;;; http://www.emacswiki.org/cgi-bin/wiki/http-post-simple.el
(defun jon-urlencode (str)
  "urlencode STR."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		       (format "%%%02x" c)))
                 str)))

(defun jon-search-mdc (thing)
  "Search MDC for THING."
  (interactive "sSearch MDC for: ")
  (browse-url (concat "http:///www.google.com/search?ie=UTF-8&q="
                      (jon-urlencode (concat "mdc " thing)))))

(defun jon-search-mdc-for-thing-at-point ()
  "Search MDC for thing at point."
  (interactive)
  (let ((thing (thing-at-point 'word)))
    (jon-search-mdc thing)))

(defun jon-rails-api ()
  "Browse the rails api v2.3.10"
  (interactive)
  (browse-url "http://api.rubyonrails.org/v2.3.10/"))

(defun jon-jquery-api ()
  "Browse the jQuery api"
  (interactive)
  (browse-url "http://jqapi.com"))

(defun jon-search-jquery-api (thing)
  "Search jQuery api for THING."
  (interactive "sSearch jQuery API for: ")
  (browse-url (concat "http://api.jquery.com/" (jon-urlencode thing))))

(defun jon-search-jquery-api-for-thing-at-point ()
  "Search jQuery API docs for thing at point."
  (interactive)
  (let ((thing (thing-at-point 'word)))
    (jon-search-jquery-api thing)))

(defun insert-timestamp ()
  (interactive)
  (insert (current-time-string)))

(defun colorize-rails-log ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun decolor-rails-log ()
  (interactive)
  (ansi-color-filter-region (point-min) (point-max)))

(defun loop-with-range (start stop function)
  "Loop over the range START..STOP and insert the result of
calling the lambda FUNCTION with the value as a string at each step.

e.g. calling with the lambda

  (lambda (i) (concat \"(\" i \"),\"))

results in output

\(1),
\(2),
\(3),
...
"
  (interactive "nStart: \nnStop: \nxLambda: ")
  (while (<= start stop)
    (insert (concat (funcall function (number-to-string start))
                    "\n"))
    (setq start (+ 1 start))))


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

(defun jon-hash-sorted-region (start end)
  "Return an md5 of the current region's words when sorted in a buffer."
  (interactive "r")
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf start end)
      (mark-whole-buffer)
      (replace-regexp "\[ \}\{,\]" "\n")
      (sort-lines nil (point-min) (point-max))
      ;; (write-region nil nil "/tmp/emacs.blah")
      (message (md5 (current-buffer))))))

(defun jon-copy-filename-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "%s" (buffer-file-name)))

(fset 'ruby-debug
   [tab ?r ?e ?q ?u ?i ?r ?e ?  ?\' ?r ?u ?b ?y ?- ?d ?e ?b ?u ?g ?\C-f ?\; ?  ?d ?e ?b ?u ?g ?g ?e ?r ?\C-j])

(provide 'jon-defuns)
