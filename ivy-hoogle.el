(defun ivy-hoogle--do-search (&optional request-prefix)
  (let* ((pattern (or (and request-prefix
                           (concat request-prefix
                                   " "))))
         (short-pattern
          (if (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\) " pattern)
              (match-string 1 pattern)
            pattern))
         (lim 10)
         (args (append (list "search" "-l")
                       (and lim (list "-n" (int-to-string lim)))
                       (list short-pattern))))
    (let (candidates)
      (with-temp-buffer
        (apply #'call-process "hoogle" nil t nil args)
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "\\(.+?\\) -- \\(.+\\)")
              (push (propertize (match-string 1)
				'hoogle-url (match-string-no-properties 2))
                    candidates))
          (forward-line 1)))
      (nreverse candidates))))

(defun ivy-hoogle--fontify-haskell (src)
  (with-temp-buffer
    (insert-string src)
    (let ((delay-mode-hooks t))
      (haskell-mode)
      (font-lock-ensure))
    (buffer-string)))

(defun ivy-hoogle--function (str)
  (split-string (ivy-hoogle--fontify-haskell
		 (mapconcat #'identity (ivy-hoogle--do-search str) "\n"))
		"\n"))

(defun ivy-hoogle--function (str)
  (mapcar #'ivy-hoogle--fontify-haskell (ivy-hoogle--do-search str)))

(defun ivy-hoogle ()
  (interactive)
  (ivy-read "Hoogle: "
	    #'ivy-hoogle--function
	    :dynamic-collection t
	    :re-builder #'regexp-quote
	    :action (lambda (str)
		      (browse-url (get-text-property 0 'hoogle-url str)))))
