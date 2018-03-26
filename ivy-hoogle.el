(require 'ivy)
(require 's)

(defun ivy-hoogle--do-search (&optional request-prefix)
  (let* ((pattern (or (and request-prefix
                           (concat request-prefix
                                   " "))))
         (short-pattern
          (if (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\) " pattern)
              (match-string 1 pattern)
            pattern))
         (lim 25)
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
  (mapcar #'ivy-hoogle--fontify-haskell (ivy-hoogle--do-search str)))

(defvar ivy-hoogle-history nil)

(defun ivy-hoogle ()
  (interactive)
  (ivy-read "Hoogle: "
	    #'ivy-hoogle--function
	    :dynamic-collection t
	    :history ivy-hoogle-history
	    :preselect (counsel-symbol-at-point)
	    :re-builder #'regexp-quote
	    :action (lambda (str)
		      (browse-url (get-text-property 0 'hoogle-url str)))))

(global-set-key (kbd "C-z h") #'ivy-hoogle)

(defun lowercasep (c) (and (wordp c) (= c (downcase c))))
(defun uppercasep (c) (and (wordp c) (= c (upcase c))))
(defun whitespacep (c) (= 32 (char-syntax c)))

(defun ivy-hoogle--parse-module ()
  (parsec-collect-s
   (parsec-satisfy #'uppercasep)
   (parsec-many-till-s (parsec-any-ch)
		       (parsec-try
			(parsec-or (parsec-ch ?.)
				   (parsec-ch ? )
				   (parsec-eob))))))

(defun ivy-hoogle--parse-module-path ()
  (parsec-many (ivy-hoogle--parse-module)))

(defun ivy-hoogle--parse-module-symbol ()
  (parsec-collect
   (ivy-hoogle--parse-module-path)
   (parsec-many-till-s (parsec-any-ch)
		       (parsec-or (parsec-ch ? )
				  (parsec-eob)))))

(defun ivy-hoogle--colour-module-path (mod-path)
    (propertize (s-join "." mod-path) 'face 'haskell-liquid-haskell-annotation-face))

(defvar teststr "Control.Lens.Getter view :: MonadReader s m => Getting a s a -> m a")

(defun test ()
  (interactive)
  (let 
      ((p (parsec-with-input teststr
	    (ivy-hoogle--parse-module-symbol))))
    (message "%s" (ivy-hoogle--colour-module-path (car p)))))

(replace-regexp-in-string "\\([A-Z][a-z]*\\)" (propertize "\\1" 'font-lock-face 'haskell-type-face) "MonadReader s m => Getting a s a -> m a")
