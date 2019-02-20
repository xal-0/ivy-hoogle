;;; ivy-hoogle.el --- hoogle search for ivy

;; Copyright (C) 2018 Sam Schweigel

;; Author: Sam Schweigel <s.schweigel@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((ivy "0.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a syntax-coloured hoogle search through ivy.

;;; Code:

(require 'ivy)
(require 's)

(defun ivy-hoogle--do-search (&optional request-prefix)
  (let* ((pattern (or (and request-prefix
                           (concat request-prefix
                                   " "))))
         (lim 100)
         (args (append (list "search" "-l")
                       (and lim (list "-n" (int-to-string lim)))
                       (list pattern))))
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
    (setq case-fold-search nil)
    (insert src)
    (goto-char (point-min))
    (when (search-forward-regexp "\\(\\(?:[A-Z]\\w*\.?\\)+\\)" nil 'noerror)
      (replace-match (propertize (match-string 1) 'face 'haskell-type-face)))
    (when (search-forward-regexp "\\(\\w*\\)" nil 'noerror)
      (replace-match (propertize (match-string 1) 'face 'haskell-definition-face)))
    (while (search-forward-regexp "\\([A-Z]\\w*\\)" nil 'noerror)
      (let ((ref (match-string 1)))
	(replace-match (propertize ref 'face 'haskell-type-face))))
    (goto-char (point-min))
    (while (search-forward-regexp "\\([!#$%&*+./<=>?@\\^|-~:]\\)" nil 'noerror)
      (let ((ref (match-string 1)))
	(replace-match (propertize ref 'face 'haskell-operator-face))))
    (goto-char (point-min))
    (while (search-forward-regexp "\\(case\\|class\\|data\\|default\\|deriving\\|do\\|else\\|if\\|import\\|in\\|infix\\|infixl\\|infixr\\|instance\\|let\\|module\\|mdo\\|newtype\\|of\\|rec\\|pattern\\|proc\\|then\\|type\\|where\\|_\\|package\\)\\s-+" nil 'noerror)
      (let ((ref (match-string 0)))
	(replace-match (propertize ref 'face 'haskell-keyword-face))))
    (buffer-string)))

(defun ivy-hoogle--function (str)
  (mapcar #'ivy-hoogle--fontify-haskell (ivy-hoogle--do-search str)))

(defvar ivy-hoogle-history nil)

;;;###autoload
(defun ivy-hoogle ()
  "Perform a hoogle search."
  (interactive)
  (ivy-read "Hoogle: "
	    #'ivy-hoogle--function
	    :dynamic-collection t
	    :history ivy-hoogle-history
	    :preselect (counsel-symbol-at-point)
	    :re-builder #'regexp-quote
	    :action (lambda (str)
		      (browse-url (get-text-property 0 'hoogle-url str)))))

(provide 'ivy-hoogle)
;;; ivy-hoogle.el ends here
