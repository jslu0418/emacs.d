;;; code:
;; solidity comment style
(load-file '"~/.emacs.d/additional/browser-refresh.el")
(require 'solidity-mode)
(setq solidity-comment-style 'star)
(setq solidity-flycheck-solc-checker-active t)
(setq solidity-flycheck-solium-checker-active t)
(setq flycheck-solidity-solc-addstd-contracts t)
(setf ispell-program-name "/usr/local/bin/aspell")
(require 'ox-latex)

(defun org-export-to-html-refresh-browser ()
  (interactive)
  (org-html-export-to-html)
  (browser-refresh)
  )

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(defun org-custom-latex-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;; (pyenv-mode)
"Activate a light color theme."
(setq custom-enabled-themes '(sanityinc-tomorrow-bright))
;;;(setq custom-enabled-themes '(sanityinc-solarized-light))
;;; The SBCL binary and command-line arguments
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
;;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;;; My function for adding a center framebox of math
(defun my-center-framebox-math ()
  (interactive)
  (insert "\\vspace{0.3cm}
\\begin{center}
\\framebox{\\(
\\)}
\\end{center}")
  (previous-line)
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command)
  (end-of-line)
  (insert "\n")
  (indent-for-tab-command)
  )

;;; My function for process latex file with bibliography in bibtex
(defun LaTex-BibTeX-LaTeX-LaTeX()
  (interactive)
  (if (buffer-modified-p)
      (progn
    ;;;(setq TeX-save-query nil)
        (TeX-save-document (TeX-master-file))
        (TeX-command "LaTeX" 'TeX-master-file -1)
        (TeX-run-BibTex nil t :help "Run BibTeX")
        (TeX-command "LaTeX" 'TeX-master-file -1)
        (TeX-command "LaTeX" 'TeX-master-file -1))
    (TeX-view))
  )

(defun latex-text-bold ()
  (interactive)
  (insert "\\textbf{}")
  (backward-char))

(defun latex-text-italic ()
  (interactive)
  (insert "\\textit{}")
  (backward-char))

(defun latex-text-verbatim ()
  (interactive)
  (insert "\\begin{verbatim}

\\end{verbatim}")
  (previous-line)
  (previous-line)
  (reindent-then-newline-and-indent)
  )

(defun latex-text-enumerate ()
  (interactive)
  (insert "\\begin{enumerate}
\\end{enumerate}")
  (previous-line)
  (reindent-then-newline-and-indent)
  (previous-line)
  )

(defun latex-algorithm ()
  (interactive)
  (insert "\\begin{minipage}{0.05\\textwidth}
    \\end{minipage}
    \\begin{minipage}{0.93\\textwidth}
    \\begin{algorithm}[H]
      \\caption{}
      \\begin{algorithmic}[1]
      \\end{algorithmic}
    \\end{algorithm}
    \\end{minipage}"))

(defun latex-homework-question()
  (interactive)
  (insert "\\begin{question}
    \\qnumber{}
    \\end{question}
")
  (previous-line)
  (previous-line)
  (previous-line)
  (beginning-of-line)
  (indent-for-tab-command)
  (next-line)
  (indent-for-tab-command)
  (next-line)
  (indent-for-tab-command)
  (previous-line)
  (end-of-line)
  (backward-char)
  )

(defun latex-homework-solution ()
  (interactive)
  (insert "\\begin{solution}

    \\end{solution}
")
  (previous-line)
  (previous-line)
  (previous-line)
  (beginning-of-line)
  (indent-for-tab-command)
  (next-line)
  (next-line)
  (indent-for-tab-command)
  (previous-line)
  )

(defun latex-math-parenthesis-with-displaystype ()
  (interactive)
  (insert "\\(\\displaystyle \\)")
  (backward-char)
  (backward-char))

(require 'xcscope)
(cscope-setup)

(setq org-publish-project-alist
      '(("Tirisfal Glades"
         :base-directory "~/Documents/web/"
         :base-extension "org"
         :recursive t
         :section-numbers nil
         :with-latex t
         :table-of-contents nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/public_html"
         :style "")
        ("imgs"
         :base-directory "~/Documents/web/imgs/"
         :base-extension "jpg\\|gif\\|png\\|jpeg"
         :publishing-directory "~/public_html/imgs"
         :publishing-function org-publish-attachment
         :recursive t)
        ("etc"
         :base-directory "~/Documents/web/"
         :base-extension "css\\|bib\\|el"
         :publishing-directory "~/public_html"
         :publishing-function org-publish-attachment)
        ("docs"
         :base-directory "~/Documents/web/docs/"
         :base-extension "html\\|tex\\|bib"
         :publishing-directory "~/public_html/docs"
         :publishing-function org-publish-attachment)
        ("notes"
         :base-directory "~/Documents/web/notes/"
         :base-extension "org"
         :publishing-directory "~/public_html/notes"
         :publishing-function org-latex-publish-to-pdf)
        ("thewholedamnshow" :components ("Tirisfal Glades" "imgs" "etc" "docs" "notes"))))
(defun rsync-web ()
  (interactive)
  (shell-command
   "rsync -a ~/public_html/* jxl173630@csgrads1.utdallas.edu:public_html/"))



(setenv "MAILHOST" "127.0.0.1:1110")

(setq rmail-primary-inbox-list '("jxl173630@utdallas.edu")
      rmail-pop-password-required t)

(setq user-mail-address	"jxl173630@utdallas.edu"
      user-full-name	"Jiashuai Lu")

;; ;; gnus+davmail bug, so I have to use pop3 for DavMail
;; ;; http://permalink.gmane.org/gmane.emacs.gnus.general/83301
;; ;; but delete all the mails on server is scary
(setq pop3-leave-mail-on-server t)


(setq gnus-secondary-select-methods '((nnml ""))
      mail-sources
      '((pop :server "127.0.0.1" ;; DavMail is running at localhost
             :port 1110
             :user "jxl173630@utdallas.edu"
             :password "Tina.1234567"
             :stream network))) ;; by default, DavMail don't encrypt mail

(setq send-mail-function		'smtpmail-send-it
      message-send-mail-function	'smtpmail-send-it
      smtpmail-smtp-server		"127.0.0.1"
      smtpmail-smtp-service             1025)

(defun offlineimap-get-password (host port)
  (require 'netrc)
  (let* ((netrc (netrc-parse (expand-file-name "~/.netrc.gpg")))
         (hostentry (netrc-machine netrc host port port)))
    (when hostentry (netrc-get hostentry "password"))))

;; (add-to-list load-path "~/.emacs.d/message-outlook")
;; (setq mail-user-agent 'message-user-agent)
;; (require 'message-outlook)

(provide 'init-local)
;;; init-local.el end here
