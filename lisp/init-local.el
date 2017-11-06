;;; code:
(setf ispell-program-name "/usr/local/bin/aspell")
(require 'ox-latex)

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

(pyenv-mode)
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

(defun latex-math-parenthesis-with-displaystype ()
  (interactive)
  (insert "\\(\\displaystyle \\)")
  (backward-char)
  (backward-char))

(provide 'init-local)
;;; init-local.el end here
