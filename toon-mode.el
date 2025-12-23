;;; toon-mode.el --- Major mode for editing TOON files -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 1.0.0
;; Keywords: data, languages
;; URL: https://github.com/toon-format/spec

;;; Commentary:

;; This mode provides syntax highlighting and indentation for TOON (Token-Oriented Object Notation) files.
;; Based on TOON Specification v3.0.

;;; Code:

(defgroup toon nil
  "Major mode for editing TOON files."
  :prefix "toon-"
  :group 'languages)

(defcustom toon-indent-offset 2
  "Number of spaces for each indentation step in `toon-mode'."
  :type 'integer
  :group 'toon)

(defcustom toon-cli-command "toon"
  "Executable name or path for the TOON CLI."
  :type 'string
  :group 'toon)

(defconst toon--identifier-rx "[A-Za-z_][A-Za-z0-9_.]*")

(defvar toon-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    
    ;; Punctuation
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\[ "(" table)
    (modify-syntax-entry ?\] ")" table)
    (modify-syntax-entry ?\{ "(" table)
    (modify-syntax-entry ?\} ")" table)
    (modify-syntax-entry ?- "." table)
    
    table)
  "Syntax table for `toon-mode'.")

(defvar toon-font-lock-keywords
  (let ((kw-constants '("true" "false" "null")))
    (list
     ;; Array header key
     `(,(concat "^\\s-*\\(" toon--identifier-rx "\\)\\s-*\\[")
       1 font-lock-variable-name-face)
     ;; Bracketed length and delimiter
     '("\\[\\([0-9]+\\)\\(?:\\(\\t\\|\\|\\)\\)?\\]"
       (1 font-lock-constant-face)
       (2 font-lock-keyword-face nil t))
     ;; Field list {a,b,c}
     '("{\\([^}]*\\)}" 1 font-lock-string-face)
     ;; Object keys (unquoted)
     `(,(concat "^\\s-*\\(" toon--identifier-rx "\\)\\s-*:") 
       1 font-lock-variable-name-face)
     ;; Object keys (quoted)
     '("^\\s-*\\(\"[^\"]+\"\\)\\s-*:"
       1 font-lock-variable-name-face)
     ;; List marker
     '("^\\s-*\\(-\\)\\s-+" 1 font-lock-keyword-face)
     ;; Numbers
     '("\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\_>"
       0 font-lock-constant-face)
     ;; Constants
     `(,(concat "\\_<" (regexp-opt kw-constants) "\\_>")
       0 font-lock-constant-face)))
  "Font lock keywords for `toon-mode'.")

;; Indentation Logic

(defun toon--line-opens-block-p (line)
  "Return non-nil if LINE opens a nested block."
  (string-match-p ":\\s-*$" line))

(defun toon-calculate-indentation ()
  "Return the suggested indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        0
      (forward-line -1)
      ;; Skip empty lines to find previous non-empty line
      (while (and (not (bobp)) (looking-at-p "^\\s-*$"))
        (forward-line -1))
      (if (looking-at-p "^\\s-*$")
          0
        (let ((prev-indent (current-indentation))
              (prev-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
          (if (toon--line-opens-block-p prev-line)
              (+ prev-indent toon-indent-offset)
            prev-indent))))))

(defun toon-indent-line ()
  "Indent current line as TOON code."
  (interactive)
  (let ((indent (toon-calculate-indentation)))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (save-excursion
        (indent-line-to indent)))))

(defun toon--ensure-cli ()
  "Return the TOON CLI path, or signal a user error."
  (or (executable-find toon-cli-command)
      (user-error "TOON CLI not found. Customize `toon-cli-command'")))

(defun toon--call-cli (input &rest args)
  "Run the TOON CLI with ARGS on INPUT and return stdout as a string."
  (let ((program (toon--ensure-cli)))
    (with-temp-buffer
      (insert input)
      (let ((exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              program
                              t t nil
                              args)))
        (if (zerop exit-code)
            (buffer-string)
          (error "TOON CLI failed (%s)" exit-code))))))

(defun toon-convert-buffer-to-json (&optional replace)
  "Convert current buffer from TOON to JSON using the TOON CLI.
With prefix arg REPLACE, replace buffer contents."
  (interactive "P")
  (let ((json-text (toon--call-cli (buffer-string) "--decode")))
    (if replace
        (progn
          (erase-buffer)
          (insert json-text))
      (let ((buf (get-buffer-create "*TOON JSON*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert json-text)
          (when (fboundp 'json-mode)
            (json-mode)))
        (display-buffer buf)))))

;;;###autoload
(define-derived-mode toon-mode prog-mode "TOON"
  "Major mode for editing TOON files."
  :syntax-table toon-mode-syntax-table
  (setq-local font-lock-defaults '(toon-font-lock-keywords))
  (setq-local indent-line-function 'toon-indent-line)
  (setq-local indent-tabs-mode nil) ; TOON strictly forbids tabs for indentation
  (setq-local comment-start nil) ; No comments in TOON
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toon\\'" . toon-mode))

(provide 'toon-mode)
;;; toon-mode.el ends here
