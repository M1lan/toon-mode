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

(defvar toon-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Strings
    (modify-syntax-entry ?" "" table)
    (modify-syntax-entry ?\ \\ table)
    
    ;; Punctuation
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?[ "(]" table)
    (modify-syntax-entry ?] ")[ " table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){ " table)
    (modify-syntax-entry ?- "." table)
    
    table)
  "Syntax table for `toon-mode'.")

(defvar toon-font-lock-keywords
  (let ((kw-constants '("true" "false" "null")))
    (list
     ;; Array Headers: key[N] or [N]
     ;; Highlight the key before bracket
     `(,(concat "^\s-*\(\(?:[A-Za-z_][A-Za-z0-9_.]*\)\)\s-*\[")
       1 font-lock-variable-name-face)
     
     ;; Highlight the brackets and content [N<delim>]
     '("\\[\([0-9]+\)\(?:\(\t\|\|\)\)?\\]"
       (1 font-lock-constant-face)
       (2 font-lock-keyword-face nil t))
     
     ;; Highlight the fields list {a,b,c}
     '("{\([^}]*\)}"
       1 font-lock-string-face)

     ;; Object Keys (Unquoted)
     `(,(concat "^\s-*\(\(?:[A-Za-z_][A-Za-z0-9_.]*\)\):")
       1 font-lock-variable-name-face)
     
     ;; Object Keys (Quoted)
     '("^\s-*\(\"[^\"]+\"\):"
       1 font-lock-variable-name-face)
     
     ;; List markers
     '("^\s-*\(-\)\s-" 1 font-lock-keyword-face)
     
     ;; Constants
     `(,(concat "\_<" (regexp-opt kw-constants) "\_>")
       0 font-lock-constant-face)
     
     ))
  "Font lock keywords for `toon-mode'.")

;; Indentation Logic

(defun toon-calculate-indentation ()
  "Return the suggested indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        0
      (let ((current-line-is-empty (looking-at-p "^\s-*$")))
        (forward-line -1)
        ;; Skip empty lines to find previous non-empty line
        (while (and (not (bobp)) (looking-at-p "^\s-*$"))
          (forward-line -1))
        (if (looking-at-p "^\s-*$")
            0
          (let ((prev-indent (current-indentation))
                (prev-line-ends-colon (looking-at-p ".*:\s-*$")))
            (if prev-line-ends-colon
                (+ prev-indent toon-indent-offset)
              prev-indent)))))))

(defun toon-indent-line ()
  "Indent current line as TOON code."
  (interactive)
  (let ((indent (toon-calculate-indentation)))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (save-excursion
        (indent-line-to indent)))))

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
(add-to-list 'auto-mode-alist '("\.toon\'" . toon-mode))

(provide 'toon-mode)
;;; toon-mode.el ends here
