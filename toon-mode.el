;;; toon-mode.el --- Major mode for editing TOON files -*- lexical-binding: t; -*-

;; Author: Milan Santosi
;; Version: 1.0.0
;; Keywords: data, languages
;; URL: https://github.com/m1lan/toon-mode

;;; Commentary:

;; This mode provides syntax highlighting and indentation for TOON
;; (Token-Oriented Object Notation) files, based on TOON Specification v3.0.

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

(defun toon--read-file-string (path)
  "Return PATH contents as a string, or an empty string if missing."
  (if (and path (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    ""))

(defun toon--display-cli-error (program args exit-code stdout stderr)
  "Show a buffer with CLI failure details."
  (let ((buf (get-buffer-create "*TOON CLI Error*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Program: " program "\n")
      (insert "Args: " (mapconcat #'identity args " ") "\n")
      (insert "Exit: " (format "%s" exit-code) "\n\n")
      (when (and (stringp stderr) (> (length stderr) 0))
        (insert "stderr:\n" stderr "\n"))
      (when (and (stringp stdout) (> (length stdout) 0))
        (insert "stdout:\n" stdout "\n"))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun toon--string-empty-p (str)
  "Return non-nil if STR is empty."
  (or (null str) (string= str "")))

(defun toon--string-trim-left (str)
  "Trim leading whitespace from STR."
  (if (toon--string-empty-p str)
      ""
    (replace-regexp-in-string "\\`[ \t]+" "" str)))

(defun toon--string-trim-right (str)
  "Trim trailing whitespace from STR."
  (if (toon--string-empty-p str)
      ""
    (replace-regexp-in-string "[ \t]+\\'" "" str)))

(defun toon--string-trim (str)
  "Trim leading and trailing whitespace from STR."
  (toon--string-trim-right (toon--string-trim-left str)))

(defun toon--count-leading-spaces (line)
  "Return cons of (spaces . rest) for LINE; error on tab indentation."
  (let ((i 0)
        (len (length line))
        (spaces 0))
    (while (< i len)
      (let ((ch (aref line i)))
        (cond
         ((= ch ?\s)
          (setq spaces (1+ spaces))
          (setq i (1+ i)))
         ((= ch ?\t)
          (user-error "Tabs are not allowed for indentation"))
         (t
          (setq i len)))))
    (cons spaces (substring line spaces))))

(defun toon--scan-quoted (s i)
  "Return index after closing quote starting at I in S, or nil."
  (let ((len (length s))
        (j (1+ i))
        (escaped nil)
        (end nil))
    (while (and (< j len) (null end))
      (let ((ch (aref s j)))
        (cond
         ((and (not escaped) (= ch ?\\))
          (setq escaped t))
         ((and (not escaped) (= ch ?\"))
          (setq end (1+ j)))
         (t
          (setq escaped nil))))
      (setq j (1+ j)))
    end))

(defun toon--find-unquoted-char (s ch)
  "Find CH in S ignoring quoted regions. Return index or nil."
  (let ((i 0)
        (len (length s))
        (pos nil))
    (while (< i len)
      (let ((c (aref s i)))
        (cond
         ((= c ?\")
          (let ((end (toon--scan-quoted s i)))
            (setq i (if end end len))))
         ((= c ch)
          (setq pos i)
          (setq i len))
         (t
          (setq i (1+ i))))))
    pos))

(defun toon--identifier-start-p (ch)
  "Return non-nil if CH can start an unquoted key."
  (or (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?a) (<= ch ?z))
      (= ch ?_)))

(defun toon--identifier-char-p (ch)
  "Return non-nil if CH can appear in an unquoted key."
  (or (toon--identifier-start-p ch)
      (and (>= ch ?0) (<= ch ?9))
      (= ch ?.)
      (= ch ?_)))

(defun toon--parse-key-token (s i)
  "Parse a key token from S starting at I.
Return (TOKEN . END) or nil."
  (let ((len (length s)))
    (when (< i len)
      (let ((ch (aref s i)))
        (cond
         ((= ch ?\")
          (let ((end (toon--scan-quoted s i)))
            (when end
              (cons (substring s i end) end))))
         ((toon--identifier-start-p ch)
          (let ((j (1+ i)))
            (while (and (< j len)
                        (toon--identifier-char-p (aref s j)))
              (setq j (1+ j)))
            (cons (substring s i j) j)))
         (t nil))))))

(defun toon--skip-ws (s i)
  "Skip spaces and tabs in S from I. Return new index."
  (let ((len (length s))
        (j i))
    (while (and (< j len)
                (let ((ch (aref s j)))
                  (or (= ch ?\s) (= ch ?\t))))
      (setq j (1+ j)))
    j))

(defun toon--skip-spaces (s i)
  "Skip spaces in S from I. Return new index."
  (let ((len (length s))
        (j i))
    (while (and (< j len) (= (aref s j) ?\s))
      (setq j (1+ j)))
    j))

(defun toon--parse-bracket-seg (s i)
  "Parse bracket segment at I. Return plist or nil."
  (let ((len (length s)))
    (when (and (< i len) (= (aref s i) ?\[))
      (let ((j (1+ i)))
        (when (and (< j len) (>= (aref s j) ?0) (<= (aref s j) ?9))
          (while (and (< j len)
                      (>= (aref s j) ?0) (<= (aref s j) ?9))
            (setq j (1+ j)))
          (let ((digits (substring s (1+ i) j))
                (delim ?,))
            (when (< j len)
              (let ((ch (aref s j)))
                (when (or (= ch ?\t) (= ch ?|))
                  (setq delim ch)
                  (setq j (1+ j)))))
            (when (and (< j len) (= (aref s j) ?\]))
              (list :digits digits :delim delim :end (1+ j)))))))))

(defun toon--parse-fields-seg (s i delim)
  "Parse fields segment at I using DELIM. Return plist or nil."
  (let ((len (length s)))
    (when (and (< i len) (= (aref s i) ?\{))
      (let ((j (1+ i))
            (fields '()))
        (let ((skip (if (= delim ?\t) #'toon--skip-spaces #'toon--skip-ws)))
          (setq j (funcall skip s j))
          (let ((res (toon--parse-key-token s j)))
            (unless res (setq j nil))
            (when res
              (setq fields (list (car res)))
              (setq j (cdr res))))
          (when j
            (setq j (funcall skip s j))
            (while (and (< j len) (= (aref s j) delim))
              (setq j (1+ j))
              (setq j (funcall skip s j))
              (let ((res (toon--parse-key-token s j)))
                (unless res (setq j nil))
                (when res
                  (setq fields (append fields (list (car res))))
                  (setq j (cdr res))))
              (when j
                (setq j (funcall skip s j)))))
          (when (and j (< j len) (= (aref s j) ?\}))
            (list :fields fields :end (1+ j))))))))

(defun toon--parse-header (s)
  "Parse a header line from S. Return plist or nil."
  (let ((len (length s))
        (i 0)
        key
        bracket
        fields)
    (setq i (toon--skip-ws s i))
    (catch 'fail
      (if (and (< i len) (= (aref s i) ?\[))
          (setq key nil)
        (let ((res (toon--parse-key-token s i)))
          (unless res (throw 'fail nil))
          (setq key (car res))
          (setq i (cdr res))))
      (setq i (toon--skip-ws s i))
      (setq bracket (toon--parse-bracket-seg s i))
      (unless bracket (throw 'fail nil))
      (setq i (plist-get bracket :end))
      (setq i (toon--skip-ws s i))
      (when (and (< i len) (= (aref s i) ?\{))
        (setq fields (toon--parse-fields-seg s i (plist-get bracket :delim)))
        (unless fields (throw 'fail nil))
        (setq i (plist-get fields :end))
        (setq i (toon--skip-ws s i)))
      (unless (and (< i len) (= (aref s i) ?:)) (throw 'fail nil))
      (let ((rest (substring s (1+ i))))
        (list :key key
              :digits (plist-get bracket :digits)
              :delim (plist-get bracket :delim)
              :fields (and fields (plist-get fields :fields))
              :rest rest)))))

(defun toon--build-header (hdr)
  "Build a canonical header string from HDR plist."
  (let* ((key (plist-get hdr :key))
         (digits (plist-get hdr :digits))
         (delim (plist-get hdr :delim))
         (fields (plist-get hdr :fields))
         (delim-str (char-to-string delim))
         (bracket-delim (if (= delim ?,) "" delim-str)))
    (concat
     (or key "")
     "[" digits bracket-delim "]"
     (when fields
       (concat "{" (mapconcat #'identity fields delim-str) "}"))
     ":")))

(defun toon--parse-key-value (s)
  "Parse key-value line from S. Return cons (KEY . VALUE) or nil."
  (let* ((pos (toon--find-unquoted-char s ?:))
         (len (length s)))
    (when pos
      (let* ((key-part (toon--string-trim-right (substring s 0 pos)))
             (value-part (substring s (1+ pos)))
             (i 0))
        (setq i (toon--skip-ws key-part i))
        (let ((res (toon--parse-key-token key-part i)))
          (when res
            (let ((end (cdr res)))
              (setq end (toon--skip-ws key-part end))
              (when (= end (length key-part))
                (cons (car res) (toon--string-trim-left value-part))))))))))

(defun toon--format-statement (s)
  "Format a single TOON statement S (no indentation)."
  (let ((hdr (toon--parse-header s)))
    (cond
     (hdr
      (let* ((header (toon--build-header hdr))
             (rest (toon--string-trim (plist-get hdr :rest))))
        (if (toon--string-empty-p rest)
            header
          (concat header " " rest))))
     (t
      (let ((kv (toon--parse-key-value s)))
        (if kv
            (let ((key (car kv))
                  (val (cdr kv)))
              (if (toon--string-empty-p val)
                  (concat key ":")
                (concat key ": " (toon--string-trim val))))
          (toon--string-trim s)))))))

(defun toon--format-line-content (content)
  "Format a TOON line CONTENT without leading indentation."
  (let ((trimmed (toon--string-trim content)))
    (cond
     ((toon--string-empty-p trimmed) "")
     ((or (string= trimmed "-") (string-prefix-p "- " trimmed))
      (let ((after (toon--string-trim-left (substring trimmed 1))))
        (if (toon--string-empty-p after)
            "-"
          (concat "- " (toon--format-statement after)))))
     (t (toon--format-statement trimmed)))))

(defun toon--format-buffer-string (input)
  "Return formatted TOON for INPUT."
  (let* ((lines (split-string input "\n" nil))
         (indent-size toon-indent-offset)
         (out-lines '()))
    (dolist (line lines)
      (let* ((line (toon--string-trim-right line))
             (split (toon--count-leading-spaces line))
             (spaces (car split))
             (content (cdr split)))
        (if (toon--string-empty-p (toon--string-trim content))
            (setq out-lines (cons "" out-lines))
          (when (and indent-size (> indent-size 0))
            (when (/= 0 (mod spaces indent-size))
              (user-error "Indentation is not a multiple of toon-indent-offset")))
          (let* ((depth (if (and indent-size (> indent-size 0))
                            (/ spaces indent-size)
                          0))
                 (indent (make-string (* depth indent-size) ?\s))
                 (formatted (toon--format-line-content content)))
            (setq out-lines (cons (concat indent formatted) out-lines))))))
    (mapconcat #'identity (nreverse out-lines) "\n")))

(defun toon--call-cli (input &rest args)
  "Run the TOON CLI with ARGS on INPUT and return stdout as a string."
  (let ((program (toon--ensure-cli))
        (err-file (make-temp-file "toon-cli-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (insert input)
          (let* ((exit-code (apply #'call-process-region
                                   (point-min) (point-max)
                                   program
                                   t (list t err-file) nil
                                   args))
                 (stdout (buffer-string))
                 (stderr (toon--read-file-string err-file)))
            (if (eq exit-code 0)
                stdout
              (toon--display-cli-error program args exit-code stdout stderr)
              (error "TOON CLI failed (%s). See *TOON CLI Error* for details."
                     exit-code))))
      (when (and err-file (file-exists-p err-file))
        (delete-file err-file)))))

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

(defun toon--format-string (input)
  "Format TOON INPUT by normalizing whitespace and indentation."
  (toon--format-buffer-string input))

(defun toon-format-buffer ()
  "Pretty-print the current buffer as canonical TOON."
  (interactive)
  (let ((formatted (toon--format-string (buffer-string))))
    (erase-buffer)
    (insert formatted)))

(defvar toon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'toon-convert-buffer-to-json)
    (define-key map (kbd "C-c C-f") #'toon-format-buffer)
    map)
  "Keymap for `toon-mode'.")

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
