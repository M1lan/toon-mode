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

(defun toon--in-string-p ()
  "Return non-nil if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun toon--bounds-of-string-at-point ()
  "Return bounds of string at point, or nil."
  (let ((state (syntax-ppss)))
    (when (nth 3 state)
      (let ((start (nth 8 state)))
        (save-excursion
          (goto-char start)
          (forward-sexp)
          (cons start (point)))))))

(defun toon--number-bounds-at-point ()
  "Return bounds of number at point, or nil."
  (save-excursion
    (when (and (not (toon--in-string-p))
               (or (looking-at-p "[0-9]")
                   (looking-at-p "-")
                   (looking-back "[0-9]" (line-beginning-position))))
      (let ((end (progn
                   (skip-chars-forward "-0-9eE+.")
                   (point)))
            (start (progn
                     (skip-chars-backward "-0-9eE+.")
                     (point))))
        (when (save-excursion
                (goto-char start)
                (looking-at
                 "\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\_>"))
          (cons start end))))))

(defun toon--key-token-identifier-p (token)
  "Return non-nil if TOKEN is an unquoted identifier key."
  (and token
       (not (string-prefix-p "\"" token))
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_.]*\\'" token)))

(defun toon--unescape-quoted (token)
  "Unescape a quoted TOKEN and return the inner string."
  (let ((s (substring token 1 -1)))
    (setq s (replace-regexp-in-string "\\\\\"" "\"" s))
    (setq s (replace-regexp-in-string "\\\\\\\\" "\\\\" s))
    (setq s (replace-regexp-in-string "\\\\n" "\n" s))
    (setq s (replace-regexp-in-string "\\\\r" "\r" s))
    (setq s (replace-regexp-in-string "\\\\t" "\t" s))
    s))

(defun toon--escape-for-path (s)
  "Escape S for use inside a quoted path segment."
  (let ((out s))
    (setq out (replace-regexp-in-string "\\\\" "\\\\\\\\" out))
    (setq out (replace-regexp-in-string "\"" "\\\\\"" out))
    out))

(defun toon--path-append-key (path token)
  "Append key TOKEN to PATH."
  (if (toon--key-token-identifier-p token)
      (concat path "." token)
    (let* ((raw (if (string-prefix-p "\"" token)
                    (toon--unescape-quoted token)
                  token))
           (escaped (toon--escape-for-path raw)))
      (concat path "[\"" escaped "\"]"))))

(defun toon--path-from-stack (stack)
  "Build a path string from STACK."
  (let ((path "$"))
    (dolist (frame (nreverse stack) path)
      (pcase (plist-get frame :type)
        ('item
         (setq path (concat path "[" (number-to-string (plist-get frame :index)) "]")))
        ((or 'object 'array 'tabular)
         (let ((key (plist-get frame :key)))
           (when key
             (setq path (toon--path-append-key path key)))))))))

(defun toon--count-delims-before (s delim pos)
  "Count unquoted DELIM occurrences before POS in S."
  (let ((i 0)
        (count 0)
        (len (length s))
        (in-string nil)
        (escaped nil))
    (while (< i (min pos len))
      (let ((ch (aref s i)))
        (cond
         ((and (not in-string) (= ch ?\"))
          (setq in-string t))
         ((and in-string (not escaped) (= ch ?\\))
          (setq escaped t))
         ((and in-string (not escaped) (= ch ?\"))
          (setq in-string nil))
         ((and in-string escaped)
          (setq escaped nil))
         ((and (not in-string) (= ch delim))
          (setq count (1+ count)))))
      (setq i (1+ i)))
    count))

(defun toon--find-tabular-frame (stack depth)
  "Return the nearest tabular frame matching DEPTH, or nil."
  (let ((frames stack)
        (match nil))
    (while (and frames (not match))
      (let ((frame (car frames)))
        (when (and (eq (plist-get frame :type) 'tabular)
                   (= (plist-get frame :row-depth) depth))
          (setq match frame)))
      (setq frames (cdr frames)))
    match))

(defun toon--update-first-frame (stack pred fn)
  "Update first frame in STACK matching PRED with FN."
  (cond
   ((null stack) nil)
   ((funcall pred (car stack))
    (cons (funcall fn (car stack)) (cdr stack)))
   (t (cons (car stack)
            (toon--update-first-frame (cdr stack) pred fn)))))

(defun toon--find-array-frame (stack)
  "Return nearest array frame from STACK."
  (let ((frames stack)
        (match nil))
    (while (and frames (not match))
      (let ((frame (car frames)))
        (when (eq (plist-get frame :type) 'array)
          (setq match frame)))
      (setq frames (cdr frames)))
    match))

(defun toon--path-at-point ()
  "Return TOON path at point, or nil."
  (save-excursion
    (let* ((target-pos (point))
           (target-line (line-number-at-pos))
           (indent-size toon-indent-offset)
           (stack '())
           (line-num 1)
           (path nil))
      (goto-char (point-min))
      (while (and (not path) (<= line-num target-line))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (line (toon--string-trim-right line))
               (split (toon--count-leading-spaces line))
               (spaces (car split))
               (content (cdr split)))
          (when (and indent-size (> indent-size 0) (/= 0 (mod spaces indent-size)))
            (setq spaces (* (floor (/ (float spaces) indent-size)) indent-size)))
          (let ((depth (if (and indent-size (> indent-size 0))
                           (/ spaces indent-size)
                         0)))
            (while (and stack (>= (plist-get (car stack) :depth) depth))
              (setq stack (cdr stack)))
            (unless (toon--string-empty-p (toon--string-trim content))
              (let* ((trimmed (toon--string-trim content))
                     (is-target (= line-num target-line))
                     (col (if is-target (- target-pos bol) 0))
                     (content-col (max 0 (- col spaces))))
                (cond
                 ;; List item
                 ((or (string= trimmed "-") (string-prefix-p "- " trimmed))
                  (let* ((after (toon--string-trim-left (substring trimmed 1)))
                         (hdr (and (not (toon--string-empty-p after))
                                   (toon--parse-header after)))
                         (kv (and (not hdr) (toon--parse-key-value after)))
                         (array-frame (toon--find-array-frame stack))
                         (idx (or (and array-frame (plist-get array-frame :index)) 0)))
                    (when is-target
                      (setq path (toon--path-from-stack stack))
                      (setq path (concat path "[" (number-to-string idx) "]"))
                      (when (and kv (car kv))
                        (setq path (toon--path-append-key path (car kv))))
                      (when (and hdr (plist-get hdr :key))
                        (setq path (toon--path-append-key path (plist-get hdr :key)))))
                    (when array-frame
                      (setq stack (toon--update-first-frame
                                   stack
                                   (lambda (f) (eq f array-frame))
                                   (lambda (f)
                                     (plist-put f :index (1+ idx))))))
                    (setq stack (cons (list :type 'item :depth depth :index idx) stack))
                    (when (and kv (toon--string-empty-p (toon--string-trim (cdr kv))))
                      (setq stack (cons (list :type 'object :depth depth :key (car kv)) stack)))
                    (when hdr
                      (let* ((tabular (plist-get hdr :fields))
                             (row-depth (if tabular (+ depth 2) (+ depth 1)))
                             (frame (list :type (if tabular 'tabular 'array)
                                          :depth depth
                                          :key (plist-get hdr :key)
                                          :index 0
                                          :delim (plist-get hdr :delim)
                                          :fields (plist-get hdr :fields)
                                          :row-depth row-depth)))
                        (setq stack (cons frame stack))))))
                 ;; Header line
                 ((let ((hdr (toon--parse-header trimmed)))
                    (when hdr
                      (when is-target
                        (setq path (toon--path-from-stack stack))
                        (when (plist-get hdr :key)
                          (setq path (toon--path-append-key path (plist-get hdr :key)))))
                      (let* ((tabular (plist-get hdr :fields))
                             (frame (list :type (if tabular 'tabular 'array)
                                          :depth depth
                                          :key (plist-get hdr :key)
                                          :index 0
                                          :delim (plist-get hdr :delim)
                                          :fields (plist-get hdr :fields)
                                          :row-depth (+ depth 1))))
                        (setq stack (cons frame stack)))
                      t)))
                 ;; Key/value line
                 ((let ((kv (toon--parse-key-value trimmed)))
                    (when kv
                      (when is-target
                        (setq path (toon--path-from-stack stack))
                        (setq path (toon--path-append-key path (car kv))))
                      (when (toon--string-empty-p (toon--string-trim (cdr kv)))
                        (setq stack (cons (list :type 'object :depth depth :key (car kv)) stack)))
                      t)))
                 ;; Tabular row
                 ((let ((tab (toon--find-tabular-frame stack depth)))
                    (when tab
                      (let ((idx (or (plist-get tab :index) 0)))
                        (when is-target
                          (setq path (toon--path-from-stack stack))
                          (setq path (concat path "[" (number-to-string idx) "]"))
                          (let* ((fields (plist-get tab :fields))
                                 (delim (plist-get tab :delim))
                                 (field-index (toon--count-delims-before trimmed delim content-col)))
                            (when (and fields (< field-index (length fields)))
                              (setq path (toon--path-append-key path (nth field-index fields))))))
                        (setq stack (toon--update-first-frame
                                     stack
                                     (lambda (f) (eq f tab))
                                     (lambda (f)
                                       (plist-put f :index (1+ idx))))))
                      t)))
                 (t nil))))))
        (forward-line 1)
        (setq line-num (1+ line-num)))
      path)))

(defun toon-mode-show-path ()
  "Print the path to the node at point in the minibuffer."
  (interactive)
  (let ((path (toon--path-at-point)))
    (if path
        (message "%s" path)
      (message "No TOON path found at point."))))

(defun toon-mode-kill-path ()
  "Save the path to the node at point to the kill ring."
  (interactive)
  (let ((path (toon--path-at-point)))
    (if path
        (progn
          (kill-new path)
          (message "%s" path))
      (message "No TOON path found at point."))))

(defun toon-toggle-boolean ()
  "If point is on `true' or `false', toggle it."
  (interactive)
  (unless (toon--in-string-p)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (string (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
           (pt (point)))
      (when (and bounds (member string '("true" "false")))
        (delete-region (car bounds) (cdr bounds))
        (if (string= "true" string)
            (insert "false")
          (insert "true"))
        (goto-char (min (point-max) (max (point-min) pt)))))))

(defun toon-nullify-value ()
  "Replace the value at point with `null'."
  (interactive)
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line (buffer-substring-no-properties bol eol))
         (split (toon--count-leading-spaces (toon--string-trim-right line)))
         (spaces (car split))
         (content (cdr split))
         (trimmed (toon--string-trim content)))
    (cond
     ;; Key/value line: replace value (or insert) regardless of point.
     ((let ((kv (toon--parse-key-value trimmed)))
        (when kv
          (let ((colon-pos (toon--find-unquoted-char trimmed ?:)))
            (when colon-pos
              (goto-char (+ bol spaces (1+ colon-pos)))
              (delete-region (point) (+ bol spaces (length trimmed)))
              (insert " null")
              t)))))
     ;; List item with primitive value.
     ((or (string= trimmed "-") (string-prefix-p "- " trimmed))
      (let ((after (toon--string-trim-left (substring trimmed 1))))
        (when (and (not (toon--string-empty-p after))
                   (not (toon--parse-header after))
                   (not (toon--parse-key-value after)))
          (goto-char (+ bol spaces))
          (delete-region (point) (+ bol spaces (length trimmed)))
          (insert "- null")
          t)))
     ;; Fallback: replace token/number/string at point.
     (t
      (let ((bounds (or (toon--bounds-of-string-at-point)
                        (toon--number-bounds-at-point)
                        (bounds-of-thing-at-point 'symbol))))
        (when bounds
          (delete-region (car bounds) (cdr bounds))
          (insert "null")))))))

(defun toon-increment-number-at-point (&optional delta)
  "Add DELTA to the number at point; DELTA defaults to 1."
  (interactive "P")
  (let ((bounds (toon--number-bounds-at-point)))
    (when bounds
      (let* ((num (string-to-number
                   (buffer-substring-no-properties (car bounds) (cdr bounds))))
             (delta (or delta 1))
             (new (number-to-string (+ num delta)))
             (pt (point)))
        (delete-region (car bounds) (cdr bounds))
        (insert new)
        (goto-char (min (point-max) pt))))))

(defun toon-decrement-number-at-point (&optional delta)
  "Subtract DELTA from the number at point; DELTA defaults to 1."
  (interactive "P")
  (toon-increment-number-at-point (- (or delta 1))))

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
    (define-key map (kbd "C-c C-p") #'toon-mode-show-path)
    (define-key map (kbd "C-c C-y") #'toon-mode-kill-path)
    (define-key map (kbd "C-c C-t") #'toon-toggle-boolean)
    (define-key map (kbd "C-c C-k") #'toon-nullify-value)
    (define-key map (kbd "C-c C-i") #'toon-increment-number-at-point)
    (define-key map (kbd "C-c C-d") #'toon-decrement-number-at-point)
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
