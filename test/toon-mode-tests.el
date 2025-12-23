;;; toon-mode-tests.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(defconst toon-test--root
  (file-name-directory (or load-file-name buffer-file-name)))

(load-file (expand-file-name "../toon-mode.el" toon-test--root))

(defun toon-test--indent (input)
  (with-temp-buffer
    (insert input)
    (toon-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest toon-indent-basic-blocks ()
  (let* ((input "root:\nkey: 1\nitems[2]:\n- a\n- b\n")
         (expected "root:\n  key: 1\n  items[2]:\n    - a\n    - b\n"))
    (should (equal (toon-test--indent input) expected))))

(defun toon-test--face-at (text token)
  (with-temp-buffer
    (insert text)
    (toon-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward token)
    (get-text-property (match-beginning 0) 'face)))

(ert-deftest toon-font-lock-object-key ()
  (should (eq (toon-test--face-at "user_name: 1\n" "user_name")
              'font-lock-variable-name-face)))

(ert-deftest toon-font-lock-constants ()
  (should (eq (toon-test--face-at "flag: true\n" "true")
              'font-lock-constant-face)))

(ert-deftest toon-font-lock-number ()
  (should (eq (toon-test--face-at "count: 42\n" "42")
              'font-lock-constant-face)))

(ert-deftest toon-cli-decode-basic ()
  (skip-unless (executable-find "toon"))
  (let* ((json-object-type 'alist)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (json-text (toon--call-cli "a: 1\n" "--decode"))
         (data (json-read-from-string json-text)))
    (should (equal (alist-get 'a data) 1))))

(ert-deftest toon-format-buffer-normalizes-spacing ()
  (let* ((toon-indent-offset 4)
         (input "root:\n    key:1\n")
         (formatted (toon--format-string input)))
    (should (equal formatted "root:\n    key: 1\n"))))

(ert-deftest toon-mode-keybindings ()
  (with-temp-buffer
    (toon-mode)
    (should (eq (lookup-key toon-mode-map (kbd "C-c C-j"))
                'toon-convert-buffer-to-json))
    (should (eq (lookup-key toon-mode-map (kbd "C-c C-f"))
                'toon-format-buffer))))
