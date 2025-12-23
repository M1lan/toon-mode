(require 'ert)

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
