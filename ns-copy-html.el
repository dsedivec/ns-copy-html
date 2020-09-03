;;; ns-copy-html.el --- Copy region as HTML on macOS  -*- lexical-binding: t; -*-
;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Version: 1.4
;; Package-Requires: ((htmlize "1.34") (emacs "25.1"))
;; URL: https://github.com/dsedivec/ns-copy-html

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `ns-copy-html-region' to copy the region to the macOS clipboard
;; as both HTML and plain text.

;;; Code:

(require 'htmlize)

(defgroup ns-copy-html nil
  "Copy text to macOS clipboard as HTML."
  :group 'environment
  :prefix "ns-copy-html-")

(defcustom ns-copy-html-suppressed-faces
  '(flyspell-incorrect flyspell-duplicate whitespace-line)
  "List of faces that should not be included in the HTML we generate."
  :type '(repeat face))

;;;###autoload
(defun ns-copy-html-region (start end)
  "Put HTML version of buffer between START and END on macOS clipboard."
  (interactive "r")
  (let* ((htmlize-face-overrides
          (seq-reduce (lambda (overrides face)
                        ;; OVERRIDES must come last in this nconc call
                        ;; or else we'll modify
                        ;; `htmlize-face-overrides', which would be
                        ;; bad.
                        (nconc (list face '(:inherit nil)) overrides))
                      ns-copy-html-suppressed-faces
                      htmlize-face-overrides))
         (htmlize-output-type 'inline-css)
         (htmlize-html-charset nil)
         ;; This allows me to blissfully ignore most encoding issues.
         ;; If you have a non-Unicode character in your buffer, this
         ;; will presumably just error out.
         (htmlize-convert-nonascii-to-entities t)
         ;; `htmlize-region' will gladly capture the background color
         ;; of the active region if you let it.
         (html-buffer (save-mark-and-excursion
                        (deactivate-mark)
                        (htmlize-region start end)))
         utf8-start)
    (unwind-protect
         (progn
           (with-current-buffer html-buffer
             ;; Convert the buffer to hex.
             (goto-char (point-min))
             (while (not (eobp))
               (insert (format "%02x" (prog1
                                          (char-after (point))
                                        (delete-char 1)))))
             ;; Now start turning the result into an AppleScript
             ;; script.  This is not insane, not at all.
             (goto-char 0)
             ;; "HTML" really has to be upper case here, at least in
             ;; the "data" literal.
             (insert "set the clipboard to {«class HTML»:«data HTML")
             (goto-char (point-max))
             (insert "», «class utf8»:\"")
             (setq utf8-start (point)))
           (append-to-buffer html-buffer start end)
           (with-current-buffer html-buffer
             (goto-char utf8-start)
             (while (re-search-forward "\\([\"\\\\]\\)" nil t)
               (replace-match "\\\\\\1"))
             (goto-char (point-max))
             (insert "\"}\n")
             (if (fboundp 'ns-do-applescript)
                 (ns-do-applescript (buffer-string))
               (call-process-region nil nil "osascript"))))
      (kill-buffer html-buffer))
    (message "Copied region to clipboard as HTML and plain text")))

(provide 'ns-copy-html)
;;; ns-copy-html.el ends here
