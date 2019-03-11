;;; ns-copy-html.el --- Copy region as HTML on macOS  -*- lexical-binding: t; -*-
;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Version: 1.2
;; Package-Requires: ((htmlize "1.34") (emacs "24"))
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
;; as both HTML and plain text.  Requires pbcopyhtml, the source for
;; which is short and given below in `ns-copy-html-pbcopyhtml-source'.
;; We will try to compile for you with swiftc if pbcopyhtml cannot be
;; found on your path or in `user-emacs-directory'.

;; What a friggin' hack job.

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

(defconst ns-copy-html-pbcopyhtml-source
  "import AppKit
let INIT_READ = 12
let stdin = FileHandle.standardInput
let firstChunk = stdin.readData(ofLength: INIT_READ)
let newlineIdx: Int = firstChunk.firstIndex(of: 0xA)!
let htmlSize: Int = Int(String(data: firstChunk.prefix(upTo: newlineIdx),
                               encoding: .ascii)!)!
var html = firstChunk.advanced(by: newlineIdx + 1)
html.append(stdin.readData(ofLength: htmlSize - html.count))
let text = stdin.readDataToEndOfFile()
let pb = NSPasteboard.general
pb.clearContents()
pb.setData(html, forType: NSPasteboard.PasteboardType.html)
pb.setData(text, forType: NSPasteboard.PasteboardType.string)
")

(defvar ns-copy-html-pbcopyhtml nil)

(defun ns-copy-html--find-pbcopyhtml ()
  "Find the \"pbcopyhtml\" binary, or else try to compile one."
  (if ns-copy-html-pbcopyhtml
      ns-copy-html-pbcopyhtml
    (setq ns-copy-html-pbcopyhtml
          (or (let ((exec-path (cons user-emacs-directory exec-path)))
                (executable-find "pbcopyhtml"))
              (let ((output (expand-file-name "pbcopyhtml"
                                              user-emacs-directory))
                    (temp-file (make-temp-file "pbcopyhtml" nil ".swift")))
                (unwind-protect
                     (progn
                       (with-temp-file temp-file
                         (insert ns-copy-html-pbcopyhtml-source))
                       (with-temp-buffer
                         (message "Compiling pbcopyhtml...")
                         (unless (zerop (call-process "swiftc" nil t nil
                                                      temp-file "-o" output))
                           (error "Failed to build pbcopyhtml: %s"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
                         (message "Compiling pbcopyhtml... done.")))
                  (delete-file temp-file))
                output)))))

(defun ns-copy-html--htmlize-region (start end)
  "htmlize the region from START to END."
  (let ((htmlize-face-overrides
         (seq-reduce (lambda (overrides face)
                       ;; OVERRIDES must come last in this nconc call
                       ;; or else we'll modify ;;
                       ;; `htmlize-face-overrides', which would be
                       ;; bad.
                       (nconc (list face '(:inherit nil)) overrides))
                     ns-copy-html-suppressed-faces
                     htmlize-face-overrides)))
    (htmlize-region-for-paste start end)))

;;;###autoload
(defun ns-copy-html-region (start end)
  "Put HTML version of buffer between START and END on macOS clipboard."
  (interactive "r")
  (let* ((pbcopyhtml (ns-copy-html--find-pbcopyhtml))
         (src-buf (current-buffer))
         (proc-out-buf (get-buffer-create "*pbcopyhtml*"))
         (html (ns-copy-html--htmlize-region start end)))
    (with-temp-buffer
      (insert (format "%d\n" (string-bytes html))
              html)
      (insert-buffer-substring src-buf start end)
      (unless (zerop (call-process-region (point-min) (point-max)
                                          pbcopyhtml nil proc-out-buf))
        (error "pbcopyhtml failed"))
      (message "Copied region to clipboard as HTML and plain text"))))

(provide 'ns-copy-html)
;;; ns-copy-html.el ends here
