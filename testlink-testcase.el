;;  testlink-testcase.el --- testlink pages in emacs via XML-RPC

;; Copyright (C) 2009  Yuta Namiki

;; Author: Yuta Namiki <namikister@gmail.com>
;; Keywords: testlink, xml-rpc
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Overview:

(require 'testlink-rpc)

(defvar testlink-testcase-mode-hook nil)

(defvar testlink-testcase-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'testlink-testcase-quit)
    (define-key map "n" 'testlink-testcase-next-ticket)
    (define-key map "p" 'testlink-testcase-prev-ticket)
    map))

(defvar testlink-testcase-w3m-list
  '("summary" "steps" "expected_results"))

(defvar testlink-testcase-header-format
  '("#%s: %s:ver%s"
    "tc_external_id" "name" "version")
  "*A testcase header format.
This variable is a following form:
\(FORMAT NAME NAME ...)

FORMAT is a format string of `format'.
NAME is a name of field.
Field value corresponding to NAME is used to format parameter.")

(defvar w3m-prefix "w3m-")

(defvar testlink-testcase-contents-format
  (list (concat
         "[Author] %s %s\n"
         "[Updater] %s %s\n"
         "[Created]  %s\n"
         "[Modified] %s\n"
         "[Summary]\n"
         "%s\n"
         "[Steps]\n"
         "%s\n"
         "[Results]\n"
         "%s")
    "author_last_name"  "author_first_name"
    "updater_last_name" "updater_first_name"
    "creation_ts" "modification_ts"
    (concat w3m-prefix "summary")
    (concat w3m-prefix "steps")
    (concat w3m-prefix "expected_results"))
  "*A testcase header format.
This variable is a following form:
\(FORMAT NAME NAME ...)

FORMAT is a format string of `format'.
NAME is a name of field.
Field value corresponding to NAME is used to format parameter.")

(defvar testlink-testcase-font-lock-keywords
  '(("^\s*\\(\\[\\(.*\\)\\]\\)"
     1 font-lock-keyword-face)))

(defvar testlink-testcase-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?# "w" tab)
    (modify-syntax-entry ?: "w" tab)
    tab))

(defvar testlink-testcase-buffer-name-format "*testcase%s:%s*")

(defun testlink-testcase-mode ()
  "View ticket mode.
\\{testlink-testcase-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map testlink-testcase-mode-map)
  (set-syntax-table testlink-testcase-syntax-table)
  (setq major-mode 'testlink-testcase-mode)
  (setq mode-name "TestCase")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
         '(testlink-testcase-font-lock-keywords
           t     ; KEYWORDS-ONLY
           t     ; CASE-FOLD
           nil   ; SYNTAX-ALIST
           nil)) ; SYNTAX-BEGIN
  (run-hooks 'testlink-testcase-mode-hook))

(defun testlink-testcase (id)
  (interactive
   (list (or testlink-current-project
             (testlink-get-project current-prefix-arg))
         (read-number "TestCase ID: ")))
  (let* ((testcase (car (testlink-rpc-get-testcase id)))
         (external-id (cdr (assoc "tc_external_id" testcase)))
         (version (cdr (assoc "version" testcase))))
    (setq testlink-current-case-id id)
    (setq testlink-current-case-buffer
          (switch-to-buffer (format testlink-testcase-buffer-name-format
                                    external-id
                                    version)))
    (erase-buffer)
    ;; convert html strings
    (dolist (key testlink-testcase-w3m-list)
      (let ((val (testlink-assoc-if-exist key testcase)))
        (insert (if val val "")))
      (w3m-region (point-min) (point-max))
      (testlink-put-alist (concat w3m-prefix key)
                          (buffer-substring-no-properties
                           (point-min)
                           (max (point-min) (- (point-max) 2)))
                          testcase)
      (erase-buffer))
    ;; title
    (insert (propertize
             (apply 'format (car testlink-testcase-header-format)
                    (mapcar (lambda (key)
                              (let ((val (testlink-assoc-if-exist key testcase)))
                                (if val val "")))
                            (cdr testlink-testcase-header-format)))
             'face 'bold)
            "\n")
    ;; contents
    (insert (propertize
             (apply 'format (car testlink-testcase-contents-format)
                    (mapcar (lambda (key)
                              (let ((val (testlink-assoc-if-exist key testcase)))
                                (if val val "")))
                            (cdr testlink-testcase-contents-format)))
             )
            "\n")
    (goto-char (point-min))
    (testlink-testcase-mode)
    ))

(defun testlink-testcase-find-next (search-list id)
  (if search-list
      (print search-list)
      (let (ret alist)
        (while (setq alist (pop search-list))
          (print alist)
          (setq ret
                (cond (testlink-assoc-if-exist "node_type_id" alist)
                      ('("1" "2") ;; project suite
                       (testlink-testcase-find-next
                        (testlink-assoc-if-exist "childNodes" alist) id))
                      ("3" ;; case
                       (print (testlink-assoc-if-exist "id" alist) id)
                       (if (equal id (testlink-assoc-if-exist "id" alist))
                           (if search-list (pop search-list))))))
          (if ret (return ret))))))

(defun testlink-testcase-next (&optional id)
  (interactive "P")
  (unless id (setq id testlink-current-case-id))
  (print (testlink-testcase-find-next testlink-list-current-list id)))

(defun testlink-list-select-current-element (&optional not-open)
  "Select a testcase or a testsuite"
  (interactive "P")
  (let (id)
    (cond ((get-text-property (point) :case)
           (setq id (get-text-property (point) 'id))
           (testlink-list-select-element id))
          ((or (get-text-property (point) :project)
               (get-text-property (point) :suite))
           (unless not-open
             (testlink-list-toggle-open)))
          (t
           (message "Can't select this line!")))))

(provide 'testlink-testcase)