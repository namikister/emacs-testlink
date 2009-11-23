;;  testlink.el --- testlink pages in emacs via XML-RPC

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

;; Commentary:

;; Overview:

(require 'xml-rpc)
(require 'cl)
(require 'w3m)

(require 'testlink-rpc)
(require 'testlink-testcase)

(defvar testlink-projects
  nil
  "*List of project definitions.
The value is alist of project name and information plist.
For example:

  '((\"sample\"
     :endpoint \"http://www.sample.org/testlink/lib/api/xmlrpc.php\"
     :apikey \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\")
    (\"test\"
     :endpoint \"http://localhost/testlink/lib/api/xmlrpc.php\"
     :apikey \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"
     :projects (\"testproject\")))")

(defvar testlink-list-mode-hook nil)
(defvar testlink-list-after-sync-hook nil)

(defvar testlink-list-node-type-alist
  '(("1" :project testlink-list-project-face)
    ("2" :suite   testlink-list-suite-face)
    ("3" :case    testlink-list-case-face)))

(defvar testlink-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'testlink-list-select-current-element)
    (define-key map "o"  'testlink-list-select-current-element)
    (define-key map "n"  'testlink-list-next-line)
    (define-key map "p"  'testlink-list-prev-line)
    (define-key map "/"  'testlink-list-toggle-open)
    ;;     (define-key map "?" 'testlink-list-search)
    ;;     (define-key map "[" 'testlink-list-open-all-list)
    ;;     (define-key map "]" 'testlink-list-close-all-list)
    ;;     (define-key map "q" 'testlink-exit)
    map))

(defvar testlink-list-buffer-name  "*testlink list*")

(defvar testlink-current-project nil)
(make-variable-buffer-local 'testlink-current-project)

(defvar testlink-project-history nil)
(defvar testlink-url-history nil)

(defvar testlink-list-current-list nil)
(make-variable-buffer-local 'testlink-list-current-list)
(defvar testlink-current-case-id nil)
(make-variable-buffer-local 'testlink-current-case-id)
(defvar testlink-current-case-buffer nil)

(defvar testlink-open-regexp "^ *\\(\\[[+-]\\]\\)")

(defcustom testlink-list-stay-list-window t
  "*If non-nil, list window is stayed opne."
  :type 'boolean
  :group 'testlink-list)

(defcustom testlink-list-window-height 40
  "*Height of list window."
  :type 'integer
  :group 'testlink-list)

(defcustom testlink-list-indent-width 2
  "*Indent width of tree in list window"
  :type 'integer
  :group 'testlink-list)

(defcustom testlink-list-display-id-p t
  "*If non-nil, id is displayed"
  :type 'boolean
  :group 'testlink-list)

(defgroup testlink nil
  "*Hypermedia, Testlink."
  :prefix "testlink-"
  :link '(url-link :tag "Home page of Testlink Project" "http://testlink.sourceforge.net/")
  :group 'hypermedia)

(defgroup testlink-face nil
  "Testlink, Faces."
  :prefix "testlink-"
  :group 'testlink)

(defgroup testlink-list nil
  "*Testlink, list buffer."
  :prefix "testlink-"
  :group 'testlink)

(defcustom testlink-list-mouse-face 'highlight
  "face used in list mode when tree node is pointed."
  :type '(choice (face :tag "Spedify face")
                 (const :tag "Not to use face" nil))
  :group 'testlink-list)

(defface testlink-list-project-face
  '((((class color) (background light))(:foreground "MidnightBlue"))
    (((class color) (background dark)) (:foreground "PaleTurquoise")))
  "face of test projcet"
  :group 'testlink-face)

(defface testlink-list-suite-face
  '((((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark))  (:foreground "LightBlue")))
  "face of test suite"
  :group 'testlink-face)

(defface testlink-list-case-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark))  (:foreground "white")))
  "face of test case"
  :group 'testlink-face)

;; from apel
(defsubst testlink-put-alist (key value alist)
  "Modify ALIST to set VALUE to KEY.
If there is a pair whose car is KEY, replace its cdr by VALUE.
If there is not such pair, create new pair (KEY . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc key alist)))
    (if pair
        (progn
          (setcdr pair value)
          alist)
      (setcdr alist (cons (cons key value) (cdr alist))))))

(defsubst testlink-assoc-if-exist (key alist &optional default)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
      (progn
        (testlink-put-alist key default alist)
        default))))

(defun testlink-list-mode ()
  "\\{testlink-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'testlink-list-mode)
  (setq mode-name "Testlink List")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map testlink-list-mode-map)
  (run-hooks 'testlink-list-mode-hook)
  (force-mode-line-update))

(defun testlink-list ()
  (interactive)
  (if (get-buffer testlink-list-buffer-name)
      (switch-to-buffer testlink-list-buffer-name)
    (switch-to-buffer (get-buffer-create testlink-list-buffer-name))
    (testlink-list-mode)
    (testlink-get-project)
    (save-excursion
      (testlink-list-sync t))))

(defun testlink-list-get-all ()
  (let ((projects (testlink-rpc-get-testprojects))
        (show-projects (plist-get testlink-current-project :projects))
        ret)
    (dolist (project projects)
      (if (or (not show-projects)
              (find-if
               #'(lambda (ele) (equal ele (cdr (assoc "name" project))))
               show-projects))
          (let* ((id (cdr (assoc "id" project)))
                 (suites (testlink-rpc-get-first_level_testsuites_for_testproject id)))
            (setcdr project (cons (cons "childNodes" suites) (cdr project)))
            (push project ret))))
    (nreverse ret)))

(defun testlink-list-sync (&optional first)
  (interactive "P")
  (save-excursion
    (let ((buffer-read-only nil)
          updated header time old-category-list)
      (when first
        (setq testlink-list-current-list
              (testlink-list-get-all)))
      (erase-buffer)
      (testlink-list-insert-element-names testlink-list-current-list 0)))
  (run-hooks 'testlink-list-after-sync-hook))

(defun testlink-list-select-element (id)
  "Select element of specified ID."
  (let* ((wins (testlink-list-split-window))
         (list-win (car wins))
         (case-win (cdr wins)))
    (select-window case-win)
    (switch-to-buffer testlink-list-buffer-name)
    (testlink-testcase id)
    (select-window list-win)))

(defun testlink-list-split-window ()
  "Split window if testlink-list-stay-list-window is non-nil"
  (interactive "P")
  (let ((list-win (get-buffer-window testlink-list-buffer-name))
        (case-win (if testlink-current-case-buffer
                      (get-buffer-window testlink-current-case-buffer))))
    (unless case-win
      (setq case-win
            (split-window
             nil
             (if (> (window-height) testlink-list-window-height)
                 testlink-list-window-height)
             nil)))
    (cons list-win case-win)))

(defun testlink-list-forward-line (&optional n)
  "Move N lines forward (backward if N is negative) and open a testcase."
  (forward-line n)
  (testlink-list-select-current-element t))

(defun testlink-list-next-line ()
  "Move to next line and open a testcase."
  (interactive)
  (testlink-list-forward-line 1))

(defun testlink-list-prev-line ()
  "Move to previous line and open a testcase."
  (interactive)
  (testlink-list-forward-line -1))

(defun testlink-list-get-child (alist)
  (let* ((key "childNodes")
         (child (cdr (assoc key alist))))
    (if child
        child
      (let* ((id (cdr (assoc "id" alist))))
        (setq child (testlink-rpc-get-subtree id))
        (when child
          (setq child (cdr (car child)))
          (testlink-put-alist key child alist))
        child))))

(defun testlink-list-toggle-open ()
  "toggle open list."
  (interactive)
  (when (save-excursion
          (end-of-line)
          (re-search-backward testlink-open-regexp nil t))
    (goto-char (match-beginning 1))
    (let* ((id    (get-text-property (point) 'id))
           (alist (get-text-property (point) 'alist))
           (level (get-text-property (point) 'level))
           (open  (testlink-assoc-if-exist "open" alist nil))
           (child (unless open (testlink-list-get-child alist)))
           (props (text-properties-at (point)))
           (buffer-read-only nil))
      (delete-region (point) (+ 3 (point)))
      (insert "[" (if open "+" "-") "]")
      (set-text-properties (- (point) 3) (point) props)
      (save-excursion
        (forward-line 1)
        (if open
            (delete-region
             (point)
             (let ((cur-level (1+ level)))
               (while (> cur-level level)
                 (setq cur-level
                       (cond ((re-search-forward "^ *[^ \n]" nil t)
                              (match-beginning 0)
                              (get-text-property (point) 'level))
                             (t -1))))
               (cond
                ((>= cur-level 0)
                 (beginning-of-line)
                 (point))
                (t (point-max)))))
          (testlink-list-insert-element-names child (+ level 1))))
      (testlink-put-alist "open" (not open) alist))))

(defun testlink-list-insert-element-names (list level)
  "insert content of list."
  (let ((prev (point))
        (indent (make-string (* testlink-list-indent-width level) ?\ )))
    (dolist (alist list)
      (let* ((name (cdr (assoc "name" alist)))
             (id   (cdr (assoc "id"   alist)))
             (node-type-attr
              (cdr (assoc (cdr (assoc "node_type_id" alist))
                          testlink-list-node-type-alist)))
             (node-type (car node-type-attr))
             (open  (testlink-assoc-if-exist "open" alist nil))
             (child (testlink-assoc-if-exist "childNodes" alist nil))
;;             (child (if open (testlink-list-get-child alist)))
             (open-string
              (if (or (equal node-type :project) (equal node-type :suite))
                  (concat "[" (if open "-" "+") "]")
                " * ")))
        (insert indent open-string name "\n")
        (set-text-properties prev (point) nil)
        (set-text-properties (- (point)
                                (length name)
                                (length open-string)
                                1)
                             (1- (point))
                             (list 'mouse-face testlink-list-mouse-face
                                   'face (cadr node-type-attr)))
        (put-text-property prev (point) node-type name)
        (put-text-property prev (point) 'id id)
        (put-text-property prev (point) 'level level)
        (put-text-property prev (point) 'alist alist)
        (when (and open child)
          (testlink-list-insert-element-names child (+ level 1)))
        (setq prev (point))))))

(defun testlink-ask-project ()
  "Prompts to enter project name and return its information data.
Returns project info which is property list of some data.  If hit
enter without project name, ask enter project informations
interectively and remember temporary project information data
named as \"dir@host\".  It will be kept until re-start Emacs."
  (let* ((project (and (or testlink-projects
                           testlink-project-history)
                       (completing-read "Select project (or empty to define): "
                                        testlink-projects
                                        nil t nil
                                        'testlink-project-history)))
         (pinfo (and project
                     (cdr (assoc project testlink-projects)))))
    (or pinfo
        ;; make project data interactively.
        (let* ((rawurl (read-string "Site URL: "
                                    (plist-get testlink-current-project :endpoint)
                                    'testlink-url-history))
               (apikey (read-string "API key: "
                                    (plist-get testlink-current-project :apikey)))
               (found
                (find-if
                 #'(lambda (ele) (equal (plist-get (cdr ele) :endpoint)
                                        rawurl))
                 testlink-projects))
               info)
          ;; build project info property list
          (prog1
              ;; make return value
              (setq info (list :endpoint rawurl
                               :apikey apikey))
            ;; remember it
            (if (not found)
                (add-to-list 'testlink-projects (cons project info))
              ;; already exist, overwrite
              (setcdr found info)))))))

(defun testlink-get-project (&optional force-read)
  (if (and (not force-read) testlink-current-project)
      testlink-current-project
    (let ((project (testlink-ask-project)))
      (setq testlink-current-project project))))

(defun testlink-debug ()
;;   (setq testlink-rpc-endpoint "http://192.168.1.24/~namiki/testlink/lib/api/xmlrpc.php")
;;   (setq testlink-rpc-apikey "f35e4d26e086b978413a48368cac7822")
  (eval-defun 'testlink-list-toggle-open)
  (testlink-list))

(provide 'testlink)
