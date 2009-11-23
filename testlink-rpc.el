;;  testlink-rpc.el --- testlink pages in emacs via XML-RPC

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

(defun testlink-rpc-call (method &rest args)
  "Call METHOD with ARGS via XML-RPC and return response data.
WARNING: This functionis not use because synchronous
`xml-rpc-method-call' has strange behavour on authentication
retrying.  Use `testlink-rpc-call-async' instead."
  (when (< emacs-major-version 22)
    (ad-activate 'encode-coding-string))
  (unwind-protect
      (let* ((ep  (plist-get testlink-current-project :endpoint))
             (key (plist-get testlink-current-project :apikey))
             (xml-rpc-base64-encode-unicode nil)
             (xml-rpc-base64-decode-unicode nil)
             (result (with-temp-buffer
                       (funcall 'xml-rpc-method-call
                                ep method (cons (cons "devKey" key) args)))))
        (if (and (numberp result) (= result 0))
            nil
          (if (stringp result)
              (apply `concat (split-string result "\r"))
            result)))
    (when (< emacs-major-version 22)
      (ad-deactivate 'encode-coding-string))))

(defun testlink-rpc-get-testcases-for-testplan (testplan_id)
  "Get content of TESTCASE for TESTPLAN invoking XML-RPC call."
  (testlink-rpc-call 'tl.getTestCasesForTestPlan
                     (cons "testplanid" testplan_id)))

(defun testlink-rpc-get-last-execution-result (testplan_id testcase_id)
  "Get last execution result invoking XML-RPC call."
  (testlink-rpc-call 'tl.getLastExecutionResult
                     (cons "testplanid" testplan_id)
                     (cons "testcaseid" testcase_id)))

(defun testlink-rpc-get-first_level_testsuites_for_testproject (testproject_id)
  "Get first level TESTSUITES in TESTPROJECT invoking XML-RPC call."
  (testlink-rpc-call 'tl.getFirstLevelTestSuitesForTestProject
                     (cons "testprojectid" testproject_id)))

(defun testlink-rpc-get-subtree (testsuite_id)
  "Get sub tree of TESTSUITE invoking XML-RPC call."
  (testlink-rpc-call 'tl.getSubTree
                     (cons "testsuiteid" testsuite_id)))


(defun testlink-rpc-get-testcases-for-testsuite (testsuite_id deep &optional details)
  "Get all TESTCASES in TESTSUITE invoking XML-RPC call.
details should be passed 'only_id', 'simple' or 'full'"
  (testlink-rpc-call 'tl.getTestCasesForTestSuite
                     (cons "testsuiteid" testsuite_id)
                     (cons "details" details)
                     (cons "deep" deep)))

(defun testlink-rpc-get-testprojects ()
  "Get all TESTPROJECTS invoking XML-RPC call."
  (mapcar (lambda (alist)
            (append '(("node_type_id" . "1") ("open" . t)) alist))
          (testlink-rpc-call 'tl.getProjects)))

(defun testlink-rpc-get-testcase (testcase_id)
  "Get TESTCASE invoking XML-RPC call."
  (testlink-rpc-call 'tl.getTestCase
                     (cons "testcaseid" testcase_id)))

(provide 'testlink-rpc)