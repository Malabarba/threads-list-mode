;;; weaver.el --- Main entry-point                   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; (add-to-list 'load-path (expand-file-name "./"))
(require 'weaver-list)
(require 'weaver-thread)
(require 'paradox)


;;; Github
(defconst weaver--github-thread-format
  `[((updated_at) . ,weaver-field-time-ago)
    ((subject type)
     (properties . (face font-lock-comment-face)))
    ((repository name)
     (properties . (face font-lock-variable-name-face)))
    ((subject title))])

(defvar json-array-type)
(defvar json-false)
(declare-function json-read "json")
(defvar paradox--github-next-page)

(defun weaver--json-reader ()
  (let ((json-false nil)
        (json-array-type 'list))
    (json-read)))

(defun weaver--github-action (url)
  (paradox--github-action url
    :max-pages 1
    :reader #'weaver--json-reader))

(defun weaver--github-pager-function (url &optional thread)
  (lambda (&optional page)
    (require 'json)
    (if (or (not page) (= page 1))
        (if thread
            (cons thread (weaver--github-action url))
          (weaver--github-action url))
      (when paradox--github-next-page
        (weaver--github-action paradox--github-next-page)))))

(defun weaver--github-display-function (thread)
  (let-alist thread
    (weaver-thread-create
     :name (format "Github %s" (cond
                                (.subject.type)
                                ((and (stringp .url) (string-match "/pulls/" .url))
                                 "Pull Request")
                                ("issue")))
     :title (or .title .subject.title)
     :header-fields `[((user avatar_url)
                       (reader . weaver--thread-create-image)
                       (width . 4))
                      (" ")
                      ((user login))
                      ("  ")
                      ((updated_at) . ,weaver-field-time-ago)]
     :body-address '(body)
     :visit-address '(html_url)
     ;; :display-function
     :content-function (let ((url (or .url .subject.url)))
                         (thread-first (replace-regexp-in-string "/pulls/" "/issues/" url)
                           (concat "/comments")
                           (weaver--github-pager-function (weaver--github-action url)))))))

;;;###autoload
(defun weaver-github-notification ()
  "Show a thread list with your github notifications."
  (interactive)
  (weaver-list-create :name "Github Notifications"
                :fields weaver--github-thread-format
                :visit-address '(subject url)
                :visit-function (lambda (x)
                                  (browse-url
                                   (replace-regexp-in-string
                                    "//api\\.github.com/repos" "//github.com" x)))
                :display-function #'weaver--github-display-function
                :paging-function (weaver--github-pager-function "notifications")))

(defconst weaver--github-issue-list-format
  `[((updated_at) . ,weaver-field-time-ago)
    ((repository full_name)
     (properties . (face font-lock-variable-name-face)))
    ((title))])

(defun weaver-github-issues ()
  "Show a thread list with your Github issues for a given repo."
  (interactive)
  (weaver-list-create :name "Github Issues"
                :fields weaver--github-issue-list-format
                ;; :visit-address '(subject url)
                ;; :visit-function (lambda (x) (browse-url
                ;;                         (replace-regexp-in-string
                ;;                          "//api\\.github.com/repos" "//github.com" x)))
                :display-function #'weaver--github-display-function
                :paging-function (weaver--github-pager-function "issues")))

(provide 'weaver)
;;; weaver.el ends here
