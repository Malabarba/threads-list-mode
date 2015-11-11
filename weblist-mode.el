;;; weblist-mode.el --- tabulated-list-mode sourced from arbitrary web APIs  -*- lexical-binding: t; -*-

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

(require 'tabulated-list)

(defun weblist--mapv (f v)
  "Like `mapcar', but return a vector"
  (vconcat (mapcar f v)))

(defun weblist--namify-address (address)
  (let ((name (replace-regexp-in-string "_" " " (format "%s" (car (last address))))))
    (cl-callf capitalize (elt name 0))
    name))

(defun weblist--thread-format-to-tabulated-list-format (thread)
  (let ((address (pop thread)))
    (let-alist thread
      (list (or .name (weblist--namify-address address))
            (or .width 10)
            .sort
            :right-align .right-align
            :pad-right .pad-right))))

(defun weblist--get-entry-in-thread (alist format)
  (if (not (consp format))
      format
    (let* ((address (car format))
           (content (cond ((consp address) (let ((out alist))
                                             (while (and address out)
                                               (setq out (alist-get (pop address) out)))
                                             out))
                          ((stringp address) address)
                          ((functionp address) (funcall address alist)))))
      (let-alist (cdr format)
        (cons (if .reader
                  (funcall .reader content)
                content)
              `(face default ,@ .properties))))))


;;; Printing
(defvar weblist-thread-format
  `[((updated_at)
     (reader . ,(lambda (x) (concat (seconds-to-string (time-to-seconds (time-since x))))))
     (right-align . t)
     (properties . (face font-lock-comment-face)))
    ((subject type)
     (width . 11)
     (properties . (face font-lock-comment-face)))
    ((repository name)
     (width . 13)
     (properties . (face font-lock-variable-name-face)))
    ((subject title))]
  "Format used to turn received data into tabulated-list entries.
The value is a vector where each element represents a column.  
    [COLUMN-1 COLUMN-2 COLUMN-3 ...]

A column is represent by a cons cell, where the car is an address
and the cdr is an alist of symbols and values:
    ((SYMBOL-1 . VALUE-1) (SYMBOL-2 . VALUE-2) ...)
Alternatively, the cdr can be a symbol, which is the name of a
variable holding an alist.

The ADDRESS is a list of how to find the column's value in the
received data, e.g.:
    (repository full_name)")

(defun weblist--print-info (thread-data)
  "Convert `json-read' THREAD-DATA into tabulated-list format.
The conversion is governed by `weblist-thread-format'."
  (list thread-data
        (weblist--mapv (lambda (f) (weblist--get-entry-in-thread thread-data f))
                weblist-thread-format)))

(defvar weblist--pages-so-far 0
  "Number of pages currently being displayed.
This variable gets reset to 0 before every refresh and increased
by one when the user requests more pages (by scrolling beyond the
bottom).")

(defvar weblist--next-page-function nil
  "Function used to fetch the next page of threads to be displayed.
Used by `weblist-mode'. This is a function, called with
no arguments, which returns a list of threads to be displayed.

This function will be called when the user presses \\<weblist-mode-map>\\[weblist-next] at the end
of the thread list. It should either return nil (indicating
\"no more threads\") or return a list of threads which will
appended to the currently displayed list.

If this is not set, it's the same as a function which always
returns nil.")

(defvar weblist--dataset nil
  "The logical data behind the displayed list of threads.
This dataset contains even threads that are hidden by the user,
and thus not displayed in the list of threads.

This is ignored if `weblist--refresh-function' is set.")

(defvar weblist--print-function #'weblist--print-info
  "Function to convert a thread alist into a tabulated-list entry.
This gives you fine-tuned customization of how threads are
printed, but it's generally not necessary to configure this. You
can instead rely on the default value and configure
`weblist-thread-format' instead.

If this is set to a different value, it may be necessary to
change `tabulated-list-format' accordingly.")

(defun weblist-refresh (&optional redisplay no-update)
  "Update the weblist.
If REDISPLAY is non-nil (or if interactive), also call `tabulated-list-print'.
If the prefix argument NO-UPDATE is nil, query StackExchange for
a new list before redisplaying."
  (interactive "p\nP")
  ;; Reset the mode-line unread count (we rebuild it here).
  (unless no-update
    (setq weblist--pages-so-far 1))
  (let* ((thread-list (or (and no-update weblist--dataset)
                          (and (functionp weblist--next-page-function)
                               (funcall weblist--next-page-function 1))
                          weblist--dataset))
         ;; Preserve window positioning.
         (window (get-buffer-window (current-buffer)))
         (old-start (when window (window-start window))))
    ;; The dataset contains everything. Hiding and filtering is done
    ;; on the `tabulated-list-entries' below.
    (setq weblist--dataset thread-list)
    ;; Print the result.
    (setq tabulated-list-entries
          (remove nil (mapcar weblist--print-function weblist--dataset)))
    (when redisplay
      (tabulated-list-print 'remember))
    (when window
      (set-window-start window old-start)))
  (message "Done."))

(define-derived-mode weblist-mode tabulated-list-mode "WebList"
  "Major mode for browsing a list of questions from StackExchange.
Letters do not insert themselves; instead, they are commands.

The recommended way of using this mode is to activate it and then
set `weblist--next-page-function'. The return value of
this function is mapped with `weblist--print-function',
so you may need to customize the latter if the former does not
return a list of questions.

The full list of variables which can be set is:
 1. `weblist-thread-format'
 2. `weblist--print-function'
      Change this if the data you're dealing with is not strictly a
      list of threads (see the doc for details).
 4. `weblist--next-page-function'
      This is used to fetch further threads. If thread 3 is nil, it is
      also used to populate the initial list.
 5. `weblist--dataset'
      This is only used if both 3 and 4 are nil. It can be used to
      display a static list.
\\<weblist-mode-map>

Thread 2 is mandatory, but it also has a sane default which is
usually enough.

As long as one of 3, 4, or 5 is provided, the other two are
entirely optional. Populating or refreshing the list of threads
is done in the following way:
 - Set `weblist--pages-so-far' to 1.
 - Call function 2.
 - If function 2 is not given, call function 3 with argument 1.
 - If 3 is not given use the value of 4.

Adding further threads to the bottom of the list is done by:
 - Increment `weblist--pages-so-far'.
 - Call function 3 with argument `weblist--pages-so-far'.
 - If it returns anything, append to the dataset and refresh the
   display; otherwise, decrement `weblist--pages-so-far' and tell
   the user there are no more pages.

\\{weblist-mode-map}"
  (hl-line-mode 1)
  ;; (setq mode-line-format
  ;;       weblist--mode-line-format)
  (setq weblist--pages-so-far 0)
  (setq tabulated-list-format
        (weblist--mapv #'weblist--thread-format-to-tabulated-list-format
                weblist-thread-format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
            #'weblist-refresh nil t)
  ;; (setq tabulated-list-use-header-line nil)
  ;; (tabulated-list-init-header)
  (setq header-line-format weblist--header-line))


;;; Keys
(defun weblist--key-definitions-to-header-line (definitions)
  "Return a `header-line-format' from DEFINITIONS.
DEFINITIONS is a list where each element has one of the following two forms
    (KEY COMMAND)
    (KEY COMMAND DESCRIPTION)

The latter are used to build the return value, the former are
ignored."
  (let ((ptize (lambda (x) `(:propertize ,x face mode-line-buffer-id)))
        alist out)
    (dolist (it definitions)
      (when (> (length it) 2)
        (let* ((key (car it))
               (desc (elt it 2))
               (cell (assoc desc alist)))
          (if cell (push key (cdr cell))
            (push (cons desc (list key)) alist)))))
    (dolist (it alist out)
      (let ((desc (car it))
            (keys (cdr it)))
        (push (list "   "
                    (cons (funcall ptize (car keys))
                          (mapcar (lambda (k) `("," ,(funcall ptize k))) (cdr keys)))
                    (let ((match
                           (and (= 1 (length keys))
                                (string-match (regexp-quote (car keys)) desc))))
                      (if (and (numberp match) (= 0 match))
                          (substring desc (length (car keys)))
                        (concat ":" desc))))
              out)))))

(defconst weblist--key-definitions
  '(
    ;; S-down and S-up would collide with `windmove'.
    ("<down>" weblist-next)
    ("<up>" weblist-previous)
    ("RET" weblist-display "Display")
    ("n" weblist-next "Navigate")
    ("p" weblist-previous "Navigate")
    ("j" weblist-view-next "Navigate")
    ("k" weblist-view-previous "Navigate")
    ("N" weblist-next-far)
    ("P" weblist-previous-far)
    ("J" weblist-next-far)
    ("K" weblist-previous-far)
    ("g" weblist-refresh)
    ("v" weblist-visit-externally "visit")
    ;; ("m" weblist-mark-read "mark-read")
    )
  "List of key definitions for `weblist-mode'.
This list must follow the form described in
`weblist--key-definitions-to-header-line'.")

(defconst weblist--header-line
  (weblist--key-definitions-to-header-line weblist--key-definitions)
  "Header-line used on the threads list.")

(mapc (lambda (x) (define-key weblist-mode-map (kbd (car x)) (cadr x)))
      weblist--key-definitions)


;;; Commands


(defun weblist-view-previous (n)
  "Move cursor up N questions up and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (weblist-view-next (- n)))

(defvar weblist-display-function nil)

(defun weblist--display (thread)
  "Call `weblist-display-function' on THREAD."
  (funcall weblist-display-function thread))

(defun weblist-view-next (n)
  "Move cursor down N questions and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (weblist-next n)
  (weblist--display (tabulated-list-get-id)))

(defun weblist--ensure-line-good-line-position ()
  "Scroll window such that current line is a good place.
Check if we're at least 6 lines from the bottom. Scroll up if
we're not. Do the same for 3 lines from the top."
  ;; At least one entry below us.
  (let ((lines-to-bottom (count-screen-lines (point) (window-end))))
    (unless (>= lines-to-bottom 6)
      (recenter (- 6))))
  ;; At least one entry above us.
  (let ((lines-to-top (count-screen-lines (point) (window-start))))
    (unless (>= lines-to-top 3)
      (recenter 3))))

(defun weblist-next-page ()
  "Fetch and display the next page of questions."
  (interactive)
  ;; Stay at the last line.
  (goto-char (point-max))
  (forward-line -1)
  (when (functionp weblist--next-page-function)
    ;; Try to get more questions
    (let ((list (funcall weblist--next-page-function
                         (1+ weblist--pages-so-far))))
      (if (null list)
          (message "No further threads.")
        ;; If it worked, increment the variable.
        (cl-incf weblist--pages-so-far)
        (setq weblist--dataset (cl-remove-duplicates (append weblist--dataset list)))
        (weblist-refresh 'redisplay 'no-update)
        (forward-line 1)))))

(defvar weblist--last-refresh (current-time)
  "Time of the latest refresh.")

(defun weblist-next (n)
  "Move cursor down N questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (if (and (< n 0) (bobp))
      (when (> (time-to-seconds
                (time-subtract (current-time) weblist--last-refresh))
               1)
        (weblist-refresh 'redisplay)
        (setq weblist--last-refresh (current-time)))
    (forward-line n)
    ;; If we were trying to move forward, but we hit the end.
    (when (eobp)
      ;; Try to get more questions.
      (weblist-next-page))
    (weblist--ensure-line-good-line-position)))

(defun weblist-previous (n)
  "Move cursor up N questions."
  (interactive "p")
  (weblist-next (- n)))

(defcustom weblist-far-step-size 5
  "How many questions `weblist-next-far' skips."
  :type 'integer
  :group 'weblist
  :package-version '(weblist . ""))

(defun weblist-next-far (n)
  "Move cursor up N*`weblist-far-step-size' questions."
  (interactive "p")
  (weblist-next (* n weblist-far-step-size)))

(defun weblist-previous-far (n)
  "Move cursor up N questions."
  (interactive "p")
  (weblist-next-far (- n)))


;;; Github
(defun weblist--github-next-page (page)
  ""
  (unless (> page 1)
    (paradox--github-action "notifications"
      :reader (lambda ()
                (let ((json-false nil)
                      (json-array-type 'list))
                  (json-read))))))
(provide 'weblist-mode)
;;; weblist-mode.el ends here
