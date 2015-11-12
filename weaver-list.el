;;; weaver-list.el --- tabulated-list-mode sourced from arbitrary web APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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
(require 'weaver-core)

(defun weaver--list-thread-format-to-tabulated-list-format (thread)
  (let ((address (pop thread)))
    (let-alist thread
      (list (or .name (weaver--namify-address address))
            (or .width 10)
            .sort
            :right-align .right-align
            :pad-right .pad-right))))


;;; Major-mode setup
(defconst weaver--list-key-definitions
  '(
    ;; S-down and S-up would collide with `windmove'.
    ("<down>" weaver-list-next)
    ("<up>" weaver-list-previous)
    ("RET" weaver-list-display "Display")
    ("n" weaver-list-next "Navigate")
    ("p" weaver-list-previous "Navigate")
    ("j" weaver-list-view-next "Navigate")
    ("k" weaver-list-view-previous "Navigate")
    ("N" weaver-list-next-far)
    ("P" weaver-list-previous-far)
    ("J" weaver-list-next-far)
    ("K" weaver-list-previous-far)
    ("g" weaver-list-refresh)
    ("v" weaver-visit-externally "visit")
    ;; ("m" weaver-list-mark-read "mark-read")
    )
  "List of key definitions for `weaver-list-mode'.
This list must follow the form described in
`weaver--key-definitions-to-header-line'.")

(defun weaver-list-refresh (&optional redisplay no-update)
  "Update the weaver-list.
If REDISPLAY is non-nil (or if interactive), also call `tabulated-list-print'.
If the prefix argument NO-UPDATE is nil, query StackExchange for
a new list before redisplaying."
  (interactive "p\nP")
  ;; Reset the mode-line unread count (we rebuild it here).
  (unless no-update
    (setq weaver--pages-so-far 1))
  (let* ((thread-list (or (and no-update weaver--dataset)
                          (and (functionp weaver--paging-function)
                               (funcall weaver--paging-function 1))
                          weaver--dataset))
         ;; Preserve window positioning.
         (window (get-buffer-window (current-buffer)))
         (old-start (when window (window-start window))))
    ;; The dataset contains everything. Hiding and filtering is done
    ;; on the `tabulated-list-entries' below.
    (setq weaver--dataset thread-list)
    ;; Print the result.
    (setq tabulated-list-entries
          (remove nil (mapcar (lambda (x) (list x (funcall weaver--print-function x)))
                              weaver--dataset)))
    (when redisplay
      (tabulated-list-print 'remember))
    (when window
      (set-window-start window old-start)))
  (message "Done."))

(define-derived-mode weaver-list-mode tabulated-list-mode "Weaver-List"
  "Major mode for browsing a list of questions from StackExchange.
Letters do not insert themselves; instead, they are commands.

The recommended way of using this mode is to activate it and then
set `weaver--paging-function'. The return value of
this function is mapped with `weaver--print-function',
so you may need to customize the latter if the former does not
return a list of questions.

The full list of variables which can be set is:
 1. `weaver-thread-format'
 2. `weaver--print-function'
      Change this if the data you're dealing with is not strictly a
      list of threads (see the doc for details).
 4. `weaver--paging-function'
      This is used to fetch further threads. If thread 3 is nil, it is
      also used to populate the initial list.
 5. `weaver--dataset'
      This is only used if both 3 and 4 are nil. It can be used to
      display a static list.
\\<weaver-mode-map>

Thread 2 is mandatory, but it also has a sane default which is
usually enough.

As long as one of 3, 4, or 5 is provided, the other two are
entirely optional. Populating or refreshing the list of threads
is done in the following way:
 - Set `weaver--pages-so-far' to 1.
 - Call function 2.
 - If function 2 is not given, call function 3 with argument 1.
 - If 3 is not given use the value of 4.

Adding further threads to the bottom of the list is done by:
 - Increment `weaver--pages-so-far'.
 - Call function 3 with argument `weaver--pages-so-far'.
 - If it returns anything, append to the dataset and refresh the
   display; otherwise, decrement `weaver--pages-so-far' and tell
   the user there are no more pages.

\\{weaver-list-mode-map}"
  (hl-line-mode 1)
  ;; (setq mode-line-format
  ;;       weaver--mode-line-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
            #'weaver-list-refresh nil t)
  ;; (setq tabulated-list-use-header-line nil)
  ;; (tabulated-list-init-header)
  (setq header-line-format (weaver--key-definitions-to-header-line weaver--list-key-definitions)))

(mapc (lambda (x) (define-key weaver-list-mode-map (kbd (car x)) (cadr x)))
      weaver--list-key-definitions)

(cl-defun weaver-list-create (&key name fields
                                   display-function paging-function visit-function
                                   visit-address
                                   nodisplay)
  "Create and display a weaver-list buffer with the given specs."
  (with-current-buffer (get-buffer-create (format "*%s*" name))
    (weaver-list-mode)
    (setq mode-name name)
    (setq weaver-thread-format
          (mapcar #'weaver--normalize-field-properties
                  (let ((i -1))
                    (mapcar (lambda (f) `(,(car f) (idx . ,(cl-incf i)) ,@(cdr f)))
                            fields))))
    (setq weaver--paging-function paging-function)
    (weaver-list-refresh)
    (when visit-function
      (setq weaver--visit-function visit-function))
    (when visit-address
      (setq weaver--visit-function
            (add-function :filter-args weaver--visit-function
                          (lambda (x) (list (weaver--get-in (car x) visit-address)))
                          '((name . visit-address)))))
    (setq weaver--display-function (or display-function weaver--visit-function))
    (setq weaver-thread-format
          (mapcar #'weaver--normalize-field-width weaver-thread-format))
    (setq tabulated-list-format
          (weaver--mapv #'weaver--list-thread-format-to-tabulated-list-format
                        weaver-thread-format))
    (tabulated-list-print)
    (goto-char (point-min))
    (unless nodisplay (pop-to-buffer (current-buffer)))))


;;; Commands
(defun weaver-list-view-previous (n)
  "Move cursor up N questions up and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (weaver-list-view-next (- n)))

(defun weaver-list-display (thread)
  "Call `weaver--display-function' on THREAD."
  (interactive (list (tabulated-list-get-id)))
  ;; (weaver--list-mark-read thread)
  (funcall weaver--display-function thread))

(defun weaver-list-view-next (n)
  "Move cursor down N questions and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (weaver-list-next n)
  (weaver-list-display (tabulated-list-get-id)))

(defun weaver--list-ensure-good-line-position ()
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

(defun weaver-list-next-page ()
  "Fetch and display the next page of questions."
  (interactive)
  ;; Stay at the last line.
  (goto-char (point-max))
  (forward-line -1)
  (when (functionp weaver--paging-function)
    ;; Try to get more questions
    (let ((list (funcall weaver--paging-function
                         (1+ weaver--pages-so-far))))
      (if (null list)
          (message "No further threads.")
        ;; If it worked, increment the variable.
        (cl-incf weaver--pages-so-far)
        (setq weaver--dataset (cl-remove-duplicates (append weaver--dataset list)))
        (weaver-list-refresh 'redisplay 'no-update)
        (forward-line 1)))))

(defvar-local weaver--list-last-refresh (current-time)
  "Time of the latest refresh.")

(defun weaver-list-next (n)
  "Move cursor down N questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (if (and (< n 0) (bobp))
      (when (> (time-to-seconds
                (time-subtract (current-time) weaver--list-last-refresh))
               1)
        (weaver-list-refresh 'redisplay)
        (setq weaver--list-last-refresh (current-time)))
    (forward-line n)
    ;; If we were trying to move forward, but we hit the end.
    (when (eobp)
      ;; Try to get more questions.
      (weaver-list-next-page))
    (weaver--list-ensure-good-line-position)))

(defun weaver-list-previous (n)
  "Move cursor up N questions."
  (interactive "p")
  (weaver-list-next (- n)))

(defcustom weaver-list-far-step-size 5
  "How many questions `weaver-list-next-far' skips."
  :type 'integer
  :group 'weaver-list
  :package-version '(weaver-list . ""))

(defun weaver-list-next-far (n)
  "Move cursor up N*`weaver-list-far-step-size' questions."
  (interactive "p")
  (weaver-list-next (* n weaver-list-far-step-size)))

(defun weaver-list-previous-far (n)
  "Move cursor up N questions."
  (interactive "p")
  (weaver-list-next-far (- n)))

(provide 'weaver-list)
;;; weaver-list.el ends here
