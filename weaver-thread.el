;;; weaver-thread.el --- major-mode for printing and reading threads  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: 

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

(require 'subr-x)
(require 'weaver-thread-print)
(require 'weaver-core)

(defun weaver--goto-property-change (prop &optional direction)
  "Move forward to the next change of text-property PROP.
Return the new value of PROP at point.

If DIRECTION is negative, move backwards instead."
  (let ((func (if (and (numberp direction)
                       (< direction 0))
                  #'previous-single-property-change
                #'next-single-property-change))
        (limit (if (and (numberp direction)
                        (< direction 0))
                   (point-min) (point-max))))
    (goto-char (funcall func (point) prop nil limit))
    (get-text-property (point) prop)))


;;; Movement commands
;; Sections are headers placed above a thread's content or an
;; answer's content, or above the list of comments. They are
;; identified with the `weaver--thread-section' text property.
;; To move between sections, just search for the property. The value
;; of the text-property is the depth of the section (1 for contents, 2
;; for comments).
(defcustom weaver-thread-recenter-line 1
  "Screen line to which we recenter after moving between sections.
This is used as an argument to `recenter', only used if the end
of section is outside the window.

If nil, no recentering is performed."
  :type '(choice (const :tag "Don't recenter" nil)
                 integer)
  :group 'weaver-thread)

(defun weaver-thread-next-section (&optional n)
  "Move down to next section (thread or answer) of this buffer.
Prefix argument N moves N sections down or up."
  (interactive "p")
  (let ((count (if n (abs n) 1)))
    (while (> count 0)
      ;; This will either move us to the next section, or move out of
      ;; the current one.
      (unless (weaver--goto-property-change 'weaver--thread-section n)
        ;; If all we did was move out the current one, then move again
        ;; and we're guaranteed to reach the next section.
        (weaver--goto-property-change 'weaver--thread-section n))
      (unless (get-char-property (point) 'invisible)
        (cl-decf count))))
  (when (equal (selected-window) (get-buffer-window))
    (when weaver-thread-recenter-line
      (let ((ov (weaver--thread-section-overlays-at (line-end-position))))
        (when (and (overlayp ov) (> (overlay-end ov) (window-end)))
          (recenter weaver-thread-recenter-line))))
    (when-let ((echo (get-text-property (point) 'help-echo)))
      (message "%s" echo))))

(defun weaver-thread-previous-section (&optional n)
  "Move down to previous section (thread or answer) of this buffer.
Prefix argument moves N sections up or down."
  (interactive "p")
  (weaver-thread-next-section (- (or n 1))))

(defun weaver-thread-hide-show-section (&optional _)
  "Hide or show section under point.
Optional argument _ is for `push-button'."
  (interactive)
  (let ((ov (or (weaver--thread-section-overlays-at
                 (line-end-position))
                (weaver--thread-section-overlays-at (point)))))
    (unless (overlayp ov)
      (user-error "Not inside a comment"))
    (goto-char (overlay-start ov))
    (forward-line 0)
    (overlay-put
     ov 'invisible
     (null (overlay-get ov 'invisible)))))

(defun weaver--thread-section-overlays-at (pos)
  "Return the highest priority section overlay at POS.
A section overlay has a `weaver--thread-section-content'
property."
  (cdr-safe (get-char-property-and-overlay
             pos 'weaver--thread-section-content nil)))


;;; Major-mode constants
(defconst weaver--thread-key-definitions
  '(
    ("<down>" weaver-thread-next-section)
    ("<up>" weaver-thread-previous-section)
    ("n" weaver-thread-next-section "Navigate")
    ("p" weaver-thread-previous-section "Navigate")
    ("g" weaver-thread-refresh)
    ("v" weaver-visit-externally)
    ("q" quit-window)
    ("SPC" scroll-up-command)
    ;; ("e" sx-edit "edit")
    ;; ("K" sx-delete "Delete")
    ;; ("c" sx-comment "comment")
    ;; ("a" sx-answer "answer")
    ("TAB" forward-button "Navigate")
    ("<S-iso-lefttab>" backward-button)
    ("<S-tab>" backward-button)
    ("<backtab>" backward-button))
  "List of key definitions for `weaver-thread'.
This list must follow the form described in
`weaver--key-definitions-to-header-line'.")


;;; Major-mode definition
(define-derived-mode weaver-thread-mode special-mode "Thread"
  "Major mode to display and navigate a thread and its answers.
Letters do not insert themselves; instead, they are commands.

Don't activate this mode directly.  Instead, to print a thread
on the current buffer use
`weaver--thread-erase-and-print-thread'.

\\<weaver-thread-mode-map>
\\{weaver-thread-mode-map}"
  ;; (setq mode-line-format weaver--thread-mode-line)
  (set (make-local-variable 'nobreak-char-display) nil)
  ;; Determine how to close this window.
  (unless (window-parameter nil 'quit-restore)
    (set-window-parameter
     nil 'quit-restore `(other window nil ,(current-buffer))))
  ;; We call font-lock-region manually. See `weaver--thread-insert-markdown'.
  (font-lock-mode -1))

;; We need this quote+eval combo because `kbd' was a macro in 24.2.
(mapc (lambda (x) (define-key weaver-thread-mode-map (kbd (car x)) (cadr x)))
      weaver--thread-key-definitions)

(cl-defun weaver-thread-create (&key name header-fields nodisplay
                               content-function body-address
                               visit-function visit-address)
  "Create and display a thread buffer with the given specs."
  (with-current-buffer (get-buffer-create (format "*%s*" name))
    (unless nodisplay (pop-to-buffer (current-buffer)))
    (weaver-thread-mode)
    (weaver--calculate-font-width)
    (setq header-line-format (weaver--key-definitions-to-header-line
                              weaver--thread-key-definitions))
    (setq mode-name name)
    (setq weaver--paging-function (lambda (_) (funcall content-function)))
    (setq weaver--print-function (lambda (x) (list x
                                        (weaver--get-in x body-address)
                                        (weaver--print-info x))))
    (setq weaver-thread-format
          (mapcar #'weaver--normalize-field-properties
                  (let ((i -1))
                    (mapcar (lambda (f) `(,(car f) (idx . ,(cl-incf i)) ,@(cdr f)))
                            header-fields))))
    (when visit-function
      (setq weaver--visit-function visit-function))
    (when visit-address
      (setq weaver--visit-function
            (add-function :filter-args weaver--visit-function
                          (lambda (x) (list (weaver--get-in (car x) visit-address)))
                          '((name . visit-address)))))
    ;; (add-to-list 'mode-line-buffer-identification
    ;;              weaver--thread-closed-mode-line-string)
    (weaver-thread-refresh)
    (goto-char (point-min))
    (weaver-thread-next-section)))

(defun weaver-thread-refresh (&optional no-update)
  "Refresh currently displayed thread.
Queries the API for any changes to the thread or its answers or
comments, and redisplays it.

With non-nil prefix argument NO-UPDATE, just redisplay, don't
query the api."
  (interactive "P")
  (weaver--thread-ensure-mode)
  (let ((point (point))
        (line (count-screen-lines (window-start) (point))))
    (unless no-update
      (setq weaver--dataset (mapcar weaver--print-function
                              (funcall weaver--paging-function 1))))
    (weaver--thread-print)
    (goto-char point)
    (when (equal (selected-window)
                 (get-buffer-window (current-buffer)))
      (recenter line)))
  (message "Done."))

(defun weaver--thread-ensure-mode ()
  "Ensures we are in thread mode, erroring otherwise."
  (unless (derived-mode-p 'weaver-thread-mode)
    (error "Not in `weaver-thread-mode'")))

(provide 'weaver-thread)
;;; weaver-thread.el ends here
