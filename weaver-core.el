;;; weaver-core.el --- basic functions                     -*- lexical-binding: t; -*-

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

(require 'url)
(require 'subr-x)

(defun weaver--mapv (f v)
  "Like `mapcar', but return a vector"
  (vconcat (mapcar f v)))

(defun weaver--namify-address (address)
  (let ((name (replace-regexp-in-string "_" " " (format "%s" (car (last address))))))
    (cl-callf capitalize (elt name 0))
    name))

(defun weaver--get-in (alist address)
  (let ((out alist))
    (while (and address out)
      (setq out (alist-get (pop address) out)))
    out))

(defvar weaver--font-width 10)

(defun weaver--calculate-font-width ()
  (setq weaver--font-width
        (with-selected-window (selected-window)
          (let ((inhibit-read-only t))
            (insert " ")
            (prog1 (- (car (elt (posn-at-point) 2))
                      (car (elt (posn-at-point (1- (point))) 2)))
              (delete-char -1))))))


;;; Printing
(defconst weaver-field-time-ago
  `((reader . ,(lambda (x) (seconds-to-string (time-to-seconds (time-since x)))))
    (right-align . t)
    (properties . (face font-lock-comment-face)))
  "Specs to display time fields as \"time ago\" strings.")

(defvar-local weaver-thread-format nil
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

(defvar weaver-thread-image-max-width 550
  "Maximum width, in pixels, of images in the question buffer.")

(defun weaver--get-entry-in-thread (alist format)
  (let* ((address (car format))
         (content (cond ((consp address) (weaver--get-in alist address))
                        ((stringp address) address)
                        ((functionp address) (funcall address alist))))
         (spec (cdr format)))
    (let-alist (if (symbolp spec) (symbol-value spec) spec)
      (let ((weaver-thread-image-max-width
             (if .width (* .width weaver--font-width)
               weaver-thread-image-max-width)))
        (cons (if .reader (funcall .reader content) content)
              (if (plist-member .properties 'face) .properties
                `(face default . ,\.properties)))))))

(defun weaver--print-info (thread-data)
  "Convert `json-read' THREAD-DATA into tabulated-list vector.
The conversion is governed by `weaver-thread-format'."
  (weaver--mapv (lambda (f) (weaver--get-entry-in-thread thread-data f))
          weaver-thread-format))

(defvar-local weaver--pages-so-far 0
  "Number of pages currently being displayed.
This variable gets reset to 0 before every refresh and increased
by one when the user requests more pages (by scrolling beyond the
bottom).")

(defvar-local weaver--paging-function nil
  "Function used to fetch the next page of threads to be displayed.
Used by `weaver-list-mode'. This is a function, called with
no arguments, which returns a list of threads to be displayed.

This function will be called when the user presses \\<weaver-list-mode-map>\\[weaver-list-next] at the end
of the thread list. It should either return nil (indicating
\"no more threads\") or return a list of threads which will
appended to the currently displayed list.

If this is not set, it's the same as a function which always
returns nil.")

(defvar-local weaver--dataset nil
  "The logical data behind the displayed list of threads.
This dataset contains even threads that are hidden by the user,
and thus not displayed in the list of threads.

This is ignored if `weaver--refresh-function' is set.")

(defvar-local weaver--print-function #'weaver--print-info
  "Function to convert a thread alist into a tabulated-list entry.
This gives you fine-tuned customization of how threads are
printed, but it's generally not necessary to configure this. You
can instead rely on the default value and configure
`weaver-thread-format' instead.

If this is set to a different value, it may be necessary to
change `tabulated-list-format' accordingly.")


;;; Mode helpers
(defun weaver--normalize-field-properties (field)
  "Return a new spec for FIELD with proper `properties' entry.
Must be called before populating `tabulated-list-entries'."
  (let ((address (car field))
        (spec (copy-alist (cdr field))))
    (let-alist spec
      (unless (plist-member .properties 'face)
        (setf (alist-get 'properties spec)
              (append '(face default) .properties))))
    (cons address spec)))

(defun weaver--normalize-field-width (field)
  "Return a new spec for FIELD with a `width' entry.
Must be called after populating `tabulated-list-entries'."
  (let ((address (car field))
        (spec (copy-alist (cdr field))))
    (let-alist spec
      (unless (or .width (not (consp tabulated-list-entries)))
        (setf (alist-get 'width spec)
              (if (stringp address)
                  (string-width address)
                (apply #'max (mapcar (lambda (x) (pcase (car (elt (cadr x) .idx))
                                              ((and s (pred stringp)) (string-width s))
                                              (_ 0)))
                                     tabulated-list-entries))))))
    (cons address spec)))

(defvar-local weaver--display-function nil)

(defun weaver--plausible-url-p (url)
  (and (stringp url)
       (not (string-match "\\`https?://api\\." url))
       (y-or-n-p (concat "Visit " url " "))))

(defvar-local weaver--visit-function #'browse-url
  "Function that takes a url and visits it externally.")

(defun weaver-visit-externally (thread)
  "Visit the current thread in a web browser."
  (interactive (list (weaver--data-here)))
  (funcall weaver--visit-function thread))

(defun weaver--data-here ()
  (cond
   ((derived-mode-p 'weaver-list-mode) (tabulated-list-get-id))
   ((derived-mode-p 'weaver-thread-mode) (get-char-property (point) 'weaver--data-here))))


;;; Keys
(defun weaver--key-definitions-to-header-line (definitions)
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


;;; Networking
(defvar weaver--url-cache (make-hash-table))

(defun weaver--read-and-cache-buffer-data (url)
  "Return the buffer contents after any url headers.
Error if url headers are absent or if they indicate something
went wrong."
  (goto-char (point-min))
  (unless (string-match "200" (thing-at-point 'line))
    (error "Page not found."))
  (if (not (search-forward "\n\n" nil t))
      (error "Headers missing; response corrupt")
    (let ((out (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      (puthash url out weaver--url-cache)
      out)))

(defun weaver-get-url (url &optional callback)
  "Fetch and return data stored online at URL.
If CALLBACK is nil, fetching is done synchronously and the
data (buffer contents sans headers) is returned as a string.

Otherwise CALLBACK must be a function of a single argument.  Then
`url-retrieve' is called asynchronously and CALLBACK is passed
the retrieved data."
  (let* ((url-automatic-caching t)
         (url-inhibit-uncompression t)
         (url-request-method "GET")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (callback-internal
          (when callback
            ;; @TODO: Error check in STATUS.
            (lambda (_status)
              (funcall callback (weaver--read-and-cache-buffer-data url)))))
         (response-buffer
          (if callback (url-retrieve url callback-internal nil 'silent)
            (url-retrieve-synchronously url))))
    (unless callback
      (if (not response-buffer)
          (error "Something went wrong in `url-retrieve-synchronously'")
        (with-current-buffer response-buffer
          (weaver--read-and-cache-buffer-data url))))))

(defun weaver-get-cached-url (url &optional callback)
  "Like `weaver-get-url', but allow caching."
  (if-let ((cache (gethash url weaver--url-cache)))
      (if callback
          (funcall callback cache)
        cache)
    (weaver-get-url url callback)))

(defun weaver--shorten-url (url)
  "Shorten URL hiding anything other than the domain.
Paths after the domain are replaced with \"...\".
Anything before the (sub)domain is removed."
  (replace-regexp-in-string
   ;; Remove anything after domain.
   (rx (group-n 1 (and (1+ (any word ".")) "/"))
       (1+ anything) string-end)
   (eval-when-compile
     (concat "\\1" (if (char-displayable-p ?…) "…" "...")))
   ;; Remove anything before subdomain.
   (replace-regexp-in-string
    (rx string-start (or (and (0+ word) (optional ":") "//")))
    "" url)))

(provide 'weaver-core)
;;; weaver-core.el ends here
