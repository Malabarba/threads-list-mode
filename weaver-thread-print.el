;;; weaver-thread-print.el --- Printing threads             -*- lexical-binding: t; -*-

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

(require 'markdown-mode)
(require 'weaver-core)

(defgroup weaver-thread nil
  "Customization group for weaver-thread."
  :prefix "weaver-thread-"
  :tag "WEAVER Question Mode"
  :group 'weaver)

(defgroup weaver-thread-faces '((weaver-user custom-group))
  "Customization group for the faces of `weaver-thread'.
Some faces of this mode might be defined in the `weaver-user' group."
  :prefix "weaver-thread-"
  :tag "WEAVER Question Mode Faces"
  :group 'weaver-thread)


;;; Faces and Variables
(defface weaver-thread-header
  '((t :inherit font-lock-variable-name-face))
  "Face used on the question headers in the question buffer."
  :group 'weaver-thread-faces)

(defface weaver-thread-title
  '((t :weight bold :inherit default))
  "Face used on the question title in the question buffer."
  :group 'weaver-thread-faces)

(defface weaver-thread-content-face
  '((((background dark)) :background "#090909")
    (((background light)) :background "#f4f4f4"))
  "Face used on the question body in the question buffer.
This shouldn't have a foreground, or this will interfere with
font-locking."
  :group 'weaver-thread-faces)

(defcustom weaver-thread-separator
  (concat (propertize (make-string 72 ?\s)
                      'face '(underline weaver-thread-header))
          "\n")
  "Separator used between header and body."
  :type 'string
  :group 'weaver-thread)

(defface weaver-thread-closed
  '((t :box 2 :inherit font-lock-warning-face))
  "Face used for closed question header in the question buffer."
  :group 'weaver-thread-faces)

(defcustom weaver-thread-pretty-links t
  "If non-nil, markdown links are displayed in a compact form."
  :type 'boolean
  :group 'weaver-thread)

(defcustom weaver-thread-use-images
  (eval-when-compile
    (image-type-available-p 'imagemagick))
  "Non-nil if WEAVER should download and display images.
By default, this is `t' if the `imagemagick' image type is
available (checked with `image-type-available-p').  If this image
type is not available, images won't work."
  :type 'boolean
  :group 'weaver-thread)

(defcustom weaver-thread-image-max-width 550
  "Maximum width, in pixels, of images in the question buffer."
  :type 'integer
  :group 'weaver-thread)


;;; Functions
;;;; Printing the general structure
(defconst weaver--thread-closed-mode-line-string
  '(:propertize "  [CLOSED]  " face font-lock-warning-face)
  "String indicating closed questions in the mode-line.")

(defvar-local weaver--overlays nil
  "Overlays created by weaver on this buffer.")

(defun weaver--thread-print ()
  "Print a buffer describing QUESTION.
QUESTION must be a data structure returned by `json-read'."
  ;; Clear the overlays
  (mapc #'delete-overlay weaver--overlays)
  (setq weaver--overlays nil)
  ;; Print everything
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mapc #'weaver--thread-print-section weaver--dataset)))
;; (insert-text-button "Write an Answer" :type 'weaver-button-answer)

(defvar-local weaver--overlay-printing-depth 0
  "Track how many overlays we're printing on top of each other.
Used for assigning higher priority to inner overlays.")

(defmacro weaver--wrap-in-overlay (properties &rest body)
  "Start a scope with overlay PROPERTIES and execute BODY.
Overlay is pushed on the buffer-local variable `weaver--overlays' and
given PROPERTIES.

Return the result of BODY."
  (declare (indent 1) (debug t))
  `(let ((p (point-marker))
         (result (progn ,@body))
         ;; The first overlay is the shallowest. Any overlays created
         ;; while the first one is still being created go deeper and
         ;; deeper.
         (weaver--overlay-printing-depth (1+ weaver--overlay-printing-depth)))
     (let ((ov (make-overlay p (point)))
           (props ,properties))
       (while props
         (overlay-put ov (pop props) (pop props)))
       ;; Let's multiply by 10 just in case we ever want to put
       ;; something in the middle.
       (overlay-put ov 'priority (* 10 weaver--overlay-printing-depth))
       (push ov weaver--overlays))
     result))

(defvar weaver-button-map
  (let ((map (copy-keymap button-map)))
    (define-key map "w" #'weaver-button-copy)
    map)
  "Keymap used on buttons.")

(defun weaver-button-copy ()
  "Copy the content of thing at point.
This is usually a link's URL, or the content of a code block."
  (interactive)
  (let ((content
         (get-text-property (point) 'weaver-button-copy)))
    (if (null content)
        (message "Nothing to copy here.")
      (kill-new content)
      (message "Copied %s to kill ring."
               (or (get-text-property
                    (point) 'weaver-button-copy-type)
                   content)))))

(defun weaver-button-follow-link (&optional pos)
  "Follow link at POS.  If POS is nil, use `point'."
  (interactive)
  (let ((url (or (get-text-property (or pos (point)) 'weaver-button-url)
                 (user-error "No url under point: %s"
                             (or pos (point))))))
    (browse-url url)))

(defconst weaver-button--help-echo
  (concat "mouse-1, RET" 
          (propertize ": %s -- " 'face 'minibuffer-prompt)
          "w" 
          (propertize ": copy %s" 'face 'minibuffer-prompt))
  "Base help-echo on which others can be written.")

(defconst weaver--button-section-help-echo
  (format weaver-button--help-echo
          "hide content"
          "link")
  "Help echoed in the minibuffer when point is on a section.")

(define-button-type 'weaver-button
  'follow-link t
  'keymap weaver-button-map)

(declare-function weaver-thread-hide-show-section "weaver-thread")
(define-button-type 'weaver-section-title
  'action    #'weaver-thread-hide-show-section
  'help-echo weaver--button-section-help-echo
  'weaver-button-copy-type "Share Link"
  :supertype 'weaver-button)

(defun thread--insert-header (spec)
  (pcase-let ((`(,head . ,rest) spec))
    (if (overlayp head) " ¶ "
      (apply #'propertize head
             (if (plist-member rest 'face) rest
               (append rest '(face weaver-thread-header)))))
    (when (overlayp head)
      (move-overlay head (- (point) 3) (point) (current-buffer)))))

(defun weaver--thread-print-section (data)
  "Print a section corresponding to DATA.
DATA can represent a question or an answer."
  ;; This makes `data' accessible through `weaver--data-here'.
  (pcase-let ((`(,raw-data ,body ,headers) data))
    (weaver--wrap-in-overlay (list 'weaver--data-here raw-data)
      (insert "\n")
      (let ((beg (point)))
        (mapcar #'thread--insert-header headers)
        (make-text-button beg (point)
                          'weaver--thread-section 1
                          ;; 'face 'weaver-thread-title
                          :type 'weaver-section-title))
      (when body
        (weaver--wrap-in-overlay '(weaver--thread-section-content t)
          ;; Body
          (insert "\n" weaver-thread-separator)
          (weaver--wrap-in-overlay '(face weaver-thread-content-face)
            (insert "\n")
            (weaver--thread-insert-markdown (replace-regexp-in-string "\r\n?" "\n" body))
            (insert "\n" weaver-thread-separator)))))))


;;;; Printing and Font-locking the content (body)
(defvar weaver-thread-bullet-appearance
  (propertize (if (char-displayable-p ?•) "•" "*")
              'face 'markdown-list-face)
  "String to be displayed as the bullet of markdown list items.")

(defconst weaver--thread-reference-regexp
  (rx line-start (0+ blank) "[%s]:" (0+ blank)
      (group-n 1 (1+ (not (any blank "\n\r")))))
  "Regexp used to find the url of labeled links.
E.g.:
   [1]: https://...")

(defconst weaver--thread-link-regexp
  ;; Done at compile time.
  (rx (or (and (opt "!") "[" (group-n 1 (1+ (not (any "[]")))) "]"
               (or (and "(" (group-n 2 (1+ (not (any ")")))) ")")
                   (and "[" (group-n 3 (1+ (not (any "]")))) "]")))
          (group-n 4 (and "http" (opt "s") "://"
                          (>= 2 (any lower numeric "_%"))
                          "."
                          (>= 2 (any lower numeric "_%"))
                          (* (any lower numeric "-/._%&#?=;"))))))
  "Regexp matching markdown links.")

(defun weaver--thread-process-line-breaks (beg end-marker)
  "Process Markdown line breaks between BEG and END-MARKER.
Double space at the end of a line becomes an invisible \"\\n\".
Consecutive blank lines beyond the first are consensed.
Assumes `marker-insertion-type' of END-MARKER is t."
  (goto-char beg)
  (while (search-forward-regexp
          (rx line-start (* blank) "\n"
              (group-n 1 (+ (any blank "\n"))))
          end-marker 'noerror)
    ;; An invisible newline ensures the previous text
    ;; will get filled as a separate paragraph.
    (replace-match "" nil nil nil 1))
  (goto-char beg)
  (while (search-forward-regexp "  $" end-marker 'noerror)
    ;; An invisible newline ensures the previous text
    ;; will get filled as a separate paragraph.
    (replace-match (propertize "\n" 'invisible t))))

(defun weaver--thread-process-markdown-in-region (beg end)
  "Process Markdown text between BEG and END.
This does not do Markdown font-locking.  Instead, it fills text,
propertizes links, inserts images, cleans up html comments, and
font-locks code-blocks according to mode."
  ;; Paragraph filling
  (let ((paragraph-start
         "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.[ \t]\\|[ \t]*: ")
        (paragraph-separate "\\(?:[ \t\f]*\\|.*  \\)$")
        (adaptive-fill-first-line-regexp "\\`[ \t]*>[ \t]*?\\'")
        (adaptive-fill-function #'markdown-adaptive-fill-function)) 
    (save-restriction
      (narrow-to-region beg end)
      ;; html tags can span many paragraphs, so we handle them
      ;; globally first.
      (weaver--thread-process-html-tags (point-min) (copy-marker (point-max)))
      ;; And now the filling and other handlings.
      (goto-char (point-min))
      (while (null (eobp))
        ;; Don't fill pre blocks.
        (unless (weaver--thread-dont-fill-here)
          (let ((beg (point)))
            (skip-chars-forward "\r\n[:blank:]")
            (forward-paragraph)
            (let ((end (point-marker)))
              (set-marker-insertion-type end t)
              ;; Turn markdown linebreaks into their final form
              (weaver--thread-process-line-breaks beg end)
              ;; Compactify links by paragraph, so we don't linkify
              ;; inside code-blocks. This will still linkify inside
              ;; code tags, unfortunately.
              (weaver--thread-process-links beg end)
              ;; Filling is done after all of the above, since those
              ;; steps change the length of text.
              (fill-region beg end)
              (goto-char end)))))
      (goto-char (point-max)))))

(defun weaver--thread-insert-markdown (text)
  "Return TEXT fontified according to `markdown-mode'."
  (let ((beg (point)))
    (insert
     ;; Font-locking needs to be done in a temp buffer, because it
     ;; affects the entire buffer even if we narrow.
     (with-temp-buffer
       (insert text)
       ;; Trim whitespace
       (goto-char (point-max))
       (skip-chars-backward "\r\n[:blank:]")
       (delete-region (point) (point-max))
       (goto-char (point-min))
       (skip-chars-forward "\r\n[:blank:]")
       (forward-line 0)
       (delete-region (point-min) (point))
       ;; Font lock
       (delay-mode-hooks (markdown-mode))
       (font-lock-mode -1)
       (when weaver-thread-bullet-appearance
         (font-lock-add-keywords ;; Bullet items.
          nil
          `((,(rx line-start (0+ blank) (group-n 1 (any "*+-")) blank)
             1 '(face nil display ,weaver-thread-bullet-appearance) prepend))))
       (font-lock-add-keywords ;; Highlight usernames.
        nil
        `((,(rx (or blank line-start)
                (group-n 1 (and "@" (1+ (not space))))
                symbol-end)
           1 font-lock-builtin-face)))
       ;; Everything.
       (font-lock-fontify-region (point-min) (point-max))
       (replace-regexp-in-string "[[:blank:]]+\\'" "" (buffer-string))))
    ;; This part can and should be done in place, this way it can
    ;; create overlays.
    (weaver--thread-process-markdown-in-region beg (point))))


;;; HTML tags
(defconst weaver--thread-html-tag-regexp
  (rx "<" (group-n 1 "%s") (* (not (any ">"))) ">"))

(defface weaver-thread-sub-sup-tag
  '((t :height 0.7))
  "Face used on <sub> and <sup> tags."
  :group 'weaver-thread-faces)

(defface weaver-thread-kbd-tag
  '((((background dark))
     :height 0.9
     :weight semi-bold
     :box (:line-width 3 :style released-button :color "gray30"))
    (((background light))
     :height 0.9
     :weight semi-bold
     :box (:line-width 3 :style released-button :color "gray70")))
  "Face used on <kbd> tags."
  :group 'weaver-thread-faces)

(defun weaver--thread-inside-code-p ()
  "Return non-nil if point is inside code.
This can be inline Markdown code or a Markdown code-block."
  (save-match-data
    (or (markdown-code-at-point-p)
        (save-excursion
          (weaver--thread-skip-and-fontify-pre 'dont-fontify)))))

(defun weaver--thread-process-html-tags (beg end-marker)
  "Hide all html tags between BEG and END and possibly interpret them.
END-MARKER should be a marker."
  ;; This code understands nested html, but not if the same tag is
  ;; nested in itself (e.g., <kbd><kbd></kbd></kbd>).
  (set-marker-insertion-type end-marker t)
  (goto-char beg)
  (while (search-forward-regexp
          (format weaver--thread-html-tag-regexp "[[:alpha:]]+")
          end-marker 'noerror)
    (unless (weaver--thread-inside-code-p)
      (let ((tag (match-string 1))
            (l   (match-beginning 0)))
        (replace-match "")
        (when (search-forward-regexp
               (format weaver--thread-html-tag-regexp (concat "/" tag))
               ;; Searching for a match has no bounds.
               end-marker 'noerror)
          (let ((r (copy-marker (match-beginning 0))))
            ;; The code tag is special, because it quotes everything inside.
            (if (string= tag "code")
                (progn (replace-match "`")
                       (save-excursion (goto-char l) (insert "`")))
              (replace-match "")
              ;; Handle stuff between the two tags.
              (save-match-data (weaver--thread-process-html-tags l r))
              (cond
               ((string= tag "kbd")
                (add-text-properties l r '(face weaver-thread-kbd-tag))
                (when (looking-at-p
                       (format weaver--thread-html-tag-regexp "kbd"))
                  (insert " ")))
               ((string= tag "sub")
                (add-text-properties
                 l r '(face weaver-thread-sub-sup-tag display (raise -0.3))))
               ((string= tag "sup")
                (add-text-properties
                 l r '(face weaver-thread-sub-sup-tag display (raise +0.3))))))))))))


;;; Handling links
(defun weaver--thread-process-links (beg end-marker)
  "Turn all markdown links between BEG and ENG into compact format.
Image links are downloaded and displayed, if
`weaver-thread-use-images' is non-nil.
Assumes `marker-insertion-type' of END-MARKER is t."
  (goto-char beg)
  (while (search-forward-regexp weaver--thread-link-regexp end-marker t)
    ;; Other links are link-buttons.
    (let* ((text (match-string-no-properties 1))
           (url (or (match-string-no-properties 2)
                    (match-string-no-properties 4)
                    (weaver-thread-find-reference
                     (match-string-no-properties 3)
                     text)))
           (full-text (match-string-no-properties 0))
           (image-p (and weaver-thread-use-images
                         (eq ?! (elt full-text 0)))))
      (when (stringp url)
        (replace-match "")
        (weaver--thread-insert-link
         (cond (image-p (weaver--thread-create-image url))
               ((and weaver-thread-pretty-links text))
               ((not text) (weaver--shorten-url url))
               (t full-text))
         url)))))

(defun weaver--thread-create-image (url)
  "Get and create an image from URL and insert it at POINT.
The image will take the place of the character at POINT.
Its size is bound by `weaver-thread-image-max-width' and
`window-body-width'."
  (let* ((ov (make-overlay (point) (point) (current-buffer) t nil))
         (callback
          (lambda (data)
            (let* ((image (create-image data 'imagemagick t))
                   (image-width (car (image-size image 'pixels))))
              (overlay-put
               ov 'display
               (append image
                       (list :width (min weaver-thread-image-max-width
                                         (window-body-width nil 'pixel)
                                         image-width))))))))
    (weaver-get-url url callback)
    (overlay-put ov 'face 'default)
    ov))

(define-button-type 'weaver-button-link
  'action    #'weaver-button-follow-link
  :supertype 'weaver-button)

(defconst weaver-button--link-help-echo
  (format weaver-button--help-echo
          "visit %s"
          "URL")
  "Help echoed in the minibuffer when point is on a section.")

(defun weaver--thread-insert-link (text url)
  "Return a link propertized version of TEXT-OR-IMAGE.
URL is used as 'help-echo and 'url properties."
  ;; Try to handle an image/link inside another link.
  (when (eq (char-before) ?\[)
    (insert "a")
    (forward-char -2)
    (if (looking-at weaver--thread-link-regexp)
        (progn (setq url (or (match-string-no-properties 2)
                             (match-string-no-properties 4)
                             (weaver-thread-find-reference
                              (match-string-no-properties 3)
                              (if (stringp text) text "¶"))
                             url))
               (replace-match ""))
      (forward-char 1)
      (delete-char 1)))
  (unless (stringp text)
    ;; Images need to be at the start of a line.
    (unless (looking-at-p "^") (insert "\n"))
    ;; And need an empty line above so they don't get wrapped into
    ;; text when we do filling.
    (insert (propertize "\n" 'display "")))
  ;; Insert the link button.
  (insert-text-button (if (stringp text) text "¶")
                      ;; Mouse-over
                      'help-echo
                      (format weaver-button--link-help-echo
                              ;; If TEXT is a shortened url, we don't shorten URL.
                              (propertize (if (and (stringp text)
                                                   (string-match "^https?:" text))
                                              url (weaver--shorten-url url))
                                          'face 'font-lock-function-name-face))
                      ;; For visiting and stuff.
                      'weaver-button-url url
                      'weaver-button-copy url
                      :type 'weaver-button-link)
  ;; Images need to be at the end of a line too.
  (unless (stringp text)
    (move-overlay text (1- (point)) (point) (current-buffer))
    (insert (propertize "\n\n" 'display "\n"))))

(defun weaver-thread-find-reference (id &optional fallback-id)
  "Find url identified by reference ID in current buffer.
If ID is nil, use FALLBACK-ID instead."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (search-forward-regexp
             (format weaver--thread-reference-regexp
                     (or id fallback-id))
             nil t)
        (match-string-no-properties 1)))))


;;; Things we don't fill
(defun weaver--thread-dont-fill-here ()
  "If text shouldn't be filled here, return t and skip over it."
  (catch 'weaver-thread-done
    (let ((before (point)))
      (skip-chars-forward "\r\n[:blank:]")
      (let ((first-non-blank (point)))
        (dolist (it '(weaver--thread-skip-and-fontify-pre
                      weaver--thread-skip-headline
                      weaver--thread-skip-references
                      weaver--thread-skip-comments))
          ;; If something worked, keep point where it is and return t.
          (if (funcall it) (throw 'weaver-thread-done t)
            ;; Before calling each new function. Go back to the first
            ;; non-blank char.
            (goto-char first-non-blank)))
        ;; If nothing matched, go back to the very beginning.
        (goto-char before)
        ;; And return nil
        nil))))

(defun weaver--thread-skip-and-fontify-pre (&optional _dont-fontify)
  "If there's a pre block ahead, handle it, skip it and return t.
Handling means to turn it into a button and remove erroneous
font-locking.

If DONT-FONTIFY is non-nil, just return the result and possibly
move point, don't create the code-block button."
  (let ((beg (line-beginning-position)))
    ;; To identify code-blocks we need to be at start of line.
    (goto-char beg)
    (when (markdown-match-pre-blocks (line-end-position))
      ;; (unless dont-fontify
      ;;   (weaver-babel--make-pre-button beg (point)))
      t)))

(defun weaver--thread-skip-comments ()
  "If there's an html comment ahead, skip it and return t."
  ;; @TODO: Handle the comment.
  ;; "Handling means to store any relevant metadata it might be holding."
  (let ((end (save-excursion
               (when (markdown-match-comments (line-end-position))
                 (point)))))
    (when end
      (delete-region (point) end)
      (skip-chars-backward "[:blank:]")
      (when (looking-at "^[:blank:]*\n")
        (replace-match ""))
      t)))

(defun weaver--thread-skip-headline ()
  "If there's a headline ahead, skip it and return non-nil."
  (when (or (looking-at-p "^#+ ")
            (progn (forward-line 1) (looking-at-p "===\\|---")))
    ;; Returns non-nil.
    (forward-line 1)))

(defun weaver--thread-skip-references ()
  "If there's a reference ahead, skip it and return non-nil."
  (forward-line 0)
  (when (looking-at-p (format weaver--thread-reference-regexp ".+"))
    ;; Returns non-nil
    (forward-paragraph 1)
    t))

(provide 'weaver-thread-print)
;;; weaver-thread-print.el ends here
