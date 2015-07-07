;;; ocr-assist-mode.el --- assists in restructuring an OCRed text.

;; Copyright (C) 2015 Christian Lück

;; Author: Christian Lück <christian.lueck@fernuni-hagen.de>
;; Version: 0.1
;; URL: https://github.com/lueck/ocr-assist-mode
;; Keywords: text, ocr

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with standoff-mode. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode which helps you restructuring a plain linear text from
;; optical character recognition. This mode provides interactive
;; commands that let you

;; - put together the mark and text of a footnote


;; Usage:

;; M-x ocr-assist-mode RET activates the mode.

;; C-c , ist bound to the command for putting together the mark and
;; text of a footnote.

;; Customization:

;; The format string that replaces the footnote mark is customized on
;; a major mode basis. This means, that there are different strings
;; for buffers in latex-mode or in nxml-mode etc. E.g. for latex-mode
;; the format string is set to
;; "%\n\\\\footnote{##FOOTNOTETEXT##}\\3%\n".

;; ##FOOTNOTETEXT## ist a placeholder for the text of the footnote and
;; will be replaced with it. The placeholder can be changed by setting
;; the variable `ocr-assist-fn-text-placeholder'. It is recommended to
;; use that variable when defining the format string, e.g. by
;; concatenation:
;; (concat "%\n\\\\footnote{" ocr-assist-fn-text-placeholder } "\\3%\n")

;; The string may contain \N with N of 1...3. These match the Nth
;; group `\(..\)' in `ocr-assist-fn-re'; so \1 provides the whitespace
;; found before the footnote mark, \2 provides the footnote mark, \3
;; provides the whitespace after the mark.

;; E.g. to customize the format string for footnotes used in
;; latex-mode (AucTeX) so that it prints the original number of the
;; footnote in the end of the text, put the following code in your
;; init file:

;; (eval-after-load "ocr-assist-mode"
;;   '(progn
;;      (setq my-fn-replacement-latex (concat "%\n\\\\footnote{"
;; 					   ocr-assist-fn-text-placeholder
;; 					   "\\\\origfn{\\2}}\\3%\n"))
;;      (add-to-list 'ocr-assist-fn-replacement-mode-alist
;; 		  `(latex-mode . ,my-fn-replacement-latex))))


;;; Code

(defgroup ocr-assist nil
  "Customize ocr-assist-mode.
ocr-assist-mode is a minor mode assisting with OCRed texts."
  :group 'Text)

;;;; footnotes

(defvar ocr-assist-fn-re "\\([[:space:]]*\\)\\([0-9]+\\)\\([[:space:]]*\\)")
(defvar ocr-assist-fn-text-number-re "^%s[[:space:]\n]*")

(defconst ocr-assist-fn-text-placeholder "##FOOTNOTETEXT##"
  "A placeholder for the text of footnote.
`ocr-assist-fn-replacement' must contain this string to mark the
place where to put the footnote text.")

(defcustom ocr-assist-fn-more-re "^"
  "Pattern at which the user is asked if the footnote text ends or exceeds.
With the default value of \"^\" this asks at every new line.")

(defvar ocr-assist-fn-replacement nil
  "This variable set per buffer according to its major mode.
See `ocr-assist-fn-replacement-mode-alist' for details.")

(defcustom ocr-assist-fn-replacement-mode-alist
  (let ((txt ocr-assist-fn-text-placeholder))
    `(('latex-mode . ,(format "%%\n\\\\footnote{%s}\\3%%\n" txt))
      ('nxml-mode . ,(format "<footnote label=\"\\2\">%s</footnote>\\3" txt))
      ("default" . ,(format "[\\2: %s]\\3" txt))))
  "Mapping modes to replacement strings.
Note, that one overrides the values of an alist simply by adding
a new key/value pair--no need for deletion.

Put \N (with N of 1..3) in that string to refer to 1) whitespace
before the mark, 2) the mark or 3) whitespace after the
mark. This string must contain `ocr-assist-fn-text-placeholder'
which marks where to put the text in the string.

Example for latex:

(add-to-list 'ocr-assist-fn-replacement-mode-alist
	     `(latex-mode . ,(format \"%%\n\\\\footnote{%s \\\\origfn{\\2}}\\3%%\n\"
				     ocr-assist-fn-text-placeholder)))

If you want to set the replacement string according to an XML
schema you could write a hook to set this variable up."
  :group 'ocr-assist
  :type 'list)

(defun ocr-assist-fn-replacement-set ()
  "Sets `ocr-assist-fn-replacement' according to major mode."
  (setq ocr-assist-fn-replacement
	(or (cdr (assoc major-mode ocr-assist-fn-replacement-mode-alist))
	    ocr-assist-fn-replacement-default)))

(add-hook 'ocr-assist-mode-hook 'ocr-assist-fn-replacement-set)

(defun ocr-assist-footnote ()
  "Search for footnote mark (number) and put it together with the footnote text.
This searches for the next number until user confirms that it is
a footnote mark. Then this mark is replace with the footnote code
defined in `ocr-assist-fn-replacement'. Then it searches for a
line that begins with the same number as the footnote mark until
the user confirms that this is the footnote text. The user then
is asked for the length of the footnote. At the end, the footnote
text is moved to a placeholder in the footnote code."
(interactive)
(let ((number)
      (space)
      (continue t))
  (while continue
    (re-search-forward ocr-assist-fn-re)
    (ocr-assist-highlight (match-beginning 0) (match-end 0))
    (setq number (match-string 2)
	  space (match-string 1))
    (when (y-or-n-p "Footnote Mark?")
      (replace-match ocr-assist-fn-replacement)
      (if (search-backward ocr-assist-fn-text-placeholder nil t)
	  (progn
	    (delete-char (length ocr-assist-fn-text-placeholder))
	    (insert (ocr-assist-fn-search-text number)))
	(error "The variable ocr-assist-fn-replacement doesn't contain \"%s\"" ocr-assist-fn-text-placeholder))
      (setq continue nil)))
  (ocr-assist-dehighlight)))

(defun ocr-assist-fn-search-text (number)
  "Aggregate footnote text for footnote with number NUMBER."
  (save-excursion
    (re-search-forward (format ocr-assist-fn-text-number-re number))
    (ocr-assist-highlight (match-beginning 0) (match-end 0))
    (if (y-or-n-p "Start of of this footnote's text?")
	(let ((fn-number-start (match-beginning 0))
	      (fn-number-end (match-end 0))
	      (transient-mark-mode t)
	      (more t))
	  (push-mark nil nil t)
	  (while more
	    (goto-char (+ (point) 1))
	    (re-search-forward ocr-assist-fn-more-re)
	    (setq more (y-or-n-p "More?")))
	  (ocr-assist-dehighlight)
	  (delete-region fn-number-start fn-number-end)
	  ;; after deletion of number, text starts where number started
	  (delete-and-extract-region fn-number-start (point)))
      (ocr-assist-fn-search-text number))))

;;;; page numbers

(defcustom ocr-assist-pb-re "\n\\([\n[:space:]0-9]*\\)\n"
  "Regex for pagebreaks."
  :group 'ocr-assist
  :type 'string)

(defvar ocr-assist-pb-replacement-par nil)

(defvar ocr-assist-pb-replacement-nopar nil)

(defconst ocr-assist-pb-number-placeholder "##PAGENUMBER##")

(defcustom ocr-assist-pb-replacement-mode-alist
  (let ((pn ocr-assist-pb-number-placeholder))
    `((latex-mode . (,(format " %%\n[%s] %%\n" pn) ,(format "\n\n[%s] %%\n" pn)))
      (nxml-mode . (,(format "&#10;[%s] " pn) ,(format "</p>\n<p>[%s] " pn)))
      ("default" . (,(format "\n%s " pn) ,(format "\n\n%s " pn)))))
  "Mapping strings for replacing page numbers to major modes.
The cdr of the elements of this alist are lists. The first
element is for non-paragraph pagebreaks, the second for
pagebreaks which are paragraphs, too. If the string contains the
value of `ocr-assist-pb-number-placeholder', this substring will
be replaced with a page number which is not read from the OCRed
text, but calculated by the command assisting with
pagebreaks. The groups \(..\) of `ocr-assist-pb-re' are also
accessible, by placing \N in the string where N represents the
Nth matching group.

Example for overriding latex-mode:

 (add-to-list 'ocr-assist-pb-replacement-mode-alist
	      (let ((pn ocr-assist-pb-number-placeholder))
		`(latex-mode . (,(format \" %%\n\\\\pb{%s} %%\n\" pn)
				,(format \"\n\n\\\\pb{%s} %%\n\" pn)))))

"
  :group 'ocr-assist
  :type 'list)

(defun ocr-assist-pb-replacement-set ()
  "Set the pagebreak replacement string according to major mode."
  (let ((repl (or (assoc major-mode ocr-assist-pb-replacement-mode-alist)
		  (assoc "default" ocr-assist-pb-replacement-mode-alist))))
    (setq ocr-assist-pb-replacement-nopar (nth 0 (cdr repl))
	  ocr-assist-pb-replacement-par (nth 1 (cdr repl)))
    (message "pb-replacement: %s" ocr-assist-pb-replacement-nopar)
    ))

(add-hook 'ocr-assist-mode-hook 'ocr-assist-pb-replacement-set)

(defun ocr-assist-pb-inc-number (p)
  "Increment the page number.
Override this function to get other than arabic page numbers."
  (+ p 1))

(defun ocr-assist-pagebreak (startpage)
  "Assist finding pagebreaks and adding a page number in the text."
  (interactive "NStart with page number: ")
  (while (re-search-forward ocr-assist-pb-re)
    (ocr-assist-highlight (match-beginning 0) (match-end 0))
    (when (y-or-n-p (format "Page break %s?" startpage))
      (let ((transient-mark-mode nil))
	(push-mark)
	(if (y-or-n-p "Paragraph?")
	    (replace-match ocr-assist-pb-replacement-par)
	  (replace-match ocr-assist-pb-replacement-nopar))
	(when (search-backward ocr-assist-pb-number-placeholder nil t)
	  ;; ##PAGENUMBER## is not neccessarily included in the replacement
	  (delete-char (length ocr-assist-pb-number-placeholder))
	  (insert (format "%s" startpage)))
	(goto-char (mark))
	(pop-mark)
	(setq startpage (ocr-assist-pb-inc-number startpage))))))

;;;; highlight

(defvar ocr-assist-overlay nil)

(defvar ocr-assist-match-face 'isearch)

(defun ocr-assist-highlight (beg end)
  (if ocr-assist-overlay
      ;; overlay allready exists, just move it
      (move-overlay ocr-assist-overlay beg end (current-buffer))
    ;; overlay doesn't exist, create it
    (setq ocr-assist-overlay (make-overlay beg end))
    (overlay-put ocr-assist-overlay 'priority 1001)
    (overlay-put ocr-assist-overlay 'face ocr-assist-match-face)))

(defun ocr-assist-dehighlight ()
  (when ocr-assist-overlay
    (delete-overlay ocr-assist-overlay)))

;;;; minor mode

;;;###autoload
(define-minor-mode ocr-assist-mode
  "Toggle minor mode that assists in restructuring an OCRed text.

See the command \\[ocr-assist-footnote]."
  ;; initial value
  nil
  ;; mode line indicator
  " OCR"
  ;; key binding
  '(([?\C-c ?,] . ocr-assist-footnote)
    ([?\C-c ?.] . ocr-assist-pagebreak))
  ;; body
  (when ocr-assist-mode
    (setq-local ocr-assist-fn-replacement nil)
    (setq-local ocr-assist-pb-replacement-par nil)
    (setq-local ocr-assist-pb-replacement-nopar nil)))

;;; ocr-assist-mode.el ends here.
