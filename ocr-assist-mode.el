;;; ocr-assist-mode.el --- assists you with restructuring an OCRed text.

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
;; <http://www.gnu.org/

;;; Commentary:

;; A minor mode which helps you restructuring a plain linear text from
;; optical character recognition. This mode provides interactive
;; commands that let you

;; - put together the mark and text of a footnote

;;; Code

(defvar ocr-assist-fn-re "\\([[:space:]]*\\)\\([0-9]+\\)\\([[:space:]]*\\)")
(defvar ocr-assist-fn-text-number-re "^%s[[:space:]\n]*")

(defconst ocr-assist-fn-text-placeholder "##FOOTNOTETEXT##")

(defvar ocr-assist-fn-replacement 
  (concat "%\n\\\\footnote{" ocr-assist-fn-text-placeholder "\\\\ofn{\\2}}\\3%\n"))

(defvar ocr-assist-fn-more-re "^")

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
	    (insert (ocr-assist-fn-search-text number (point))))
	(error "The variable ocr-assist-fn-replacement doesn't contain \"%s\"" ocr-assist-fn-text-placeholder))
      (setq continue nil)))
  (ocr-assist-dehighlight)))

(defun ocr-assist-fn-search-text (number fn-point)
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
      (ocr-assist-fn-search-text number (point)))))

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
  "Toggle minor mode assisting with an OCRed text."
  ;; initial value
  nil
  ;; mode line indicator
  " OCR"
  ;; key binding
  '(([?\C-c ?,] . ocr-assist-footnote)))

;;; ocr-assist-mode.el ends here.
