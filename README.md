ocr-assist-mode
===============

ocr-assist-mode is a minor mode for GNU Emacs which helps you
restructuring a plain linear text from optical character
recognition. This mode provides interactive commands that let you

- put together the mark and text of a footnote

- handle page breaks and page numbers

## Usage ##

`M-x ocr-assist-mode RET` activates the mode.

`C-c ,` ist bound to the command for putting together the mark and
text of a footnote.

## Customization ##

The format string that replaces the footnote mark is customized on
a major mode basis. This means, that there are different strings
for buffers in latex-mode or in nxml-mode etc. E.g. for `latex-mode`
the format string is set to
`"%\n\\\\footnote{##FOOTNOTETEXT##}\\3%\n"`.

`##FOOTNOTETEXT##` ist a placeholder for the text of the footnote and
will be replaced with it. The placeholder can be changed by setting
the variable `ocr-assist-fn-text-placeholder`. It is recommended to
use that variable when defining the format string, e.g. by
concatenation:

`(concat "%\n\\\\footnote{" ocr-assist-fn-text-placeholder "}\\3%\n")`

The string may contain `\N` with `N` of `1...3`. These match the Nth
group `\(..\)` in `ocr-assist-fn-re`; so `\1` provides the whitespace
found before the footnote mark, `\2` provides the footnote mark, `\3`
provides the whitespace after the mark.

E.g. to customize the format string for footnotes used in
`latex-mode` (AucTeX) so that it prints the original number of the
footnote in the end of the text, put the following code in your
init file:


    (eval-after-load "ocr-assist-mode"
      '(progn
         (setq my-fn-replacement-latex (concat "%\n\\\\footnote{"
    					   ocr-assist-fn-text-placeholder
    					   "\\\\origfn{\\2}}\\3%\n"))
         (add-to-list 'ocr-assist-fn-replacement-mode-alist
    		  `(latex-mode . ,my-fn-replacement-latex))))

