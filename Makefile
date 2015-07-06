.PHONY: compile clean

#VERSION := $(shell git describe --tags)
VERSION := "0.1"

ELPKG := ocr-assist-mode.el

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

package : ocr-assist-mode-$(VERSION).tar

ocr-assist-mode-$(VERSION).tar: ${ELPKG} LICENSE
	tar -cf $@ --transform "s,^,ocr-assist-mode-$(VERSION)/," $^

compile: $(ELC)

clean:
	rm *.tar *.elc
