.PHONY: all
all: test

.PHONY: test
test: cmdline_spec.html

cmdline_spec.html: __init__.r cmdline.r

%.html: %.rmd
	Rscript -e 'knitr::knit2html("$<")'
