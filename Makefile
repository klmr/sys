.PHONY: all
all: test documentation

module_files = __init__.r cmdline.r

.PHONY: test
test: ${module_files}
	Rscript __init__.r

.PHONY: documentation
documentation: cmdline_spec.html

cmdline_spec.html: ${module_files}

%.html: %.rmd
	Rscript -e 'knitr::knit2html("$<")'
