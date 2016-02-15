.PHONY: all
all: test documentation README.md

module_files = __init__.r cmd.r

.PHONY: test
test: ${module_files}
	Rscript __init__.r

.PHONY: documentation
documentation: cmdline_spec.html

cmdline_spec.html: ${module_files}

README.md: README.rmd ${module_files}
	Rscript -e 'knitr::knit("$<", "$@")'

%.html: %.rmd
	Rscript -e 'knitr::knit2html("$<")'
