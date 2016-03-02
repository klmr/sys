.PHONY: all
## Execute all relevant rules
all: test documentation README.md

module_files = __init__.r cmd.r

.PHONY: test
## Run unit tests
test: ${module_files}
	Rscript __init__.r

.PHONY: documentation
## Build HTML documentation
documentation: cmdline_spec.html

cmdline_spec.html: ${module_files}

## Generate README from RMarkdown source
README.md: README.rmd ${module_files}
	Rscript -e 'knitr::knit("$<", "$@")'

%.html: %.rmd
	Rscript -e 'knitr::knit2html("$<")'
