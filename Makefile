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

.DEFAULT_GOAL := show-help

# Inspired by <http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html>
.PHONY: show-help
show-help:
	@echo "$$(tput bold)Available rules:$$(tput sgr0)"
	@echo
	@sed -n "/^##/ { \
		h; \
		n; \
		s/:.*//; \
		G; \
		s/^/$$(tput setaf 6)/; \
		s/\\n##/$$(tput sgr0)---/; \
		p; \
	}" ${MAKEFILE_LIST} \
	| sort --ignore-case \
	| awk 'BEGIN {FS = "---"} { printf "%-30s\t%s\n", $$1, $$2 }'
