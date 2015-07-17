# Command line applications for R

This is a work in progress for a `sys` module that supports command line
applications in R. Inspired by Python, it implements a simple, opinionated
paradigm for implementing command line tools.

To ensure that the code of the command line application is itself reproducible,
`sys$run` offers an entry point that is only invoked when the code is run
self-contained from the command line, not when it’s included as a module
(similar to Python’s `if __name__ == '__main__'`). The module also offers a
self-contained command line parser that supports largely the same syntax as
getopt and argparse, but with an R-like API.

Once the module is ready, the following code should be able to run:

```r
#!/usr/bin/env Rscript

sys = modules::import('sys')

sys$run({
    sys$cmdline$describe('A simple mathematical expression evaluator')
    args = sys$cmdline$parse(opt('n', 'allow-nan', 'allow NaN results (otherwise, the program exits with a failure)', FALSE),
                             opt('d', 'digits', 'number of digits to print', 0),
                             arg('expression', 'mathematical expression'))

    result = eval(parse(text = args$expression))
    if (! args$`allow-nan` && is.nan(result))
        sys$exit(1, 'NaN result invalid')
    message(round(result, args$digits))
})
```

Saving the code as `calc.r`, making it executable and running it, via

    ./calc.r -d 2 '10 / 3'

would yield the output `3.33`. Running it without arguments, with invalid
arguments or via `./calc.r --help` would display a help message, listing the
available options, their description and default values.
