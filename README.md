# Command line applications for R

[![Travis CI
status](https://travis-ci.org/klmr/sys.svg?branch=master)](https://travis-ci.org/klmr/sys)



This is a work in progress for a `sys` module that supports command line
applications in R. Inspired by Python, it implements a simple, opinionated
paradigm for implementing command line tools.

To ensure that the code of the command line application is itself reusable,
`sys$run` offers an entry point that is only invoked when the code is run
self-contained from the command line, not when it’s included as a module
(similar to Python’s `if __name__ == '__main__'`). The module also offers a
self-contained command line parser that supports largely the same syntax as
getopt and argparse, but with an R-like API.

This module supports writing tools using code such as the following:


```r
#!/usr/bin/env Rscript

sys = modules::import('sys')

"A simple mathematical expression evaluator"
VERSION = '1.0'

sys$run({
    args = sys$cmd$parse(opt('n', 'allow-nan', 'allow NaN results (otherwise, the program exits with a failure)', FALSE),
                         opt('d', 'digits', 'number of digits to print', 0),
                         arg('expression', 'mathematical expression'))

    result = eval(parse(text = args$expression))
    if (! args$allow_nan && is.nan(result))
        sys$exit(1, 'NaN result invalid')
    sys$printf('%.*f', args$digits, result)
})
```

Saving the code as `calc.r`, making it executable and running
it, via

```bash
./calc.r -d 2 "10 / 3"
```

yields the output `3.33`. Running it with missing
or invalid arguments displays an error message:

```bash
./calc.r
```
```
Usage: ./calc.r [--allow-nan] [--digits=DIGITS] expression
Error: Mandatory argument ‘expression’ not provided
```

Running it via `./calc.r --help` displays a formatted help message, listing the
available options, their description and default values:

```
A simple mathematical expression evaluator (version 1.0)

Usage: ./calc.r [--allow-nan] [--digits=DIGITS] expression

Positional arguments:
  expression         mathematical expression

Options:
  -n, --allow-nan    allow NaN results (otherwise, the program exits with a
                     failure) (default: FALSE)
  -d, --digits       number of digits to print (default: 0)
```

## Status

This module already largely works but the API (in particular that of the command
line parser) are still subject to change. Use at your own risk.
