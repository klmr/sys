#!/usr/bin/env Rscript

sys = modules::import('sys')

"
🔢  A simple mathematical expression evaluator 📐

Its features

1. evaluation of raw R expressions via `eval`
2. print overly verbose descriptions
"
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
