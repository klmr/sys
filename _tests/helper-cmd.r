cmd = modules::import('../cmdline')
sink() # To show tests, since we’re never calling `sys$run`.

shows_help = function ()
    function (x)
        expectation(inherits(x, 'try-error') &&
                    inherits(attr(x, 'condition'), 'sys$cmdline$help') &&
                    ! inherits(attr(x, 'condition'), 'sys$cmdline$error'),
                    'does not show help', 'shows help')

shows_error = function (...) {
    args = unlist(list(...))

    function (x) {
        if (inherits(x, 'try-error') &&
            inherits(attr(x, 'condition'), 'sys$cmdline$error')) {
            args_present = sapply(sQuote(args), grepl,
                                  x = conditionMessage(attr(x, 'condition')),
                                  perl = TRUE)

            as_expected = all(args_present)
            expectation(as_expected,
                        sprintf('argument(s) %s not used incorrectly',
                                paste(sQuote(args[! args_present]),
                                      collapse = ', ')),
                        'used wrong arguments')
        }
        else
            expectation(FALSE, 'did not use wrong arguments', '')
    }
}

args_equal = function (...) {
    expected = list(...)
    function (actual) {
        na = sort(names(actual))
        ne = sort(names(expected))
        expectation(identical(na, ne) &&
                    identical(actual[na], expected[ne]),
                    'arguments are not equal', 'arguments are equal')
    }
}

xc = function (args, ...) {
    call = `[[<-`(match.call(), 1, quote(cmd$parse))
    try(eval.parent(call), silent = TRUE)
}
