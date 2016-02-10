cmd = modules::import('../cmdline')
sink() # To show tests, since we’re never calling `sys$run`.

#' Assert that a command line parse call shows the help
shows_help = function ()
    function (x)
        expectation(inherits(x, 'try-error') &&
                    inherits(attr(x, 'condition'), 'sys$cmdline$help') &&
                    ! inherits(attr(x, 'condition'), 'sys$cmdline$error'),
                    'does not show help', 'shows help')

#' Assert that a command line parse call shows an error
#'
#' @param ... exact names of the arguments that should show up in the error
#' message as wrongly used
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

#' Assert that a command line parse call results in a given argument definition
#' list
#'
#' @param ... named definitions of arguments
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

#' Construct a call to \code{sys$cmdline$parse} for testing
#'
#' @param args character vector of the command line arguments
#' @param ... command line argument definition, exactly as provided to
#' \code{sys$cmdline$parse}
#' @return Result of \code{sys$cmdline$parse(..., args = args)}, or a
#' \code{try-error} object.
xc = function (args, ...) {
    call = `[[<-`(match.call(), 1, quote(cmd$parse))
    try(eval.parent(call), silent = TRUE)
}
