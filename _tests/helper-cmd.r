cmd = modules::import('../cmd')
sink() # To show tests, since we’re never calling `sys$run`.

#' Assert that a command line parse call shows the help
shows_help = function ()
    function (x)
        expect(inherits(x, 'try-error') &&
               inherits(attr(x, 'condition'), 'sys$cmd$help') &&
               ! inherits(attr(x, 'condition'), 'sys$cmd$error'),
               'does not show help', 'shows help')

#' Assert that a command line parse call shows an error
#'
#' @param ... exact names of the arguments that should show up in the error
#' message as wrongly used
shows_error = function (...) {
    args = unlist(list(...))

    function (x) {
        if (inherits(x, 'try-error') &&
            inherits(attr(x, 'condition'), 'sys$cmd$error')) {
            args_present = sapply(sQuote(args), grepl,
                                  x = conditionMessage(attr(x, 'condition')),
                                  perl = TRUE)

            as_expected = all(args_present)
            expect(as_expected,
                   sprintf('argument(s) %s not used incorrectly',
                           paste(sQuote(args[! args_present]),
                                 collapse = ', ')),
                   'used wrong arguments')
        }
        else
            expect(FALSE, 'did not use wrong arguments', '')
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
        expect(identical(na, ne) &&
               identical(actual[na], expected[ne]),
               'arguments are not equal', 'arguments are equal')
    }
}

#' Construct a call to \code{sys$cmd$parse} for testing
#'
#' @param args character vector of the command line arguments
#' @param ... command line argument definition, exactly as provided to
#' \code{sys$cmd$parse}
#' @return Result of \code{sys$cmd$parse(..., args = args)}, or a
#' \code{try-error} object.
xc = function (args, ...) {
    call = `[[<-`(match.call(), 1, quote(cmd$parse))
    result = try(eval.parent(call), silent = TRUE)
    if (inherits(result, 'try-error') &&
        inherits(attr(result, 'condition'), 'simpleError'))
        stop(result)
    result
}
