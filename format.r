# String formatting helpers used by `sys$print`.

#' Smartly format an object for printing
#'
#' \code{repr} formats an arbitrary object by choosing the appropriate
#' function that either formats or prints it.
#' @param x the object to format
#' @param sep a separator to use when pasting objects of length \code{> 1}.
#' @return \code{repr} returns a character vector of length 1 that contains a
#' concise string representation of \code{x}.
#'
#' @details
#' \code{repr} will dispatch formatting to either the \code{format} or
#' \code{print} generic, depending on which is specialized for the object. It
#' gives preference to \code{format} if both are defined.
#' Finally, it will ensure that the output consists of a single character
#' string; this may require pasting the formatted output, using \code{sep}.
#' @export
repr = function (x, sep = ', ')
    UseMethod('repr')

repr.default = function (x, sep = ', ') {
    get_method = function (generic, class)
        try(getS3method(generic, class), silent = TRUE)

    is_not_error = function (x) ! inherits(x, 'try-error')

    find_method = function (generic, classes)
        Find(is_not_error, lapply(classes, get_method, generic = generic))

    print_ = find_method('print', class(x))
    format_ = find_method('format', class(x))

    result = if(! is.null(format_))
        format_(x)
    else if (! is.null(print_))
        paste(capture.output(print_(x)), collapse = '\n')
    else
        format(x)

    paste(result, collapse = sep)
}

repr.numeric = function (x, sep = ', ')
    paste(format(x, trim = TRUE), collapse = sep)

repr.logical = function (x, sep = ', ')
    paste(format(x, trim = TRUE), collapse = sep)

repr.list = function (x, sep = ', ')
    paste(vapply(x, repr, character(1), sep = sep), collapse = sep)

repr.name = function (x, sep = ', ')
    paste(vapply(x, deparse, character(1), backtick = TRUE), collapse = sep)

repr.call = function (x, sep = ', ')
    paste(capture.output(print(x)), collapse = '\n')

`repr.{` = repr.if = repr.for = repr.while = repr.call
