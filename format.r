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
#' gives preference to \code{print}.
#' Finally, it will ensure that the output consists of a single character
#' string; this may require pasting the formatted output, using \code{sep}.
repr = function (x, sep = ', ') {
    get_method = function (generic, class)
        try(getS3method(generic, class), silent = TRUE)

    is_not_error = function (x) ! inherits(x, 'try-error')

    print = Find(is_not_error,
                  lapply(class(x), get_method, generic = 'print'))

    if(is.null(print))
        paste(format(x), collapse = sep)
    else
        paste(capture.output(print(x)), collapse = '\n')
}
