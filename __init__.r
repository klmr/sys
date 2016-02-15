#' The command line arguments
args = commandArgs(trailingOnly = TRUE)

#' The name of the script
#'
#' @note If the script was invoked interactively, this is the empty string.
script_name = local({
    file = grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
    sub('--file=', '', file)
})

#' The description of the script
#'
#' @return The description of the script, if provided, \code{NULL} otherwise.
description = function () {
    if (length(.script_output) == 0)
        return(NULL)
    if (! grepl('^\\[1\\] ".*"$', .script_output[[1]]))
        return(NULL)

    sub('^\\[1\\] "(.*)"$', '\\1', .script_output[[1]])
}

# The environment that is calling `import(sys)`
.script_env = local({
    n = 1

    while (isNamespace(topenv((env = parent.frame(n)))))
        n = n + 1
    env
})

#' The version of the script
#'
#' @return The version of the script, if provided, \code{NULL} otherwise.
version = function ()
    mget('VERSION', .script_env, mode = 'character', ifnotfound = list(NULL),
         inherits = FALSE)[[1]]

#' Quit the program
#'
#' @param code numeric exit code (default: \code{0})
#' @param msg message to be printed to the standard error (optional)
exit = function (code = 0, msg) {
    if (! missing(msg))
        message(msg)
    quit(save = 'no', status = if (is.null(code)) 0 else code)
}

#' Print a message
#'
#' \code{print} outputs a message consisting of several fragments.
#' @param ... parts of the message
#' @param file a connection or character string specifying a file name or a pipe
#'  (see \code{\link{cat}} for details)
#' @param nl logical; if \code{TRUE}, append a newline character
print = function (..., file = stdout(), nl = TRUE)
    cat(..., if(nl) '\n', file = file, sep = '')

#' \code{printf} outputs a formatted message.
#' @param format a format string that is passed to \code{\link{sprintf}}
#' @rdname print
printf = function (format, ..., file = stdout(), nl = TRUE)
    print(sprintf(format, ...), file = file, nl = nl)

#' Execute the \code{entry_point} function defined by the caller
#'
#' Execute an entry point function, but only if the calling code is executed as
#' a stand-alone script, not when it is imported as a module or sourced in
#' interactive mode.
#'
#' @param entry_point code or function to run (default: \code{main})
#' @return This function is called for its side-effect. If the calling code was
#' imported as a module, then return nothing. Otherwise, this function
#' \emph{does not return}; instead, it quits the script.
#'
#' @details The argument may either be a function (the default is assumed to be
#' a function called \code{main} in the calling code’s scope) or a
#' \emph{brace-enclosed} expression. It is executed in the calling code’s scope.
#'
#' @examples
#' \dontrun{
#' main = function () { … }
#' # Run function with name `main`
#' sys$run()
#'
#' # Run the specified function
#' sys$run(function () { … })
#'
#' # Run the specified code
#' sys$run({ … })
#'
#' # Quit with error
#' sys$run({
#'     sys$exit(1, 'Optional message')
#' })
#' }
run = function (entry_point = main) {
    caller = parent.frame()
    caller_name = evalq(modules::module_name(), envir = caller)

    if (interactive() || ! is.null(caller_name))
        return(invisible())

    # Restore standard output once script entry point is running.
    sink()

    error = tryCatch({
        if (class(substitute(entry_point)) == '{') {
            # Evaluating `entry_point` is wrapped into a function call so that,
            # when evaluation is aborted by a call to `stop` inside the entry
            # point expression, the call stack will contain `run()`, rather than
            # an obscure `doTryCatch(…)` expression.
            run = function () entry_point
            run()
        }
        else
            eval(substitute(main(), list(main = entry_point)), envir = caller)
        exit()
    }, error = identity)

    if (inherits(error, 'sys$cmd$help')) {
        is_error = inherits(error, 'sys$cmd$error')
        base::print(error, file = if (is_error) stderr() else stdout())
        if (is_error)
            exit(1)
    }
    else
        stop(error)
}

cmd = modules::import('./cmd')

if (is.null(modules::module_name())) {
    modules::import('./_tests')
} else if (! interactive()) {
    # Redirect all output from the script. This is done to prevent unintended
    # clutter, but also to capture the script “description”.
    script_output_conn = textConnection('.script_output',
                                        open = 'w',
                                        local = TRUE,
                                        encoding = 'UTF-8')
    sink(file = script_output_conn, type = 'output')
} else {
    .script_output = ''
}
