.sys = modules::import('../sys')

#' Parse the command line arguments
#'
#' Parses the command line arguments provided to the application in accordance
#' with a given command line specification. If the command line arguments do not
#' conform with the specification, quit and print an error message.
#'
#' @param ... command line specification (see \code{Details})
#' @return A named list of options and their associated value. Missing command
#' line options are filled by their default value, if provided in the
#' definition. If there was an error parsing the command line arguments, this
#' function does not return and signals an error instead.
#'
#' @details
#' The arguments consist of one or more command line definition items. Each item
#' is created by a call to the functions \code{\link{opt}} and
#' \code{\link{arg}}. These do \emph{not} need to be qualified with their full
#' module name (see \code{Examples}).
#'
#' Optionally, the last argument can be a character vector specifying the
#' command line arguments. If this is not given, then \code{sys$args} is used
#' instead. In normal usage, this argument is omitted.
#'
#' @examples
#' sys = import('sys')
#' \dontrun{
#' args = sys$cmdline$parse(sys$cmdline$arg('file', 'the input file'))
#'
#' # Or, equivalently, without qualifying `opt` and `arg`:
#' args = sys$cmdline$parse(arg('file', 'the input file'))
#' }
#' # Explicitly provide arguments:
#' sys$cmdline$parse(arg('file', 'the input file'), 'foo.txt')
#'
#' sys$cmdline$parse(opt('v', 'verbose', 'verbose logging?', FALSE),
#'                   arg('file', 'the input file'),
#'                   c('-v', 'foo.txt'))
#' @seealso \code{opt}, \code{arg}
# TODO: Specify arguments separately as `(..., cmdline [= .sys$args])`
parse = function (...) {
    args_definition = lapply(.substitute_args(match.call()[-1],
                                              list(opt = opt, arg = arg)), eval)
    last = args_definition[[length(args_definition)]]
    if (is.character(last) || is.null(last)) {
        cmdline = last
        args_definition = args_definition[-length(args_definition)]
    }
    else
        cmdline = .sys$args

    stopifnot(length(args_definition) > 0)

    opts = Filter(function (x) inherits(x, 'sys$cmdline$opt'), args_definition)
    opts_long = setNames(opts, lapply(opts, `[[`, 'long'))
    opts_short = setNames(opts, lapply(opts, `[[`, 'short'))
    args = Filter(function (x) inherits(x, 'sys$cmdline$arg'), args_definition)
    positional = setNames(args, lapply(args, `[[`, 'name'))

    result = try(.parse(cmdline, args_definition, opts_long, opts_short,
                        positional), silent = TRUE)
    if (inherits(result, 'try-error')) {
        message = conditionMessage(attr(result, 'condition'))
        stop(.sys_error(message, args_definition))
    }
    else if (identical(result, 'help'))
        stop(.sys_help(args_definition))
    else
        result
}

#' The help message for a command line application
#'
#' \code{help} returns a formatted usage and help message created from a list
#' of options.
#' @param options list of options
#' @return Character vector containing the help message.
help = function (options) {
    arg_help = paste(sapply(options, .option_description), collapse = '\n')
    paste0(usage(options), '\n\nArguments:\n', arg_help)
}

#' \code{usage} returns a formatted usage message created from a list of
#' options.
#' @rdname help
usage = function (options) {
    cmd = paste(.sys$script_name,
                paste(sapply(options, .option_syntax), collapse = ' '))
    paste('Usage:', cmd)
}

#' Create a command line argument
#'
#' \code{opt} creates an option-style command line argument (used via
#' \code{--long_name} or \code{-s}).
#' @param short the option’s short name
#' @param long the option’s long name
#' @param description a user-readable description text
#' @param default the default value, for optional arguments (optional)
#' @param validate a validation function for the argument value (optional)
#' @param transform a transformation function for the argument value (optional)
#' @return \code{opt} returns an option-style command line argument.
#'
#' @details
#' An option-style command line argument can have either a short name or a long
#' name, or both. The user specifies this option by either writing the short
#' name, prefixed by \code{'-'} or the long name, prefixed by \code{'--'}. This
#' can be followed by an associated value. Optional options whose default value
#' is logical (\code{TRUE} or \code{FALSE}) are \emph{toggles}, and cannot be
#' followed by values. Rather, their presence in the command line toggles the
#' default value (by negating it); see \code{Examples}.
#'
#' For non-toggle options, values can be specified in one of the following ways:
#'
#' \enumerate{
#'  \item Following the option name, separated by a space (\code{-f bar},
#'      \code{--foo bar}),
#'  \item After a short option name, directly adjacent to the option name,
#'      without intervening space (\code{-fbar}),
#'  \item After a long option name, separated by an equals sign (\code{=})
#'      instead of a space (\code{--foo=bar}).
#' }
#'
#' If provided, \code{validate} must be a function taking a single character
#' argument and returning a single logical. Upon parsing the option’s value, the
#' \code{validate} function is called with the user-provided value to validate
#' it.
#'
#' If provided, \code{transform} must be a function taking a single character
#' argument. It will be called with the value provided by the user for this
#' argument (or the default value, if no user argument was provided!).
#'
#' For \code{opt}, the internal variable name to access the parsed option’s
#' value is the option’s long name, if provided. If no long name is provided
#' (because it exists as a short option only), the short option name is used
#' instead. It is the user’s responsibility to ensure that this does not
#' conflict with another option’s “long” name.
#'
#' @examples
#' # Create a toggle to enable verbose logging, and disable it by default.
#' opt('v', 'verbose', 'enable verbose logging', FALSE)
#' @rdname arguments
opt = function (short, long, description, default, validate, transform) {
    stopifnot(is.character(short) && length(short) == 1)
    stopifnot(is.character(long) && length(long) == 1)
    stopifnot(short != '' || long != '')
    stopifnot(is.character(description) && length(description) == 1)
    stopifnot(missing(default) || length(default) <= 1)

    optional = ! missing(default)
    name = if (long == '') short else long

    .expect_unary_function(validate)
    .expect_unary_function(transform)
    structure(as.list(environment()), class = 'sys$cmdline$opt')
}

#' \code{arg} creates a positional command line argument (such as a file name).
#' @param name the positional argument’s name
#' @return \code{arg} returns  positional command line argument.
#'
#' @details
#' Positional arguments are provided by the user in the order in which they are
#' passed to \code{\link{parse}}. That is, if the programmer provided three
#' positional arguments, then these are filled with the first three command line
#' arguments that are not options (start with \code{'-'} or \code{'--'}).
#'
#' For \code{arg}, the internal variable name to access the parsed argument’s
#' value is the argument’s name.
#'
#' @note Positional arguments can be optional but if an optional argument is
#' followed by a non-optional argument, the optional argument is nevertheless
#' filled first. It therefore does not make sense to declare non-optional
#' arguments after optional ones.
#'
#' In order to specify positional arguments that start with one or more dashes,
#' the user can specify the special option \code{--}: after this option, every
#' remaining argument is considered to be a positional argument, even if it
#' matches the name of an option.
#'
#' @examples
#' # Create arguments to specify the input and output filename.
#'
#' arg('input_file', 'the input filename')
#' arg('output_file', 'the output filename', default = '')
#'
#' # Alternatively, we can use proper types:
#' arg('output_file', 'the output filename', default = '<STDOUT>',
#'     transform = function (x) if (x == '<STDOUT>') stdout() else file(x, 'w'))
#' @rdname arguments
arg = function (name, description, default, validate, transform) {
    force(name)
    force(description)
    optional = ! missing(default)
    .expect_unary_function(validate)
    .expect_unary_function(transform)
    structure(as.list(environment()), class = 'sys$cmdline$arg')
}

.substitute_args = function (expr, env) {
    replace_names = function (expr) {
        if (length(expr) == 0)
            return()
        if (is.name(expr[[1]])) {
            name = as.character(expr[[1]])
            if (name %in% names(env))
                expr[[1]] = env[[name]]
        }

        expr
    }

    as.call(lapply(expr, replace_names))
}

.make_opt = function (prefix, name)
    if (name == '') NULL else paste0(prefix, name)

.option_syntax = function (option) {
    if (inherits(option, 'sys$cmdline$opt')) {
        name = .make_opt('--', option$long)
        if (is.null(name))
            name = .make_opt('-', option$short)

        if (! (option$optional && inherits(option$default, 'logical')))
            name = paste(name, toupper(option$name))
    }
    else
        name = option$name

    if (option$optional)
        sprintf('[%s]', name)
    else
        name
}

.option_description = function (option) {
    name = if (inherits(option, 'sys$cmdline$opt'))
            paste(c(.make_opt('-', option$short),
                    .make_opt('--', option$long)), collapse = ', ')
        else
            option$name

    exdent = 16
    description = option$description
    if (option$optional)
        description = paste(description,
                            sprintf('(default: %s)', deparse(option$default)))
    paste(strwrap(description,
                  width = .termwidth() - exdent,
                  exdent = exdent,
                  initial = sprintf('% 14s: ', name)),
          collapse = '\n')
}

.termwidth = function () {
    stty_size = suppressWarnings(try(system('stty size', intern = TRUE,
                                            ignore.stderr = TRUE),
                                     silent = TRUE))
    if (! inherits(stty_size, 'try-error'))
        if (is.null(attr(stty_size, 'status')))
            return(as.integer(strsplit(stty_size, ' ')[[1]][2]))

    as.integer(Sys.getenv('COLUMNS', getOption('width', 78)))
}

.sys_error = function (message, options) {
    structure(list(message = message, call = call('parse', options)),
              class = c('sys$cmdline$error', 'sys$cmdline$help', 'error', 'condition'))
}

.sys_help = function (options) {
    structure(list(call = call('parse', options)),
              class = c('sys$cmdline$help', 'error', 'condition'))
}

.parse = function (cmdline, args, opts_long, opts_short, positional) {
    check_positional_arg_valid = function ()
        if (arg_pos > length(positional)) {
            trunc = if (nchar(token) > 20)
                paste0(substr(token, 19), '…')
            else
                token
            stop(sprintf('Unexpected positional argument %s', sQuote(trunc)))
        }

    validate = function (option, value) {
        if (with(option, missing(validate)))
            TRUE
        else
            option$validate(value)
    }

    transform = function (option, value) {
        if (option$optional)
            value = methods::as(value, typeof(option$default))
        if (! with(option, missing(transform)))
            value = option$transform(value)
        value
    }

    store_result = function (option, value) {
        if (! validate(option, value))
            stop(sprintf('Value %s invalid for argument %s',
                         sQuote(value), sQuote(readable_name(option))))
        result[[option$name]] <<- transform(option, value)
    }

    readable_name = function (opt) {
        if (is.null(opt$long))
            opt$name
        else if (opt$long != '')
            paste0('--', opt$long)
        else
            paste0('-', opt$short)
    }

    DEFAULT = 0
    VALUE = 1
    TRAILING = 2
    long_option_pattern = '^--(?<name>[a-zA-Z0-9_-]+)(?<eq>=(?<value>.*))?$'
    i = 1
    state = DEFAULT
    result = list()
    arg_pos = 1
    short_opt_pos = 1

    while (i <= length(cmdline)) {
        token = cmdline[i]
        i = i + 1
        if (state == DEFAULT) {
            if (token == '--')
                state = TRAILING
            else if (token == '--help' || token == '-h')
                return('help')
            else if (grepl('^--', token)) {
                match = regexpr(long_option_pattern, token, perl = TRUE)
                if (match == -1)
                    stop(sprintf('Invalid token %s, expected long argument',
                                 sQuote(token)))
                name = .reggroup(match, token, 'name')

                option = opts_long[[name]]

                if (is.null(option))
                    stop(sprintf('Invalid option %s',
                                 sQuote(paste0('--', name))))

                if (is.logical(option$default)) {
                    if (attr(match, 'capture.length')[, 'eq'] != 0)
                        stop(sprintf('Invalid value: option %s is a toggle',
                                     sQuote(paste0('--', name))))

                    result[[option$name]] = ! option$default
                }
                else {
                    if (attr(match, 'capture.length')[, 'eq'] == 0) {
                        current_option = option
                        state = VALUE
                    }
                    else {
                        value = .reggroup(match, token, 'value')
                        store_result(option, value)
                    }
                }
            }
            else if (grepl('^-', token)) {
                name = substr(token, short_opt_pos + 1, short_opt_pos + 1)
                option = opts_short[[name]]

                if (is.null(option))
                    stop(sprintf('Invalid option %s',
                                 sQuote(paste0('-', name))))

                if (is.logical(option$default)) {
                    result[[option$name]] = ! option$default

                    if (nchar(token) > short_opt_pos + 1) {
                        # Consume next short option in current token next.
                        i = i - 1
                        short_opt_pos = short_opt_pos + 1
                    }
                    else
                        short_opt_pos = 1
                }
                else {
                    value = substr(token, short_opt_pos + 2, nchar(token))

                    if (value == '') {
                        current_option = option
                        state = VALUE
                    }
                    else
                        store_result(option, value)

                    short_opt_pos = 1
                }
            }
            else {
                check_positional_arg_valid()
                # TODO: Treat arglist
                store_result(positional[[arg_pos]], token)
                arg_pos = arg_pos + 1
            }
        }
        else if (state == VALUE) {
            store_result(current_option, token)
            state = DEFAULT
        }
        else if (state == TRAILING) {
            check_positional_arg_valid()
            # TODO: Treat arglist
            store_result(positional[[arg_pos]], token)
            arg_pos = arg_pos + 1
        }
    }

    # Set optional arguments, if not given.

    optional = Filter(function (x) x$optional, args)
    optional_names = unlist(lapply(optional, `[[`, 'name'))
    unset = is.na(match(optional_names, names(result)))
    optional_defaults = lapply(optional[unset], `[[`, 'default')
    result[optional_names[unset]] = mapply(transform,
                                           optional[unset], optional_defaults)

    # Ensure that all arguments are set.

    mandatory = Filter(function (x) ! x$optional, args)
    mandatory_names = unlist(Map(function (x) x$name, mandatory))
    unset = is.na(match(mandatory_names, names(result)))

    if (any(unset)) {
        plural = if(sum(unset) > 1) 's' else ''
        unset_options = unlist(lapply(mandatory[unset], readable_name))
        stop(sprintf('Mandatory argument%s %s not provided', plural,
                     paste(sQuote(unset_options), collapse = ', ')))
    }

    result
}

.expect_unary_function = function (f) {
    if (! missing(f))
        stopifnot(inherits(f, 'function') &&
                  length(formals(f)) > 0)
}

`print.sys$cmdline$opt` = function (x, ...) {
    if (x$optional)
        cat(sprintf("%s: [-%s|--%s] (default: %s) %s\n",
                    x$name,
                    x$short,
                    x$long,
                    deparse(x$default),
                    x$description))
    else
        cat(sprintf("%s: -%s|--%s %s\n",
                    x$name,
                    x$short,
                    x$long,
                    x$description))
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmdline$opt', `print.sys$cmdline$opt`)

`print.sys$cmdline$arg` = function (x, ...) {
    if (x$optional)
        cat(sprintf("[%s] (default: %s) %s\n",
                    x$name, deparse(x$default), x$description))
    else
        cat(sprintf("%s: %s\n", x$name, x$description))
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmdline$arg', `print.sys$cmdline$arg`)

`print.sys$cmdline$error` = function (x, ...) {
    call = conditionCall(x)
    options = lapply(call[-1][[1]], eval)
    message = paste('Error:', conditionMessage(x))
    args = list(...)
    file = if (! is.null(args$file)) args$file else ''
    cat(paste(usage(options), message, sep = '\n'), '\n', file = file)
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmdline$error', `print.sys$cmdline$error`)

`print.sys$cmdline$help` = function (x, ...) {
    call = conditionCall(x)
    options = lapply(call[-1][[1]], eval)
    args = list(...)
    file = if (! is.null(args$file)) args$file else ''
    cat(help(options), '\n', file = file)
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmdline$help', `print.sys$cmdline$help`)

.reggroup = function (match, string, group) {
    start = attr(match, 'capture.start')[, group]
    stop = attr(match, 'capture.length')[, group] + start - 1

    substr(string, start, stop)
}
