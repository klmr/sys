.sys = modules::import('.')

#' Parse the command line arguments
#'
#' Parses the command line arguments provided to the application in accordance
#' with a given command line specification. If the command line arguments do not
#' conform with the specification, quit and print an error message.
#'
#' @param ... command line specification (see \code{Details})
#' @param args character vector of command line arguments to parse (optional);
#'  defaults to \code{sys$args}
#' @return A named list of options and their associated value. Missing command
#' line options are filled by their default value, if provided in the
#' definition. If there was an error parsing the command line arguments, this
#' function signals an error via \code{\link{stop}} instead.
#'
#' @details
#' The arguments consist of one or more command line definition items. Each item
#' is created by a call to the functions \code{\link{opt}} and
#' \code{\link{arg}}. These do \emph{not} need to be qualified with their full
#' module name (see \code{Examples}).
#'
#' Optionally, \code{args} can be a character vector specifying the command line
#' arguments. If this is not given, then \code{sys$args} is used instead. In
#' normal usage, this argument is omitted.
#'
#' @examples
#' sys = import('sys')
#' \dontrun{
#' # Use command line provided by `sys$args`
#' args = sys$cmd$parse(sys$cmd$arg('file', 'the input file'))
#'
#' # Or, equivalently, without qualifying `opt` and `arg`:
#' args = sys$cmd$parse(arg('file', 'the input file'))
#' }
#' # Explicitly provide arguments:
#' sys$cmd$parse(arg('file', 'the input file'), args = 'foo.txt')
#'
#' sys$cmd$parse(opt('v', 'verbose', 'verbose logging?', FALSE),
#'               arg('file', 'the input file'),
#'               args = c('-v', 'foo.txt'))
#' @seealso \code{opt}, \code{arg}
parse = function (..., args) {
    if (missing(args))
        args = .sys$args
    args_def_list = .substitute_args(match.call(expand.dots = FALSE)$...,
                                     list(opt = opt, arg = arg))
    args_definition = lapply(args_def_list, eval, envir = parent.frame())

    stopifnot(length(args_definition) > 0)
    stopifnot(all(sapply(args_definition, inherits, 'sys$cmd$token')))

    opts = Filter(function (x) inherits(x, 'sys$cmd$opt'), args_definition)
    opts_long = setNames(opts, lapply(opts, `[[`, 'long'))
    opts_short = setNames(opts, lapply(opts, `[[`, 'short'))
    positional = Filter(function (x) inherits(x, 'sys$cmd$arg'),
                        args_definition)
    positional = setNames(positional, lapply(positional, `[[`, 'name'))

    # Ensure there are no argument name duplicates, even after converting
    # argument names into valid R identifiers.

    r_names = gsub('-', '_', unlist(lapply(args_definition, `[[`, 'name')))
    if (any(duplicated(r_names)))
        stop('Command line definition contains duplicate names', call. = FALSE)

    # Parse and validate arguments.

    result = try(.parse(args, args_definition, opts_long, opts_short,
                        positional), silent = TRUE)
    if (inherits(result, 'try-error')) {
        message = conditionMessage(attr(result, 'condition'))
        stop(.sys_error(message, args_definition))
    }
    else if (identical(result, 'help'))
        stop(.sys_help(args_definition))
    else if (identical(result, 'version'))
        stop(.sys_version())
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
    is = function (cls) function (x) inherits(x, cls)
    args = Filter(is('sys$cmd$arg'), options)
    opts = Filter(is('sys$cmd$opt'), options)
    arg_help = paste(sapply(args, .option_description), collapse = '\n')
    opt_help = paste(sapply(opts, .option_description), collapse = '\n')

    description = .sys$description()
    version = sprintf(' (version %s)', .sys$version())
    if (length(description) > 0)
        description = c(strwrap(paste0(description, version), .termwidth()), '')

    opt_section = function (title, items)
        if (nzchar(items)) c('', title, items) else NULL

    paste(c(description, usage(options),
            opt_section('Positional arguments:', arg_help),
            opt_section('Options:', opt_help)), collapse = '\n')
}

#' \code{usage} returns a formatted usage message created from a list of
#' options.
#' @rdname help
usage = function (options) {
    cmd = paste(.sys$script_name,
                paste(sapply(options, .option_syntax), collapse = ' '))
    usage = strwrap(cmd, .termwidth(), prefix = '       ', initial = 'Usage: ')
    paste(usage, collapse = '\n')
}

#' \code{version} returns the version string, if provided; otherwise \code{""}.
#' @rdname help
version = function ()
    .sys$version()

#' Create a command line argument
#'
#' \code{opt} creates an option-style command line argument (used via
#' \code{--long_name} or \code{-s}).
#' @param short the option’s short name, or \code{''}
#' @param long the option’s long name, or \code{''}
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
opt = function (short, long, description, default, arity = 1, validate, transform) {
    stopifnot(is.character(short) && length(short) == 1)
    stopifnot(is.character(long) && length(long) == 1)
    stopifnot(short != '' || long != '')
    stopifnot(short == '' || nchar(short) == 1)
    stopifnot(is.character(description) && length(description) == 1)
    stopifnot(missing(default) || length(default) <= 1)
    stopifnot(length(arity) == 1,
              is.numeric(arity) && arity > 0 || arity == '*')

    if (! missing(default) && is.logical(default) && arity != 1)
        stop('an option flag cannot have arity ≠ 1')

    optional = ! missing(default)
    name = if (long == '') short else long

    .expect_unary_function(validate)
    .expect_unary_function(transform)
    structure(as.list(environment()),
              class = c('sys$cmd$opt', 'sys$cmd$token'))
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
arg = function (name, description, default, arity = 1, validate, transform) {
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(is.character(description) && length(description) == 1)
    stopifnot(length(arity) == 1,
              is.numeric(arity) && arity > 0 || arity == '*')
    optional = ! missing(default)
    .expect_unary_function(validate)
    .expect_unary_function(transform)
    structure(as.list(environment()),
              class = c('sys$cmd$arg', 'sys$cmd$token'))
}

.substitute_args = function (expr, env) {
    replace_names = function (expr) {
        if (! is.name(expr) && is.name(expr[[1]])) {
            name = as.character(expr[[1]])
            if (name %in% names(env))
                expr[[1]] = env[[name]]
        }

        expr
    }

    lapply(expr, replace_names)
}

.make_opt = function (prefix, name)
    if (name == '') NULL else paste0(prefix, name)

.option_usage = function (option)
    UseMethod('.option_usage')

`.option_usage.sys$cmd$opt` = function (option) {
    usage = .make_opt('--', option$long)
    use_short = is.null(usage)
    if (use_short)
        usage = .make_opt('-', option$short)

    if (! (option$optional && inherits(option$default, 'logical')))
        paste(usage, toupper(option$name), sep = if (use_short) ' ' else '=')
    else
        usage
}

`.option_usage.sys$cmd$arg` = function (option)
    option$name

.option_syntax = function (option) {
    usage = .option_usage(option)

    if (option$optional)
        paste0('[', usage, ']')
    else
        usage
}

.option_name = function (option)
    UseMethod('.option_name')

`.option_name.sys$cmd$opt` = function (option) {
    nonnull = function (x) if (is.null(x)) '' else x
    short_opt_name = nonnull(.make_opt('-', option$short))
    long_opt_name = nonnull(.make_opt('--', option$long))

    sep = if(nzchar(short_opt_name) && nzchar(long_opt_name)) ', ' else '  '
    sprintf('% 4s%s%s', short_opt_name, sep, long_opt_name)
}

`.option_name.sys$cmd$arg` = function (option)
    sprintf('  %s', option$name)

.option_description = function (option) {
    name = .option_name(option)
    description = option$description
    if (option$optional)
        description = paste(description,
                            sprintf('(default: %s)', deparse(option$default)))
    paste(strwrap(description,
                  width = .termwidth(),
                  exdent = 21,
                  initial = sprintf('% -20s ', name)),
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
              class = c('sys$cmd$error', 'sys$cmd$help', 'error', 'condition'))
}

.sys_help = function (options) {
    structure(list(message = 'help', call = call('parse', options)),
              class = c('sys$cmd$help', 'error', 'condition'))
}

.sys_version = function () {
    structure(list(message = 'version', call = call('parse', options)),
              class = c('sys$cmd$version', 'sys$cmd$help', 'error', 'condition'))
}

.parse = function (args, options, opts_long, opts_short, positional) {
    check_positional_arg_valid = function ()
        if (arg_pos > length(positional)) {
            trunc = if (nchar(token) > 20)
                paste0(strtrim(token, 19), '…')
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
        result[[option$name]] <<- append(result[[option$name]],
                                         transform(option, value))
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
    expected_args = 1

    while (i <= length(args)) {
        token = args[i]
        i = i + 1
        if (state == DEFAULT) {
            if (token == '--') {
                state = TRAILING
            } else if (token == '--help' || token == '-h') {
                return('help')
            } else if (token == '--version') {
                return('version')
            } else if (grepl('^--', token)) {
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
                } else {
                    if (attr(match, 'capture.length')[, 'eq'] == 0) {
                        current_option = option
                        expected_args = option$arity
                        state = VALUE
                    } else {
                        value = .reggroup(match, token, 'value')
                        store_result(option, value)
                        if ((expected_args = option$arity - 1) != 0) {
                            current_option = option
                            state = VALUE
                        }
                    }
                }
            } else if (grepl('^-', token)) {
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
                    } else
                        short_opt_pos = 1
                } else {
                    value = substr(token, short_opt_pos + 2, nchar(token))

                    if (value == '') {
                        current_option = option
                        expected_args = option$arity
                        state = VALUE
                    } else {
                        store_result(option, value)
                        if ((expected_args = option$arity - 1) != 0) {
                            current_option = option
                            state = VALUE
                        }
                    }

                    short_opt_pos = 1
                }
            } else {
                check_positional_arg_valid()
                store_result(positional[[arg_pos]], token)
                arg_pos = arg_pos + 1
            }
        } else if (state == VALUE) {
            store_result(current_option, token)
            if ((expected_args = expected_args - 1) == 0)
                state = DEFAULT
        } else if (state == TRAILING) {
            check_positional_arg_valid()
            store_result(positional[[arg_pos]], token)
            arg_pos = arg_pos + 1
        }
    }

    if (expected_args != 0)
        stop(sprintf('insufficient values for %s',
                     sQuote(readable_name(current_option))))

    # Set optional arguments, if not given.

    optional = Filter(function (x) x$optional, options)
    optional_names = unlist(lapply(optional, `[[`, 'name'))
    unset = is.na(match(optional_names, names(result)))
    optional_defaults = lapply(optional[unset], `[[`, 'default')
    result[optional_names[unset]] = mapply(transform,
                                           optional[unset], optional_defaults)

    # Ensure that all arguments are set.

    mandatory = Filter(function (x) ! x$optional, options)
    mandatory_names = unlist(Map(function (x) x$name, mandatory))
    unset = is.na(match(mandatory_names, names(result)))

    if (any(unset)) {
        plural = if(sum(unset) > 1) 's' else ''
        unset_options = unlist(lapply(mandatory[unset], readable_name))
        stop(sprintf('Mandatory argument%s %s not provided', plural,
                     paste(sQuote(unset_options), collapse = ', ')))
    }

    # Make all option names into valid R names.

    names(result) = gsub('-', '_', names(result))

    result
}

.expect_unary_function = function (f) {
    if (! missing(f))
        stopifnot(inherits(f, 'function') &&
                  length(formals(f)) > 0)
}

`print.sys$cmd$opt` = function (x, ...) {
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
modules::register_S3_method('print', 'sys$cmd$opt', `print.sys$cmd$opt`)

`print.sys$cmd$arg` = function (x, ...) {
    if (x$optional)
        cat(sprintf("[%s] (default: %s) %s\n",
                    x$name, deparse(x$default), x$description))
    else
        cat(sprintf("%s: %s\n", x$name, x$description))
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmd$arg', `print.sys$cmd$arg`)

`print.sys$cmd$error` = function (x, ...) {
    call = conditionCall(x)
    options = lapply(call[-1][[1]], eval)
    message = paste('Error:', conditionMessage(x))
    args = list(...)
    file = if (! is.null(args$file)) args$file else ''
    cat(paste(usage(options), message, sep = '\n'), '\n', file = file)
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmd$error', `print.sys$cmd$error`)

`print.sys$cmd$help` = function (x, ...) {
    call = conditionCall(x)
    options = lapply(call[-1][[1]], eval)
    args = list(...)
    file = if (! is.null(args$file)) args$file else ''
    cat(help(options), '\n', file = file)
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmd$help', `print.sys$cmd$help`)

`print.sys$cmd$version` = function (x, ...) {
    args = list(...)
    file = if (! is.null(args$file)) args$file else ''
    cat(version(), '\n', file = file)
    invisible(x)
}
modules::register_S3_method('print', 'sys$cmd$version', `print.sys$cmd$version`)

.reggroup = function (match, string, group) {
    start = attr(match, 'capture.start')[, group]
    stop = attr(match, 'capture.length')[, group] + start - 1

    substr(string, start, stop)
}
