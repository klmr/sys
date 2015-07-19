.sys = modules::import('../sys')

#' Parse the command line arguments
#'
#' Parses the command line arguments provided to the application in accordance
#' with a given command line specification. If the command line arguments do not
#' conform with the specification, quit and print an error message.
#'
#' @param ... command line specification (see \link{Details})
#' @return A named list of options and their associated value. Missing command
#' line options are filled by their default value, if provided in the
#' definition. If there was an error parsing the command line arguments, this
#' function does not return and signals an error instead.
#'
#' @details
#' The arguments consist of one or more command line definition items. Each item
#' is created by a call to the functions \code{\link{opt}} and
#' \code{\link{arg}}. These do \emph{not} need to be qualified with their full
#' module name (see \link{Examples}).
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

help = function (options) {
    arg_help = paste(sapply(options, .option_description), collapse = '\n')
    paste0(usage(options), '\n\nArguments:\n', arg_help)
}

usage = function (options) {
    cmd = paste(.sys$script_name,
                paste(sapply(options, .option_syntax), collapse = ' '))
    paste('Usage:', cmd)
}

# TODO: Document
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

# TODO: Document
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
    paste(strwrap(option$description,
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
    result[optional_names[unset]] = lapply(optional[unset], `[[`, 'default')

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
