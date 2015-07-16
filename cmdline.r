.sys = modules::import('../sys')

parse = function (...) {
    args_definition = list(...)
    last = length(args_definition)
    if (is.character(args_definition[[last]])) {
        cmdline = args_definition[[last]]
        args_definition = args_definition[-last]
    }

    stopifnot(length(args_definition) > 0)

    options = Filter(function (x) inherits(x, 'sys$cmdline$opt'),
                     args_definition)
    opts_long = setNames(options, lapply(options, `[[`, 'long'))
    opts_short = setNames(options, lapply(options, `[[`, 'short'))
    args = Filter(function (x) inherits(x, 'sys$cmdline$arg'), args_definition)
    positional = setNames(args, lapply(args, `[[`, 'name'))

    result = try(.parse(cmdline, options, opts_long, opts_short, positional),
                 silent = TRUE)
    if (inherits(result, 'try-error')) {
        message = attr(result, 'condition')$message
        .sys$exit(1, paste(message, usage(options), sep = '\n\n'))
    }
    else if (identical(result, 'help'))
        .sys$exit(0, usage(options))
    else
        result
}

usage = function (options) {
    cmd_usage = paste(.sys$script_name,
                      paste(sapply(options, .option_syntax), collapse = ' '))
    arg_usage = paste(sapply(options, .option_description), collapse = ' ')
    sprintf('Usage: %s\n\n%s', cmd_usage, arg_usage)
}

.option_syntax = function (option) {
    # TODO: Should be “[--long|-s] name” or “[--long|-s]” or “name”
    # TODO: Optional values in “[…]”
    option$name
}

.option_description = function (option) {
    # TODO: Make this right
    exdent = 12
    strwrap(option$description,
            width = getOption('width') - exdent,
            exdent = exdent,
            initial = sprintf('% 10s: ', option$name))
}

.parse = function (cmdline, options, opts_long, opts_short, positional) {
    check_positional_arg_valid = function ()
        if (arg_pos > length(positional)) {
            trunc = if (nchar(token) > 20)
                paste0(substr(token, 19), '…')
            else
                token
            stop(sprintf('Unexpected positional argument %s.',
                         sQuote(trunc)))
        }

    DEFAULT = 0
    VALUE = 1
    TRAILING = 2
    long_option_pattern = '^--(?<name>[a-zA-Z0-9_-]+)(?<eq>=(?<value>.*))?$'
    i = 1
    state = DEFAULT
    result = list()
    arg_pos = 1

    while (i <= length(cmdline)) {
        token = cmdline[i]
        i = i + 1
        if (state == DEFAULT) {
            if (token == '--')
                state = TRAILING
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
                        stop(sprintf('Invalid value: option %s is a toggle.',
                                     sQuote(paste0('--', name))))

                    result[[name]] = ! option$default
                }
                else {
                    if (attr(match, 'capture.length')[, 'eq'] == 0)
                        state = VALUE
                    else {
                        value = .reggroup(match, token, 'value')
                        result[[name]] = as(value, .opttype(option))
                    }
                }
            }
            else if (grepl('^-', token)) {
                name = substr(token, 2, 2)
                # TODO: store option
                # TODO: look for value and/or other options
            }
            else {
                check_positional_arg_valid()
                # TODO: Treat arglist
                result[[positional[[arg_pos]]$name]] = token
                arg_pos = arg_pos + 1
            }
        }
        else if (state == VALUE) {
        }
        else if (state == TRAILING) {
            check_positional_arg_valid()
            # TODO: Treat arglist
            result[[positional[[arg_pos]]$name]] = token
            arg_pos = arg_pos + 1
        }
    }

    # Set optional arguments, if not given.

    optional = Filter(function (x) x$optional, options)
    optional_names = unlist(lapply(optional, `[[`, 'name'))
    unset = is.na(match(optional_names, names(result)))
    result[optional_names[unset]] = lapply(optional[unset], `[[`, 'default')

    # Ensure that all options are set.

    mandatory = Filter(function (x) ! x$optional, options)
    mandatory_names = unlist(Map(function (x) x$name, options))
    unset = is.na(match(mandatory_names, names(result)))

    if (any(unset)) {
        plural = if(sum(unset) > 1) 's' else ''
        user_name = function (opt) {
            if (is.null(opt$long))
                opt$name
            else if (opt$long != '')
                paste0('--', opt$long)
            else
                paste0('-', opt$short)
        }

        unset_options = unlist(lapply(mandatory[unset], user_name))
        stop(sprintf('Mandatory option%s %s not set.', plural,
                     paste(sQuote(unset_options), collapse = ', ')))
    }

    result
}

opt = function (short, long, description, default, validation) {
    stopifnot(is.character(short) && length(short) == 1)
    stopifnot(is.character(long) && length(long) == 1)
    stopifnot(is.character(description) && length(description) == 1)
    stopifnot(missing(default) || length(default) <= 1)

    optional = ! missing(default)
    if (optional && ! is.null(names(default))) {
        name = names(default)
        names(default) = NULL
    }
    else {
        stopifnot(long != '')
        name = long
    }

    if (missing(validation))
        validation = function (x) TRUE
    else
        stopifnot(inherits(validation, 'function') &&
                  length(formals(validation)) > 0)

    structure(as.list(environment()), class = 'sys$cmdline$opt')
}

arg = function (name, description, default) {
    force(name)
    force(description)
    optional = ! missing(default)
    structure(as.list(environment()), class = 'sys$cmdline$arg')
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

.reggroup = function (match, string, group) {
    start = attr(match, 'capture.start')[, group]
    stop = attr(match, 'capture.length')[, group] + start - 1

    substr(string, start, stop)
}

.opttype = function (option) {
    if (is.null(option$default))
        'character'
    else
        typeof(option$default)
}
