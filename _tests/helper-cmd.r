cmd = modules::import('../cmdline')

shows_help = function ()
    function (x)
        expectation(inherits(x, 'try-error') &&
                    inherits(attr(x, 'condition'), 'sys$cmdline$help') &&
                    ! inherits(attr(x, 'condition'), 'sys$cmdline$error'),
                    'does not show help', 'shows help')

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

construct_parse_call = function (call) {
    call$args = call$cmdline
    call$cmdline = NULL
    call[[1]] = call('$', quote(cmd), quote(parse))
    call
}

xc = function (cmdline, ...) {
    call = construct_parse_call(match.call(expand.dots = TRUE))
    try(eval.parent(call), silent = TRUE)
}
