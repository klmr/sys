context('Short options')

test_that('single options work', {
    expect_that(xc('-O2', opt('O', '', 'optimization level', 1)),
                args_equal(O = 2))

    expect_that(xc(c('-u', 'username'), opt('u', 'user', 'the username')),
                args_equal(user = 'username'))

    # special case: we only allow “-v”, “-vv”, “-vvv” — needs to be validated separately.
    expect_that(xc('-vvv',
                   opt('v', '', 'level of verbose logging', '',
                       validate = function (value) value %in% c('', 'v', 'vv'),
                       transform = nchar)),
                args_equal(v = 2L))

    expect_that(xc(NULL,
                   opt('v', '', 'level of verbose logging', '',
                       validate = function (value) value %in% c('', 'v', 'vv'),
                       transform = nchar)),
                args_equal(v = 0L))
})

test_that('multiple options work', {
    # The following doesn’t make sense but hey.
    expect_that(xc('-vs',
                   opt('v', 'verbose', 'verbose logging', FALSE),
                   opt('s', 'silent', 'silent output', FALSE)),
                args_equal(verbose = TRUE, silent = TRUE))
})
