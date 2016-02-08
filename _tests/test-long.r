context('Long options')

test_that('command line is parsed', {
    expect_that(xc('--verbose',
                   opt('v', 'verbose', 'verbose logging', FALSE)),
                args_equal(verbose = TRUE))
})

test_that('single options work', {
    expect_that(xc(c('--optim', '2'),
                   opt('O', 'optim', 'optimization level', 1)),
                args_equal(optim = 2))
    expect_that(xc('--optim=2',
                   opt('O', 'optim', 'optimization level', 1)),
                args_equal(optim = 2))
})

test_that('multiple options work', {
    expect_that(xc(c('--verbose', '--silent'),
                   opt('v', 'verbose', 'verbose logging', FALSE),
                   opt('s', 'silent', 'silent output', FALSE)),
                args_equal(verbose = TRUE, silent = TRUE))
})

test_that('default options work', {
    expect_that(xc('--avoid-null',
                   opt('a', 'avoid-null', 'avoid NULL values', FALSE)),
                args_equal(avoid_null = TRUE))

    expect_that(xc(character(0),
                   opt('a', 'avoid-null', 'avoid NULL values', FALSE)),
                args_equal(avoid_null = FALSE))
})
