context('Command line parsing')

test_that('command line is parsed', {
    expect_that(xc('-v', opt('v', 'verbose', 'verbose logging', FALSE)),
                args_equal(verbose = TRUE))
})

test_that('positional arguments with dash prefix work', {
    expect_that(xc('-foo', arg('filename', '')),
                shows_error('-f'))

    expect_that(xc('--foo', arg('filename', '')),
                shows_error('--foo'))

    expect_that(xc(c('--', '-foo'), arg('filename', '')),
                args_equal(filename = '-foo'))

    expect_that(xc(c('--', '--foo'), arg('filename', '')),
                args_equal(filename = '--foo'))
})
