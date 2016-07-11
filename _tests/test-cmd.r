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

test_that('`parse` can be called with a variable', {
    x = cmd$arg('test', '')
    expect_that(xc('x', x), throws_error(NA))
})

test_that('`parse` can be called with fully qualified name', {
    expect_that(xc('x', cmd$arg('test', '')), throws_error(NA))
})

test_that('`parse` can be called with unqualified name', {
    expect_that(xc('x', arg('test', '')), throws_error(NA))
})
