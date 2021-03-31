context('Help')

test_that('help messages are correctly displayed', {
    expect_that(xc('-h', arg('filename', 'the input file')),
                shows_help())

    expect_that(xc('foo', arg('filename', 'the input file')),
                shows_no_help())

    expect_that(xc('--help', opt('x', 'extra', 'an extra argument', FALSE)),
                shows_help())

    expect_that(xc(character(), arg('filename', 'the input file')),
                shows_no_help())
})
