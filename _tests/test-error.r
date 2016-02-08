context('Error messages')

test_that('wrong arguments cause errors', {
    expect_that(xc(character(0), arg('filename', 'the input file')),
                shows_error('filename'))

    expect_that(xc('foo', arg('filename', 'the input file')),
                not(shows_error('filename')))

    expect_that(xc('--extra', arg('filename', 'the input file')),
                shows_error('--extra'))

    expect_that(xc('some.file', opt('x', 'extra', 'an extra argument')),
                shows_error('some.file'))

    expect_that(xc('some.file',
                   opt('x', 'extra', 'an extra argument'),
                   arg('filename', 'the input file', '')),
                shows_error('--extra'))

    expect_that(xc('-vvv',
                   opt('v', '', 'level of verbose logging', '',
                       validate = function (value) value %in% c('', 'v'))),
                shows_error('-v'))
})

test_that('wrong usage causes errors', {
    expect_that(xc(character(0),
                   arg('test-this', 'first argument', ''),
                   arg('test_this', 'duplicate argument', '')),
                throws_error('Command line definition contains duplicate names'))

    expect_that(xc('x', opt('foo', 'f', 'frobnicate')),
                throws_error('short == "" || nchar(short) == 1'))
})
