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
