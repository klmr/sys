context('Variable argument number parsing')

argdef = cmd$arg('test', 'test argument', arity = 3)
optdef = cmd$opt('t', 'test', 'test option', arity = 3)

test_that('arguments can have arity >1', {
    expect_that(xc(c('foo', 'bar', 'baz'), argdef),
                args_equal(test = c('foo', 'bar', 'baz')))
    expect_that(xc('foo', argdef), shows_error('test'))
    expect_that(xc(c('a', 'b', 'c', 'd'), argdef), shows_error())
})

test_that('long options can have arity > 1', {
    expect_that(xc(c('--test', 'foo', 'bar', 'baz'), optdef),
                args_equal(test = c('foo', 'bar', 'baz')))
    expect_that(xc(c('--test=foo', 'bar', 'baz'), optdef),
                args_equal(test = c('foo', 'bar', 'baz')))
    expect_that(xc(c('--test', 'foo'), optdef), shows_error('--test'))
    expect_that(xc(c('--test', 'a', 'b', 'c', 'd'), optdef), shows_error())
})

test_that('short options can have arity > 1', {
    expect_that(xc(c('-t', 'foo', 'bar', 'baz'), optdef),
                args_equal(test = c('foo', 'bar', 'baz')))
    expect_that(xc(c('-tfoo', 'bar', 'baz'), optdef),
                args_equal(test = c('foo', 'bar', 'baz')))
    expect_that(xc(c('-t', 'foo'), optdef), shows_error('--test'))
    expect_that(xc(c('-ta', 'b', 'c', 'd'), optdef), shows_error())
})

argdef = cmd$arg('test', 'test argument', arity = '*', default = 'x')
optdef = cmd$opt('t', 'test', 'test option', arity = '*', default = 'x')

test_that('arguments can have arbitrary arity', {
    expect_that(xc(character(0), argdef), args_equal(test = 'x'))
    expect_that(xc('foo', argdef), args_equal(test = 'foo'))
    expect_that(xc(c('foo', 'bar'), argdef), args_equal(test = c('foo', 'bar')))
})

test_that('options can have arbitrary arity', {
    expect_that(xc('--test', optdef), args_equal(test = 'x'))
    expect_that(xc('-t', optdef), args_equal(test = 'x'))

    expect_that(xc('--test=foo', optdef), args_equal(test = 'foo'))
    expect_that(xc(c('--test', 'foo'), optdef), args_equal(test = 'foo'))

    expect_that(xc('-tfoo', optdef), args_equal(test = 'foo'))
    expect_that(xc(c('-t', 'foo'), optdef), args_equal(test = 'foo'))

    expect_that(xc(c('--test', ',foo', 'bar'), optdef),
                args_equal(test = c('foo', 'bar')))
    expect_that(xc(c('-tfoo', 'bar'), optdef),
                args_equal(test = c('foo', 'bar')))
})

test_that('variable-length options can contain dashes', {
    test_that(xc(c('--foo', 'bar', '--baz'),
                 opt('f', 'foo', 'option foo', arity = '*')),
              args_equal(foo = c('bar', '--baz')))
})

test_that('variable-length option parsing gives preference to option names', {
    test_that(xc(c('--foo', '--bar', '--baz'),
                 opt('f', 'foo', 'option foo', arity = '*'),
                 opt('b', 'baz', 'option baz', default = FALSE)),
              args_equal(foo = '--bar', baz = TRUE))

    test_that(xc(c('--foo', '--bar'),
                 opt('f', 'foo', 'option foo', arity = '*', default = 'x'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = 'x', bar = TRUE))

    test_that(xc(c('--foo', 'bar', '-b'),
                 opt('f', 'foo', 'option foo', arity = '*'),
                 opt('b', 'baz', 'option baz', default = FALSE)),
              args_equal(foo = 'bar', baz = TRUE))

    test_that(xc(c('--foo', '-b'),
                 opt('f', 'foo', 'option foo', arity = '*', default = 'x'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = 'x', bar = TRUE))

    # But:

    test_that(xc('--foo=--bar',
                 opt('f', 'foo', 'option foo', arity = '*', default = 'x'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = '--bar', bar = FALSE))

    test_that(xc('-f--bar',
                 opt('f', 'foo', 'option foo', arity = '*', default = 'x'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = '--bar', bar = FALSE))

    test_that(xc(c('--foo', '--bar'),
                 opt('f', 'foo', 'option foo', arity = '*'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = '--bar', bar = FALSE))

    test_that(xc(c('-f', '--bar'),
                 opt('f', 'foo', 'option foo', arity = '*'),
                 opt('b', 'bar', 'option bar', default = FALSE)),
              args_equal(foo = '--bar', bar = FALSE))
})

argdef = cmd$arg('test', 'test argument', arity = '*')
optdef = cmd$opt('t', 'test', 'test option', arity = '*')

test_that('arguments can have arbitrary arity > 0', {
    test_that(xc('foo', argdef), args_equal(test = 'foo'))
    expect_that(xc(character(0), argdef), shows_error('test'))
})

test_that('options can have arbitrary arity > 0', {
    test_that(xc('--test=foo', argdef), args_equal(test = 'foo'))
    expect_that(xc('--test', argdef), shows_error('--test'))

    test_that(xc('-tfoo', argdef), args_equal(test = 'foo'))
    expect_that(xc('-t', argdef), shows_error('--test'))
})
