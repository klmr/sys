context('Mixture of options and arguments with different arity')

arg1 = cmd$arg('test', 'test argument', default = 'x')
opt1 = cmd$opt('a', 'opta', 'test option', arity = 1, default = 'x')

test_that("arguments can be combined with options", {
     expect_that(xc(c('-a', 'test', 'arg1'), arg1, opt1),
                args_equal(test = 'arg1', opta = 'test'))

    expect_that(xc(c('-a', 'test', 'arg1'), opt1, arg1),
                args_equal(test = 'arg1', opta = 'test'))

    expect_that(xc(c('-a', 'test', 'arg1', 'arg2'), opt1, arg1),
                shows_error('arg2'))
})

arg2 = cmd$arg('test', 'test argument', arity = '*', default = 'x')
opt2 = cmd$opt('b', 'optb', 'test option', default = 'x')

test_that("arity is respected when combinding options", {
    expect_that(xc(c('-a', 'test', 'arg1', 'arg2'), arg2, opt1),
                args_equal(opta = 'test', test = c('arg1', 'arg2')))

    expect_that(xc(c('-a', 'test', 'arg1', 'arg2'), opt1, arg2),
                args_equal(opta = 'test', test = c('arg1', 'arg2')))

    expect_that(xc(c('-a', 'test', '-bb', 'arg1', 'arg2'), opt1, opt2, arg2),
                args_equal(opta = 'test', optb = 'b', test = c('arg1', 'arg2')))
})
