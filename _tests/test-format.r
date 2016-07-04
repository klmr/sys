context('Character string formatting')

modules::import('../format', attach = 'repr')

test_that('numbers are formatted correctly', {
    expect_that(repr(c(1, 2)), equals('1, 2'))
    expect_that(repr(c(1, 10)), equals('1, 10'))
    expect_that(repr(c(1L, 10L)), equals('1, 10'))
    expect_that(repr(1 : 3), equals('1, 2, 3'))
    expect_that(repr(seq(1, 2, 0.5)), equals('1.0, 1.5, 2.0'))
    expect_that(repr(1.0), equals('1'))
    expect_that(repr(1.5), equals('1.5'))
    expect_that(repr(-1.5), equals('-1.5'))
})

test_that('simple character strings are formatted correctly', {
    expect_that(repr('foo'), equals('foo'))
})

test_that('Escape sequences are formatted correctly', {
    expect_that(repr('"hello"'), equals('"hello"'))
    expect_that(repr("'hello'"), equals("'hello'"))
    expect_that(repr('foo\n\tbar'), equals('foo\n\tbar'))
})

test_that('Unicode characters are formatted correctly', {
    uchr = '\U00DF\U00E8\U00E5\U2603'
    expect_that(repr(uchr), equals(uchr))
})

test_that('logical values are formatted correctly', {
    expect_that(repr(c(TRUE, FALSE)), equals('TRUE, FALSE'))
})

test_that('dispatch to `format` happens appropriately', {
    time_chr = '1973-11-29 21:33:09'
    time = as.POSIXct(time_chr)
    expect_that(format(time), equals(time_chr))
    expect_that(repr(time), equals(time_chr))
})

test_that('dispatch to `print` happens appropriately', {
    model = lm(speed ~ dist, cars)
    expected = paste(capture.output(model), collapse = '\n')
    expect_that(repr(model), equals(expected))
})
