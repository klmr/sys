context('Character string formatting')

modules::import('../format', attach = 'repr')

test_that('numbers are formatted correctly', {
    expect_that(repr(c(1, 2)), equals('1, 2'))
    expect_that(repr(c(1, 10)), equals('1, 10'))
    expect_that(repr(1.0), equals('1'))
    expect_that(repr(1.5), equals('1.5'))
    expect_that(repr(-1.5), equals('-1.5'))
})

test_that('dispatch to `format` happens appropriately', {
    time_chr = '1973-11-29 21:33:09'
    time = as.POSIXct(time_chr)
    expect_that(format(time), equals(time_chr))
    expect_that(repr(time), equals(time_chr))
})

test_that('dispatch to `print` happens appropriately', {
    model = lm(speed ~ dist, cars)
    expect_that(repr(model),
                equals(paste(capture.output(print(model)), collapse = '\n')))
})
