context('sys import')

test_that('sys$run is run in script', {
    expect_that(run_rscript('time'), matches_lines('Output a raw timestamp',
                                                   'Version 1.0'))
})

test_that('sys$run runs only top level module', {
    main = run_rscript('main')
    expect_that(main, matches_lines('Output a formatted timestamp',
                                    'Version 2.0'))
    expect_that(main, matches_no_lines('Output a raw timestamp',
                                       'Version 1.0'))
})
