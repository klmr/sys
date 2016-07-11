library(testthat)

args = commandArgs(TRUE)
test_cases = if (length(args) > 0)
    paste(strsplit(args, ',')[[1]], collapse = '|')

test_dir(modules::module_file(), filter = test_cases)
