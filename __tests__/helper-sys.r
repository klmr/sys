run_rscript = function (name) {
    script_path = sprintf('modules/%s.r', name)
    system2('Rscript', script_path, stdout = TRUE, stderr = TRUE)
}

matches_lines = function (...) {
    expected = list(...)
    function (actual) {
        matches = match(expected, actual)
        missing = expected[is.na(matches)]
        expect(! any (is.na(matches)),
               sprintf('Missing line%s %s in output',
                       if (length(missing) > 1) 's' else '',
                       paste(sQuote(missing), collapse = ', ')))
    }
}

matches_no_lines = function (...) {
    expected = list(...)
    function (actual) {
        matches = match(expected, actual)
        hits = expected[! is.na(matches)]
        expect(length(hits) == 0,
               sprintf('Output contains invalid line%s %s',
                       if (length(hits) > 1) 's' else '',
                       paste(sQuote(hits), collapse = ', ')))
    }
}
