sys = modules::import('sys')

"Output a raw timestamp"
VERSION = '1.0'

now = function () Sys.time()

sys$run({
    sys$print(sys$description(), file = stderr())
    sys$printf('Version %s', sys$version(), file = stderr())
    sys$print(unclass(now()))
})
