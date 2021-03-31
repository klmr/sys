box::use(./time)
box::use(../..)

"Output a formatted timestamp"
VERSION = '2.0'

sys$run({
    sys$print(sys$description(), file = stderr())
    sys$printf('Version %s', sys$version(), file = stderr())
    sys$printf('The current time is %s', time$now())
})
