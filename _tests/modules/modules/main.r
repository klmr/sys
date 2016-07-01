time = modules::import('./time')
sys = modules::import('sys')

"Output a formatted timestamp"
VERSION = '2.0'

sys$run({
    sys$print(sys$description(), file = stderr())
    sys$print(sys$version(), file = stderr())
    sys$printf('The current time is %s', time$now())
})
